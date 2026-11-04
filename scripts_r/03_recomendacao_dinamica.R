# ============================================================================
# SCRIPT 3: RECOMENDAÇÃO DINÂMICA E SENSIBILIDADE MULTICRITÉRIO (AHP)
# Sincronizado com a Tabela 1 e Seção 4.2 do TCC
# ============================================================================

library(tidyverse)
library(ggplot2)

# ============================================================================
# 1. MATRIZ DE ESCORES PARCIAIS E TOTAIS (TABELA 1 DO TCC)
# ============================================================================

scores_ahp <- data.frame(
  estrutura = c(
    "Trusts Fiduciários",
    "Estruturas Corporativas (LLC/IBC)",
    "Seguros Unit-Linked",
    "Fundos Exclusivos",
    "Fundos Multimercado",
    "Contas Diretas"
  ),
  nota_sucessorio  = c(9.2, 8.0, 8.8, 8.5, 7.0, 4.5),
  nota_tributario  = c(8.7, 8.8, 8.5, 8.0, 7.5, 6.0),
  nota_operacional = c(6.5, 7.8, 6.8, 5.5, 9.0, 9.5),
  nota_regulatorio = c(7.5, 8.5, 7.8, 7.2, 9.2, 8.0)
) %>%
  mutate(
    # Pesos Base: 40% Sucessório, 35% Tributário, 15% Operacional, 10% Regulatório
    score_ahp  = (nota_sucessorio * 0.40) + (nota_tributario * 0.35) + (nota_operacional * 0.15) + (nota_regulatorio * 0.10),
    # Cenário 1: Ênfase Tributária (25%, 50%, 15%, 10%)
    cenario_1_trib = (nota_sucessorio * 0.25) + (nota_tributario * 0.50) + (nota_operacional * 0.15) + (nota_regulatorio * 0.10),
    # Cenário 2: Ênfase Sucessória (55%, 20%, 15%, 10%)
    cenario_2_suc  = (nota_sucessorio * 0.55) + (nota_tributario * 0.20) + (nota_operacional * 0.15) + (nota_regulatorio * 0.10),
    # Cenário 3: Ênfase Operacional (20%, 25%, 45%, 10%)
    cenario_3_oper = (nota_sucessorio * 0.20) + (nota_tributario * 0.25) + (nota_operacional * 0.45) + (nota_regulatorio * 0.10)
  )

cat("\n=== CONFERÊNCIA ARITMÉTICA: TABELA 1 E CENÁRIOS DE SENSIBILIDADE (SEÇÃO 4.2) ===\n")
print(scores_ahp %>% select(estrutura, score_ahp, cenario_1_trib, cenario_2_suc, cenario_3_oper))

# ============================================================================
# 2. CRIAR SCORE FISCAL DINÂMICO E COMBINADO
# ============================================================================

score_fiscal <- analise_fiscal %>%
  select(estrutura, custo_fiscal_total) %>%
  mutate(
    score_fiscal = 10 - ((custo_fiscal_total - min(custo_fiscal_total)) / 
                           (max(custo_fiscal_total) - min(custo_fiscal_total)) * 10)
  ) %>%
  select(estrutura, score_fiscal)

scores_combinados <- scores_ahp %>%
  select(estrutura, score_ahp) %>%
  left_join(score_fiscal, by = "estrutura") %>%
  mutate(
    score_final = (score_ahp * 0.70) + (score_fiscal * 0.30)
  ) %>%
  arrange(desc(score_final))

cat("\n=== SCORES COMBINADOS (AHP + FISCAL) ===\n")
print(scores_combinados)

# ============================================================================
# 3. CLASSIFICAÇÃO E RECOMENDAÇÃO POR ANO
# ============================================================================

classificar_faixa <- function(patrimonio) {
  if (patrimonio <= 1000000) {
    return("Até R$ 1M")
  } else if (patrimonio <= 10000000) {
    return("R$ 1-10M")
  } else if (patrimonio <= 20000000) {
    return("R$ 10-20M")
  } else {
    return("Acima de R$ 20M")
  }
}

patrimonio_classificado <- resultados_completos %>%
  filter(ano > 0, semestre %% 2 == 0) %>%
  select(ano, estrutura, patrimonio_liquido) %>%
  mutate(faixa_patrimonio = sapply(patrimonio_liquido, classificar_faixa))

resumo_recomendacao <- patrimonio_classificado %>%
  left_join(scores_combinados %>% select(estrutura, score_final), by = "estrutura") %>%
  group_by(ano, faixa_patrimonio) %>%
  slice_max(score_final, n = 1) %>%
  ungroup() %>%
  select(ano, faixa_patrimonio, estrutura, patrimonio_liquido, score_final) %>%
  arrange(ano) %>%
  mutate(
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", nsmall = 0),
    score_final = round(score_final, 2)
  )

# ============================================================================
# 4. GRÁFICOS DE RECOMENDAÇÃO E SCORES
# ============================================================================

dados_grafico <- resultados_completos %>%
  filter(ano > 0, semestre %% 2 == 0) %>%
  select(ano, estrutura, patrimonio_liquido) %>%
  left_join(scores_combinados %>% select(estrutura, score_final), by = "estrutura")

estrutura_recomendada_por_ano <- dados_grafico %>%
  group_by(ano) %>%
  slice_max(score_final, n = 1) %>%
  ungroup() %>%
  select(ano, estrutura) %>%
  mutate(recomendada = TRUE)

dados_grafico <- dados_grafico %>%
  left_join(estrutura_recomendada_por_ano, by = c("ano", "estrutura")) %>%
  mutate(recomendada = replace_na(recomendada, FALSE))

grafico_evolucao_recomendacao <- dados_grafico %>%
  ggplot(aes(x = ano, y = patrimonio_liquido / 1000000, 
             color = estrutura, linewidth = recomendada)) +
  geom_line() +
  geom_point(size = 2) +
  scale_linewidth_manual(values = c("FALSE" = 0.9, "TRUE" = 2.2), guide = "none") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M", decimal.mark = ",")) +
  labs(
    title = "Evolução do Patrimônio e Estrutura Recomendada (5 Anos)",
    subtitle = "Linha em destaque indica a estrutura com maior score combinado",
    x = "Ano",
    y = "Patrimônio Líquido (R$ Milhões)",
    color = "Estrutura"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(size = 14, face = "bold"))

grafico_scores <- scores_combinados %>%
  ggplot(aes(x = reorder(estrutura, -score_final), y = score_final, fill = estrutura)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(score_final, 2)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(
    title = "Score Final de Recomendação por Estrutura",
    subtitle = "Combinação de AHP (70%) + Eficiência Fiscal (30%)",
    x = "Estrutura de Investimento",
    y = "Score (0-10)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), plot.title = element_text(size = 14, face = "bold"))