# ============================================================================
# SCRIPT 3: RECOMENDAÇÃO DINÂMICA - QUAL ESTRUTURA É ÓTIMA A CADA ANO?
# Análise de como a recomendação muda conforme o patrimônio evolui
# TCC: Análise Comparativa de Estruturas de Investimento Offshore
# ============================================================================

# Carregar bibliotecas necessárias
library(tidyverse)
library(ggplot2)

# ============================================================================
# 1. USAR RESULTADOS DO SCRIPT 1
# ============================================================================

# Usar os dados de 'resultados_completos' do Script 1
# Se não tiver, execute o Script 1 primeiro

# ============================================================================
# 2. CRIAR MATRIZ DE SCORES AHP POR ESTRUTURA (DO TCC)
# ============================================================================

# Scores consolidados do TCC (Tabela 1 do seu trabalho)
scores_ahp <- data.frame(
  estrutura = c(
    "Contas Diretas",
    "Fundos Exclusivos",
    "Fundos Multimercado",
    "Estruturas Corporativas (LLC/IBC)",
    "Seguros Unit-Linked",
    "Trusts Fiduciários"
  ),
  score_ahp = c(7.210, 7.850, 7.695, 8.300, 8.295, 8.450)
)

cat("\n=== SCORES AHP (DO TCC) ===\n")
print(scores_ahp)

# ============================================================================
# 3. CRIAR SCORE FISCAL DINÂMICO (baseado em dados do Script 2)
# ============================================================================

# Calcular score fiscal para cada estrutura
# Quanto MENOR o custo fiscal, MAIOR o score (0-10)
score_fiscal <- analise_fiscal %>%
  select(estrutura, custo_fiscal_total) %>%
  mutate(
    # Normalizar custo fiscal para escala 0-10 (inversa)
    # Estrutura com menor custo = score 10
    # Estrutura com maior custo = score 0
    score_fiscal = 10 - ((custo_fiscal_total - min(custo_fiscal_total)) / 
                           (max(custo_fiscal_total) - min(custo_fiscal_total)) * 10)
  ) %>%
  select(estrutura, score_fiscal)

# Combinar scores AHP + score fiscal
scores_combinados <- scores_ahp %>%
  left_join(score_fiscal, by = "estrutura") %>%
  mutate(
    # Score final = 70% AHP + 30% Fiscal
    # (AHP já considera sucessório, tributário, operacional, regulatório)
    # Fiscal adiciona análise de custo real ao longo do tempo
    score_final = (score_ahp * 0.70) + (score_fiscal * 0.30)
  ) %>%
  arrange(desc(score_final))

cat("\n=== SCORES COMBINADOS (AHP + FISCAL) ===\n")
print(scores_combinados)

# ============================================================================
# 4. RECOMENDAÇÃO DINÂMICA: QUAL ESTRUTURA A CADA ANO?
# ============================================================================

# Criar tabela mostrando patrimônio por ano e estrutura
patrimonio_por_ano <- resultados_completos %>%
  filter(ano > 0) %>%
  select(ano, estrutura, patrimonio_liquido) %>%
  pivot_wider(
    names_from = estrutura,
    values_from = patrimonio_liquido
  )

cat("\n=== PATRIMÔNIO POR ANO E ESTRUTURA ===\n")
print(patrimonio_por_ano)

# ============================================================================
# 5. CLASSIFICAR FAIXAS DE PATRIMÔNIO (DO TCC)
# ============================================================================

# Faixas de patrimônio conforme seção 4.2 do TCC
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

# Aplicar classificação
patrimonio_classificado <- resultados_completos %>%
  filter(ano > 0) %>%
  select(ano, estrutura, patrimonio_liquido) %>%
  mutate(
    faixa_patrimonio = sapply(patrimonio_liquido, classificar_faixa)
  )

# ============================================================================
# 6. RECOMENDAÇÃO FINAL POR ANO (CORRIGIDO: cross_join em vez de by = character())
# ============================================================================

# Para cada ano, determinar qual estrutura é recomendada
recomendacao_por_ano <- patrimonio_classificado %>%
  select(ano, faixa_patrimonio) %>%
  distinct() %>%
  cross_join(scores_combinados %>% select(estrutura, score_final)) %>%  # CORRIGIDO
  group_by(ano, faixa_patrimonio) %>%
  slice_max(score_final, n = 1) %>%
  ungroup() %>%
  select(ano, faixa_patrimonio, estrutura, score_final) %>%
  arrange(ano)

cat("\n=== RECOMENDAÇÃO DINÂMICA POR ANO ===\n")
cat("(Qual estrutura é ótima conforme o patrimônio evolui)\n\n")
print(recomendacao_por_ano)

# ============================================================================
# 7. TABELA RESUMIDA: EVOLUÇÃO DA RECOMENDAÇÃO
# ============================================================================

resumo_recomendacao <- patrimonio_classificado %>%
  left_join(scores_combinados %>% select(estrutura, score_final), 
            by = "estrutura") %>%
  group_by(ano, faixa_patrimonio) %>%
  slice_max(score_final, n = 1) %>%
  ungroup() %>%
  select(ano, faixa_patrimonio, estrutura, patrimonio_liquido, score_final) %>%
  arrange(ano) %>%
  mutate(
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", 
                                decimal.mark = ",", nsmall = 0),
    score_final = round(score_final, 2)
  )

cat("\n=== RESUMO: ESTRUTURA RECOMENDADA POR ANO ===\n")
print(resumo_recomendacao)

# ============================================================================
# 8. GRÁFICO 1: EVOLUÇÃO DO PATRIMÔNIO COM RECOMENDAÇÃO (CORRIGIDO)
# ============================================================================

# Preparar dados para gráfico
dados_grafico <- resultados_completos %>%
  filter(ano > 0) %>%
  select(ano, estrutura, patrimonio_liquido) %>%
  left_join(scores_combinados %>% select(estrutura, score_final), 
            by = "estrutura")

# Identificar estrutura recomendada por ano
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
             color = estrutura, size = recomendada, linetype = recomendada)) +
  geom_line() +
  geom_point() +
  scale_size_manual(values = c("FALSE" = 1, "TRUE" = 2.5), guide = "none") +  # CORRIGIDO
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "solid"), guide = "none") +  # CORRIGIDO
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  labs(
    title = "Evolução do Patrimônio e Estrutura Recomendada (5 Anos)",
    subtitle = "Linhas mais grossas indicam a estrutura recomendada a cada ano",
    x = "Ano",
    y = "Patrimônio Líquido (R$ Milhões)",
    color = "Estrutura"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50")
  )

print(grafico_evolucao_recomendacao)

# ============================================================================
# 9. GRÁFICO 2: SCORE FINAL DE CADA ESTRUTURA
# ============================================================================

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
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50")
  )

print(grafico_scores)

# ============================================================================
# 10. GRÁFICO 3: MUDANÇA DE RECOMENDAÇÃO AO LONGO DO TEMPO
# ============================================================================

# Contar quantas vezes a recomendação muda
mudancas_recomendacao <- recomendacao_por_ano %>%
  select(ano, estrutura) %>%
  mutate(
    mudou = estrutura != lag(estrutura),
    mudou = replace_na(mudou, FALSE)
  )

cat("\n=== MUDANÇAS NA RECOMENDAÇÃO ===\n")
cat("A recomendação mudou nos seguintes períodos:\n")
print(mudancas_recomendacao %>% filter(mudou == TRUE))

# ============================================================================
# 11. CONCLUSÃO: QUAL ESTRUTURA VENCE?
# ============================================================================

estrutura_vencedora <- scores_combinados %>%
  slice(1)

cat("\n=== CONCLUSÃO: ESTRUTURA RECOMENDADA ===\n")
cat("Estrutura com melhor score final:", estrutura_vencedora$estrutura, "\n")
cat("Score AHP:", round(estrutura_vencedora$score_ahp, 2), "\n")
cat("Score Fiscal:", round(estrutura_vencedora$score_fiscal, 2), "\n")
cat("Score Final:", round(estrutura_vencedora$score_final, 2), "\n")

# ============================================================================
# FIM DO SCRIPT 3
# ============================================================================