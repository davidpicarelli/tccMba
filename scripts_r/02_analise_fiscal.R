# ============================================================================
# SCRIPT 2: ANÁLISE FISCAL COMPARATIVA - CUSTO TRIBUTÁRIO E OPERACIONAL (5 ANOS)
# Gera exatamente as métricas da Tabela 2 do TCC (sem duplicação nos fundos)
# ============================================================================

library(tidyverse)
library(ggplot2)

if (!exists("resultados_completos")) {
  stop("Erro: Execute o Script 1 primeiro!")
}

# ============================================================================
# 1. CONSOLIDAR ENCARGOS ACUMULADOS EM 5 ANOS (10 SEMESTRES)
# ============================================================================

analise_fiscal <- resultados_completos %>%
  filter(semestre > 0) %>%
  group_by(estrutura) %>%
  summarise(
    come_cotas_total = sum(come_cotas, na.rm = TRUE),
    ir_ordinario_total = sum(imposto_ir, na.rm = TRUE),
    custo_operacional_total = sum(custo_operacional_valor, na.rm = TRUE),
    custo_fiscal_total = come_cotas_total + ir_ordinario_total + custo_operacional_total,
    patrimonio_final = last(patrimonio_liquido),
    patrimonio_inicial = first(patrimonio_inicial_periodo),
    retorno_liquido = patrimonio_final - patrimonio_inicial,
    retorno_liquido_pct = ((patrimonio_final / patrimonio_inicial) - 1) * 100,
    taxa_tributacao_efetiva = (custo_fiscal_total / (patrimonio_inicial * 5)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(patrimonio_final))

cat("\n=== TABELA 2: SÍNTESE DA PROGRESSÃO PATRIMONIAL E CUSTO ACUMULADO (5 ANOS) ===\n")
print(analise_fiscal %>% select(
  estrutura, come_cotas_total, ir_ordinario_total, 
  custo_operacional_total, custo_fiscal_total, 
  patrimonio_final, retorno_liquido_pct
))

# ============================================================================
# 2. GRÁFICO 1: CUSTO FISCAL E OPERACIONAL ACUMULADO (5 ANOS)
# ============================================================================

grafico_custo_acumulado <- analise_fiscal %>%
  ggplot(aes(x = reorder(estrutura, -custo_fiscal_total), 
             y = custo_fiscal_total / 1000000, 
             fill = estrutura)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0("R$ ", format(round(custo_fiscal_total / 1000000, 2), decimal.mark = ","), "M")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M", decimal.mark = ",")) +
  labs(
    title = "Custo Fiscal e Operacional Acumulado (5 Anos) por Estrutura",
    subtitle = "Soma de come-cotas, IR ordinário e custos operacionais (10 semestres)",
    x = "Estrutura de Investimento",
    y = "Custo Total (R$ Milhões)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50")
  )

print(grafico_custo_acumulado)

# ============================================================================
# 3. GRÁFICO 2: COMPOSIÇÃO DO CUSTO ACUMULADO (STACKED BAR)
# ============================================================================

composicao_fiscal <- analise_fiscal %>%
  select(estrutura, come_cotas_total, ir_ordinario_total, custo_operacional_total, custo_fiscal_total) %>%
  pivot_longer(
    cols = c(come_cotas_total, ir_ordinario_total, custo_operacional_total),
    names_to = "tipo_imposto",
    values_to = "valor"
  ) %>%
  mutate(
    tipo_imposto = case_when(
      tipo_imposto == "come_cotas_total" ~ "Come-cotas",
      tipo_imposto == "ir_ordinario_total" ~ "IR Ordinário",
      tipo_imposto == "custo_operacional_total" ~ "Custo Operacional"
    )
  )

grafico_composicao <- composicao_fiscal %>%
  ggplot(aes(x = reorder(estrutura, -custo_fiscal_total), 
             y = valor / 1000000, 
             fill = tipo_imposto)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M", decimal.mark = ",")) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Composição do Custo Fiscal e Operacional por Estrutura (5 Anos)",
    subtitle = "Detalhamento entre come-cotas, IR ordinário e custos operacionais",
    x = "Estrutura de Investimento",
    y = "Valor (R$ Milhões)",
    fill = "Natureza do Encargo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    legend.position = "bottom"
  )

print(grafico_composicao)