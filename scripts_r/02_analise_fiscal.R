# ============================================================================
# SCRIPT 2: ANÁLISE FISCAL COMPARATIVA - CUSTO TRIBUTÁRIO ACUMULADO
# Comparação de impacto fiscal entre estruturas offshore ao longo de 5 anos
# TCC: Análise Comparativa de Estruturas de Investimento Offshore
# ============================================================================

# Carregar bibliotecas necessárias
library(tidyverse)
library(ggplot2)

# ============================================================================
# 1. USAR RESULTADOS DO SCRIPT 1
# ============================================================================

# Se você rodou o Script 1, os dados já estão em 'resultados_completos'
# Se não, execute o Script 1 primeiro ou copie os dados abaixo

# Caso contrário, aqui está um resumo dos dados para continuar:
# (Pulamos a simulação completa e focamos na análise fiscal)

# ============================================================================
# 2. CALCULAR IMPOSTOS TOTAIS POR ESTRUTURA (5 ANOS)
# ============================================================================

# Usar os dados do Script 1
analise_fiscal <- resultados_completos %>%
  group_by(estrutura) %>%
  summarise(
    # Soma de come-cotas ao longo de 5 anos
    come_cotas_total = sum(imposto_come_cotas, na.rm = TRUE),
    
    # Soma de IR ordinário ao longo de 5 anos
    ir_ordinario_total = sum(imposto_ir, na.rm = TRUE),
    
    # Soma de custos operacionais ao longo de 5 anos
    custo_operacional_total = sum(custo_operacional_valor, na.rm = TRUE),
    
    # Total de impostos + custos
    custo_fiscal_total = come_cotas_total + ir_ordinario_total + custo_operacional_total,
    
    # Patrimônio final (ano 5)
    patrimonio_final = last(patrimonio_liquido),
    
    # Patrimônio inicial
    patrimonio_inicial = first(patrimonio_liquido),
    
    # Retorno líquido em reais
    retorno_liquido = patrimonio_final - patrimonio_inicial,
    
    # Taxa efetiva de tributação (%)
    taxa_tributacao_efetiva = (custo_fiscal_total / (patrimonio_inicial * 5)) * 100
  ) %>%
  arrange(desc(patrimonio_final))

# Exibir resultados
cat("\n=== ANÁLISE FISCAL COMPARATIVA (5 ANOS) ===\n")
print(analise_fiscal)

# ============================================================================
# 3. TABELA DETALHADA: IMPOSTO POR ANO
# ============================================================================

# Criar tabela mostrando evolução de impostos ano a ano
imposto_por_ano <- resultados_completos %>%
  filter(ano > 0) %>%  # Excluir ano 0 (inicial)
  select(ano, estrutura, imposto_come_cotas, imposto_ir, custo_operacional_valor) %>%
  mutate(
    imposto_total_ano = imposto_come_cotas + imposto_ir + custo_operacional_valor
  ) %>%
  arrange(estrutura, ano)

cat("\n=== IMPOSTOS POR ANO E ESTRUTURA ===\n")
print(imposto_por_ano)

# ============================================================================
# 4. GRÁFICO 1: CUSTO FISCAL ACUMULADO (5 ANOS)
# ============================================================================

grafico_custo_acumulado <- analise_fiscal %>%
  ggplot(aes(x = reorder(estrutura, -custo_fiscal_total), 
             y = custo_fiscal_total / 1000000, 
             fill = estrutura)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0("R$ ", round(custo_fiscal_total / 1000000, 2), "M")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  labs(
    title = "Custo Fiscal Acumulado (5 Anos) por Estrutura",
    subtitle = "Soma de come-cotas, IR ordinário e custos operacionais",
    x = "Estrutura de Investimento",
    y = "Custo Fiscal Total (R$ Milhões)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50")
  )

print(grafico_custo_acumulado)

# ============================================================================
# 5. GRÁFICO 2: COMPOSIÇÃO DO CUSTO FISCAL (STACKED BAR)
# ============================================================================

# Preparar dados para gráfico empilhado
composicao_fiscal <- analise_fiscal %>%
  select(estrutura, come_cotas_total, ir_ordinario_total, custo_operacional_total) %>%
  pivot_longer(
    cols = -estrutura,
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
  ggplot(aes(x = reorder(estrutura, -valor), 
             y = valor / 1000000, 
             fill = tipo_imposto)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Composição do Custo Fiscal por Estrutura (5 Anos)",
    subtitle = "Detalhamento de come-cotas, IR ordinário e custos operacionais",
    x = "Estrutura de Investimento",
    y = "Valor (R$ Milhões)",
    fill = "Tipo de Imposto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    legend.position = "bottom"
  )

print(grafico_composicao)

# ============================================================================
# 6. GRÁFICO 3: PATRIMÔNIO FINAL vs CUSTO FISCAL
# ============================================================================

grafico_patrimonio_vs_custo <- analise_fiscal %>%
  ggplot(aes(x = reorder(estrutura, -patrimonio_final))) +
  geom_col(aes(y = patrimonio_final / 1000000, fill = "Patrimônio Final"),
           alpha = 0.7) +
  geom_col(aes(y = custo_fiscal_total / 1000000, fill = "Custo Fiscal"),
           alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  scale_fill_manual(values = c("Patrimônio Final" = "#2ecc71", "Custo Fiscal" = "#e74c3c")) +
  labs(
    title = "Patrimônio Final vs Custo Fiscal (5 Anos)",
    subtitle = "Comparação entre patrimônio acumulado e custo tributário total",
    x = "Estrutura de Investimento",
    y = "Valor (R$ Milhões)",
    fill = "Métrica"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    legend.position = "bottom"
  )

print(grafico_patrimonio_vs_custo)

# ============================================================================
# 7. RANKING: ESTRUTURA MAIS EFICIENTE FISCALMENTE
# ============================================================================

cat("\n=== RANKING DE EFICIÊNCIA FISCAL ===\n")
ranking_eficiencia <- analise_fiscal %>%
  select(estrutura, custo_fiscal_total, taxa_tributacao_efetiva, patrimonio_final) %>%
  mutate(
    ranking = row_number(),
    custo_fiscal_total = format(custo_fiscal_total, big.mark = ".", decimal.mark = ",", nsmall = 0),
    taxa_tributacao_efetiva = paste0(round(taxa_tributacao_efetiva, 2), "%"),
    patrimonio_final = format(patrimonio_final, big.mark = ".", decimal.mark = ",", nsmall = 0)
  ) %>%
  select(ranking, estrutura, custo_fiscal_total, taxa_tributacao_efetiva, patrimonio_final)

print(ranking_eficiencia)

# ============================================================================
# 8. INSIGHT: ECONOMIA FISCAL COMPARADA À ESTRUTURA MAIS CARA
# ============================================================================

custo_maximo <- max(analise_fiscal$custo_fiscal_total)
economia_fiscal <- analise_fiscal %>%
  mutate(
    economia_vs_pior = custo_maximo - custo_fiscal_total,
    economia_percentual = (economia_vs_pior / custo_maximo) * 100
  ) %>%
  select(estrutura, custo_fiscal_total, economia_vs_pior, economia_percentual) %>%
  arrange(desc(economia_vs_pior))

cat("\n=== ECONOMIA FISCAL COMPARADA À ESTRUTURA MAIS CARA ===\n")
print(economia_fiscal)

# ============================================================================
# FIM DO SCRIPT 2
# ============================================================================