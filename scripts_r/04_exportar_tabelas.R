# ============================================================================
# SCRIPT 4: EXPORTAR TABELAS FORMATADAS PARA WORD (.DOCX)
# Reconcilia o Apêndice B (semestral/anual) com a Tabela 2 (10 semestres)
# ============================================================================

library(flextable)
library(tidyverse)

if (!dir.exists("tabelas")) {
  dir.create("tabelas")
}

cat("\n=== EXPORTANDO TABELAS PARA A PASTA 'tabelas/' ===\n")

# ============================================================================
# 1. EXPORTAR TABELAS DO APÊNDICE B (UMA PARA CADA ESTRUTURA)
# ============================================================================
# Nota: Para evitar que o leitor confunda o come-cotas do 2º semestre com o
# total anual, incluímos a nota explicativa clara no rodapé das tabelas.

estruturas_lista <- unique(resultados_completos$estrutura)

for (est in estruturas_lista) {
  
  tabela_estrutura_dados <- resultados_completos %>%
    filter(estrutura == est, semestre %% 2 == 0) %>%
    select(ano, patrimonio_inicial_periodo, lucro_semestral, come_cotas, patrimonio_liquido) %>%
    mutate(
      patrimonio_inicial_periodo = format(round(patrimonio_inicial_periodo, 0), big.mark = ".", decimal.mark = ",", nsmall = 0),
      lucro_semestral = format(round(lucro_semestral, 1), big.mark = ".", decimal.mark = ",", nsmall = 1),
      come_cotas = format(round(come_cotas, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
      patrimonio_liquido = format(round(patrimonio_liquido, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)
    ) %>%
    rename(
      "Ano" = ano,
      "Patrimônio Inicial (2º Semestre - R$)" = patrimonio_inicial_periodo,
      "Lucro Semestral (R$)" = lucro_semestral,
      "Come-cotas Semestral (R$)" = come_cotas,
      "Patrimônio Líquido Final do Ano (R$)" = patrimonio_liquido
    )
  
  tabela_formatada <- flextable(tabela_estrutura_dados) %>%
    theme_vanilla() %>%
    autofit() %>%
    add_footer_lines(
      "Nota: Valores em Reais (BRL) referentes ao fechamento do 2º semestre de cada ano (Semestres 2, 4, 6, 8 e 10). O custo acumulado quinquenal na Tabela 2 soma os 10 semestres. Fonte: Elaborado pelo autor em linguagem R (2026)."
    ) %>%
    italic(i = 1, part = "footer") %>%
    set_caption(caption = NULL)
  
  nome_arquivo <- est %>%
    tolower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[/()/]", "") %>%
    str_replace_all("-", "_")
  
  caminho <- paste0("tabelas/progressao_", nome_arquivo, ".docx")
  save_as_docx(tabela_formatada, path = caminho)
  cat("✓ Tabela exportada:", caminho, "\n")
}

# ============================================================================
# 2. EXPORTAR TABELA 2 COMPLETA (7 COLUNAS EXATAS DO ARTIGO)
# ============================================================================

tabela_fiscal_dados <- analise_fiscal %>%
  select(
    estrutura,
    come_cotas_total,
    ir_ordinario_total,
    custo_operacional_total,
    custo_fiscal_total,
    patrimonio_final,
    retorno_liquido_pct
  ) %>%
  mutate(
    come_cotas_total = format(round(come_cotas_total, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    ir_ordinario_total = format(round(ir_ordinario_total, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    custo_operacional_total = format(round(custo_operacional_total, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    custo_fiscal_total = format(round(custo_fiscal_total, 2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    patrimonio_final = format(round(patrimonio_final, 0), big.mark = ".", decimal.mark = ",", nsmall = 0),
    retorno_liquido_pct = paste0(format(round(retorno_liquido_pct, 2), decimal.mark = ",", nsmall = 2), "%")
  ) %>%
  rename(
    "Estrutura de Investimento" = estrutura,
    "Come-cotas Acumulado (R$)" = come_cotas_total,
    "IR Ordinário Acumulado (R$)" = ir_ordinario_total,
    "Custo Operac. Acumulado (R$)" = custo_operacional_total,
    "Custo Total 5 Anos (R$)" = custo_fiscal_total,
    "Patrimônio Final (R$)" = patrimonio_final,
    "Retorno Líq. (%)" = retorno_liquido_pct
  )

tabela2_formatada <- flextable(tabela_fiscal_dados) %>%
  theme_vanilla() %>%
  autofit() %>%
  add_footer_lines(
    "Nota: Simulação determinística acumulada em 10 semestres (5 anos) com retorno bruto de 8% a.a. (capitalização semestral). Nos fundos locais, a tributação de 15% ocorre exclusivamente via come-cotas semestral. Fonte: Elaborado pelo autor em linguagem R (2026)."
  ) %>%
  italic(i = 1, part = "footer") %>%
  set_caption(caption = NULL) %>%
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(tabela2_formatada, path = "tabelas/02_analise_fiscal_comparativa.docx")
cat("✓ Tabela 2 exportada: tabelas/02_analise_fiscal_comparativa.docx\n")