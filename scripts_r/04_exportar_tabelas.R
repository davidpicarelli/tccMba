# ============================================================================
# SCRIPT 4 CORRIGIDO v2: EXPORTAR TABELAS PARA TODAS AS 6 ESTRUTURAS
# ============================================================================

library(flextable)
library(tidyverse)

# ============================================================================
# 1. CRIAR PASTA PARA TABELAS
# ============================================================================

if (!dir.exists("tabelas")) {
  dir.create("tabelas")
  cat("✓ Pasta 'tabelas' criada\n")
}

cat("\n=== EXPORTANDO TABELAS PARA TODAS AS 6 ESTRUTURAS ===\n")

# ============================================================================
# 2. EXPORTAR TABELAS DE PROGRESSÃO (UMA PARA CADA ESTRUTURA) - CORRIGIDO
# ============================================================================

estruturas_lista <- unique(resultados_completos$estrutura)

for (est in estruturas_lista) {
  
  tabela_estrutura_dados <- resultados_completos %>%
    filter(estrutura == est, semestre %% 2 == 0) %>%
    select(ano, patrimonio_inicial_periodo, lucro_semestral, come_cotas, patrimonio_liquido) %>%
    mutate(
      patrimonio_inicial_periodo = format(patrimonio_inicial_periodo, big.mark = ".", decimal.mark = ",", nsmall = 0),
      lucro_semestral = format(lucro_semestral, big.mark = ".", decimal.mark = ",", nsmall = 0),
      come_cotas = format(come_cotas, big.mark = ".", decimal.mark = ",", nsmall = 0),
      patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", nsmall = 0)
    ) %>%
    rename(
      "Ano" = ano,
      "Patrimônio Inicial" = patrimonio_inicial_periodo,
      "Lucro Semestral" = lucro_semestral,
      "Come-cotas" = come_cotas,
      "Patrimônio Líquido" = patrimonio_liquido
    )
  
  # Criar tabela com flextable() diretamente (não usar as_flextable())
  tabela_formatada <- flextable(tabela_estrutura_dados) %>%
    theme_vanilla() %>%
    autofit() %>%
    # Adicionar nota em itálico no rodapé
    add_footer_lines(
      "Nota: Valores em Reais Brasileiros. Fonte: Elaborado pelo autor em linguagem R"
    ) %>%
    # Formatar nota em itálico
    italic(i = 1, part = "footer") %>%
    # Remover o "n: X" padrão do flextable
    set_caption(caption = NULL)
  
  # Criar nome do arquivo
  nome_arquivo <- est %>%
    tolower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[/()/]", "") %>%
    str_replace_all("-", "_")
  
  # Salvar como Word
  caminho <- paste0("tabelas/progressao_", nome_arquivo, ".docx")
  save_as_docx(tabela_formatada, path = caminho)
  
  cat("✓ Tabela exportada:", caminho, "\n")
}

# ============================================================================
# 3. EXPORTAR TABELA 2: ANÁLISE FISCAL COMPARATIVA (CORRIGIDO v3)
# ============================================================================

tabela_fiscal_dados <- analise_fiscal %>%
  select(estrutura, custo_fiscal_total, taxa_tributacao_efetiva, patrimonio_final) %>%
  mutate(
    custo_fiscal_total = format(custo_fiscal_total, big.mark = ".", decimal.mark = ",", nsmall = 2),
    taxa_tributacao_efetiva = paste0(round(taxa_tributacao_efetiva, 2), "%"),
    patrimonio_final = format(patrimonio_final, big.mark = ".", decimal.mark = ",", nsmall = 0)
  ) %>%
  rename(
    "Estrutura" = estrutura,
    "Custo Fiscal Total" = custo_fiscal_total,
    "Taxa Tributação Efetiva" = taxa_tributacao_efetiva,
    "Patrimônio Final" = patrimonio_final
  )

# Criar tabela com flextable() diretamente (não usar as_flextable())
tabela2_formatada <- flextable(tabela_fiscal_dados) %>%
  theme_vanilla() %>%
  autofit() %>%
  # Adicionar nota em itálico no rodapé
  add_footer_lines(
    "Nota: Valores em Reais Brasileiros. Fonte: Elaborado pelo autor em linguagem R"
  ) %>%
  # Formatar nota em itálico
  italic(i = 1, part = "footer") %>%
  # Remover o "n: 6" padrão do flextable
  set_caption(caption = NULL) %>%
  # Desabilitar o contador de linhas
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(tabela2_formatada, path = "tabelas/02_analise_fiscal_comparativa.docx")
cat("✓ Tabela 2 exportada: tabelas/02_analise_fiscal_comparativa.docx\n")

# ============================================================================
# 4. EXPORTAR TABELA 3: RECOMENDAÇÃO DINÂMICA (Script 3)
# ============================================================================

if (exists("resumo_recomendacao")) {
  
  tabela_recomendacao <- resumo_recomendacao %>%
    mutate(
      ano = as.character(ano),
      patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", nsmall = 0),
      score_final = as.character(score_final)
    ) %>%
    rename(
      "Ano" = ano,
      "Faixa de Patrimônio" = faixa_patrimonio,
      "Estrutura Recomendada" = estrutura,
      "Patrimônio Líquido (R$)" = patrimonio_liquido,
      "Score Final" = score_final
    )
  
  tabela3_formatada <- as_flextable(tabela_recomendacao) %>%
    theme_vanilla() %>%
    autofit()
  
  save_as_docx(tabela3_formatada, path = "tabelas/03_recomendacao_dinamica.docx")
  cat("✓ Tabela 3 exportada: tabelas/03_recomendacao_dinamica.docx\n")
  
} else {
  cat("⚠ Tabela 3: Execute o Script 3 primeiro\n")
}

# ============================================================================
# 5. EXPORTAR TABELA 4: SCORES COMBINADOS (Script 3)
# ============================================================================

if (exists("scores_combinados")) {
  
  tabela_scores <- scores_combinados %>%
    mutate(
      score_ahp = round(score_ahp, 2),
      score_fiscal = round(score_fiscal, 2),
      score_final = round(score_final, 2)
    ) %>%
    rename(
      "Estrutura" = estrutura,
      "Score AHP" = score_ahp,
      "Score Fiscal" = score_fiscal,
      "Score Final" = score_final
    )
  
  tabela4_formatada <- as_flextable(tabela_scores) %>%
    theme_vanilla() %>%
    autofit()
  
  save_as_docx(tabela4_formatada, path = "tabelas/04_scores_combinados.docx")
  cat("✓ Tabela 4 exportada: tabelas/04_scores_combinados.docx\n")
  
} else {
  cat("⚠ Tabela 4: Execute o Script 3 primeiro\n")
}

# ============================================================================
# 6. RESUMO FINAL
# ============================================================================

cat("\n=== RESUMO DAS EXPORTAÇÕES ===\n")
cat("Tabelas exportadas para a pasta 'tabelas/':\n\n")
cat("PROGRESSÃO PATRIMONIAL (uma para cada estrutura):\n")
for (est in estruturas_lista) {
  nome_arquivo <- est %>%
    tolower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[/()/]", "") %>%
    str_replace_all("-", "_")
  cat("  • progressao_", nome_arquivo, ".docx\n", sep = "")
}
cat("\nCOMPARATIVAS:\n")
cat("  • 02_analise_fiscal_comparativa.docx\n")
cat("  • 03_recomendacao_dinamica.docx (após Script 3)\n")
cat("  • 04_scores_combinados.docx (após Script 3)\n")
cat("\nPróximo passo: Abra cada arquivo e copie as tabelas para seu TCC Word.\n")

# ============================================================================
# FIM DO SCRIPT 4 CORRIGIDO v2
# ============================================================================