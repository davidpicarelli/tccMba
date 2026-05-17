# ============================================================================
# SCRIPT 4: EXPORTAR TABELAS PARA WORD
# Converte tabelas dos Scripts 1, 2 e 3 em documentos Word formatados
# ============================================================================

# Instalar flextable (execute uma única vez)
# install.packages("flextable")

# Carregar bibliotecas
library(flextable)
library(tidyverse)

# ============================================================================
# 1. CRIAR PASTA PARA TABELAS (se não existir)
# ============================================================================

if (!dir.exists("tabelas")) {
  dir.create("tabelas")
  cat("✓ Pasta 'tabelas' criada\n")
}

# ============================================================================
# 2. EXPORTAR TABELA 1: PROGRESSÃO PATRIMONIAL (Script 1)
# ============================================================================

cat("\n=== EXPORTANDO TABELAS ===\n")

# Tabela de progressão (exemplo: Fundos Multimercado)
tabela_progressao <- resultados_completos %>%
  filter(estrutura == "Fundos Multimercado") %>%
  select(ano, patrimonio_bruto, imposto_come_cotas, imposto_ir, patrimonio_liquido) %>%
  mutate(
    patrimonio_bruto = format(patrimonio_bruto, big.mark = ".", decimal.mark = ",", nsmall = 0),
    imposto_come_cotas = format(imposto_come_cotas, big.mark = ".", decimal.mark = ",", nsmall = 0),
    imposto_ir = format(imposto_ir, big.mark = ".", decimal.mark = ",", nsmall = 0),
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", nsmall = 0)
  ) %>%
  rename(
    "Ano" = ano,
    "Patrimônio Bruto (R$)" = patrimonio_bruto,
    "Come-cotas (R$)" = imposto_come_cotas,
    "IR Ordinário (R$)" = imposto_ir,
    "Patrimônio Líquido (R$)" = patrimonio_liquido
  )

# Formatar com flextable
tabela1_formatada <- as_flextable(tabela_progressao) %>%
  theme_vanilla() %>%
  autofit()

# Salvar como Word
save_as_docx(tabela1_formatada, path = "tabelas/01_progressao_patrimonial.docx")
cat("✓ Tabela 1 exportada: tabelas/01_progressao_patrimonial.docx\n")

# ============================================================================
# 3. EXPORTAR TABELA 2: ANÁLISE FISCAL (Script 2)
# ============================================================================

tabela_fiscal <- analise_fiscal %>%
  select(estrutura, custo_fiscal_total, taxa_tributacao_efetiva, patrimonio_final) %>%
  mutate(
    custo_fiscal_total = format(custo_fiscal_total, big.mark = ".", decimal.mark = ",", nsmall = 0),
    taxa_tributacao_efetiva = paste0(round(taxa_tributacao_efetiva, 2), "%"),
    patrimonio_final = format(patrimonio_final, big.mark = ".", decimal.mark = ",", nsmall = 0)
  ) %>%
  rename(
    "Estrutura" = estrutura,
    "Custo Fiscal Total (R$)" = custo_fiscal_total,
    "Taxa Tributação Efetiva (%)" = taxa_tributacao_efetiva,
    "Patrimônio Final (R$)" = patrimonio_final
  )

tabela2_formatada <- as_flextable(tabela_fiscal) %>%
  theme_vanilla() %>%
  autofit()

save_as_docx(tabela2_formatada, path = "tabelas/02_analise_fiscal.docx")
cat("✓ Tabela 2 exportada: tabelas/02_analise_fiscal.docx\n")

# ============================================================================
# 4. EXPORTAR TABELA 3: RECOMENDAÇÃO DINÂMICA (Script 3)
# ============================================================================

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

# ============================================================================
# 5. EXPORTAR TABELA 4: SCORES COMBINADOS (Script 3)
# ============================================================================

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

# ============================================================================
# 6. RESUMO FINAL
# ============================================================================

cat("\n=== RESUMO DAS EXPORTAÇÕES ===\n")
cat("Todas as tabelas foram exportadas para a pasta 'tabelas/':\n")
cat("  1. 01_progressao_patrimonial.docx\n")
cat("  2. 02_analise_fiscal.docx\n")
cat("  3. 03_recomendacao_dinamica.docx\n")
cat("  4. 04_scores_combinados.docx\n")
cat("\nPróximo passo: Abra cada arquivo, copie a tabela e cole no seu TCC Word.\n")

# ============================================================================
# FIM DO SCRIPT 4
# ============================================================================