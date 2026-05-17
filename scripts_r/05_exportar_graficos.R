# ============================================================================
# SCRIPT 5: EXPORTAR GRÁFICOS PARA ARQUIVO
# Salva todos os gráficos dos Scripts 1, 2 e 3 em alta resolução (PNG e PDF)
# ============================================================================

# Carregar bibliotecas necessárias
library(ggplot2)
library(tidyverse)

# ============================================================================
# 1. CRIAR PASTA PARA GRÁFICOS (se não existir)
# ============================================================================

if (!dir.exists("graficos")) {
  dir.create("graficos")
  cat("✓ Pasta 'graficos' criada\n")
}

# ============================================================================
# 2. EXPORTAR GRÁFICO 1: EVOLUÇÃO DO PATRIMÔNIO (Script 1)
# ============================================================================

cat("\n=== EXPORTANDO GRÁFICOS ===\n")

# Salvar em PNG (alta resolução)
ggsave(
  filename = "graficos/01_evolucao_patrimonio.png",
  plot = grafico_evolucao,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

# Salvar em PDF (vetorial, melhor para impressão)
ggsave(
  filename = "graficos/01_evolucao_patrimonio.pdf",
  plot = grafico_evolucao,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico 1 exportado: 01_evolucao_patrimonio.png / .pdf\n")

# ============================================================================
# 3. EXPORTAR GRÁFICO 2: CUSTO FISCAL ACUMULADO (Script 2)
# ============================================================================

ggsave(
  filename = "graficos/02_custo_fiscal_acumulado.png",
  plot = grafico_custo_acumulado,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "graficos/02_custo_fiscal_acumulado.pdf",
  plot = grafico_custo_acumulado,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico 2 exportado: 02_custo_fiscal_acumulado.png / .pdf\n")

# ============================================================================
# 4. EXPORTAR GRÁFICO 3: COMPOSIÇÃO DO CUSTO FISCAL (Script 2)
# ============================================================================

ggsave(
  filename = "graficos/03_composicao_custo_fiscal.png",
  plot = grafico_composicao,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "graficos/03_composicao_custo_fiscal.pdf",
  plot = grafico_composicao,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico 3 exportado: 03_composicao_custo_fiscal.png / .pdf\n")

# ============================================================================
# 5. EXPORTAR GRÁFICO 4: PATRIMÔNIO FINAL vs CUSTO FISCAL (Script 2)
# ============================================================================

ggsave(
  filename = "graficos/04_patrimonio_vs_custo.png",
  plot = grafico_patrimonio_vs_custo,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "graficos/04_patrimonio_vs_custo.pdf",
  plot = grafico_patrimonio_vs_custo,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico 4 exportado: 04_patrimonio_vs_custo.png / .pdf\n")

# ============================================================================
# 6. EXPORTAR GRÁFICO 5: EVOLUÇÃO COM RECOMENDAÇÃO (Script 3)
# ============================================================================

ggsave(
  filename = "graficos/05_evolucao_recomendacao.png",
  plot = grafico_evolucao_recomendacao,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "graficos/05_evolucao_recomendacao.pdf",
  plot = grafico_evolucao_recomendacao,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico 5 exportado: 05_evolucao_recomendacao.png / .pdf\n")

# ============================================================================
# 7. EXPORTAR GRÁFICO 6: SCORES FINAIS (Script 3)
# ============================================================================

ggsave(
  filename = "graficos/06_scores_finais.png",
  plot = grafico_scores,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "graficos/06_scores_finais.pdf",
  plot = grafico_scores,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico 6 exportado: 06_scores_finais.png / .pdf\n")

# ============================================================================
# 8. RESUMO FINAL
# ============================================================================

cat("\n=== RESUMO DAS EXPORTAÇÕES ===\n")
cat("Todos os gráficos foram exportados para a pasta 'graficos/':\n\n")
cat("SCRIPT 1 (Progressão Patrimonial):\n")
cat("  • 01_evolucao_patrimonio.png / .pdf\n\n")
cat("SCRIPT 2 (Análise Fiscal):\n")
cat("  • 02_custo_fiscal_acumulado.png / .pdf\n")
cat("  • 03_composicao_custo_fiscal.png / .pdf\n")
cat("  • 04_patrimonio_vs_custo.png / .pdf\n\n")
cat("SCRIPT 3 (Recomendação Dinâmica):\n")
cat("  • 05_evolucao_recomendacao.png / .pdf\n")
cat("  • 06_scores_finais.png / .pdf\n\n")
cat("PRÓXIMO PASSO: Insira os gráficos no Word (Inserir > Imagem > Do arquivo)\n")

# ============================================================================
# FIM DO SCRIPT 5
# ============================================================================