# ============================================================================
# SCRIPT 5: EXPORTAR GRÁFICOS (INDIVIDUAIS + COMPARATIVOS)
# Gera gráficos de progressão para cada estrutura + gráficos comparativos
# ============================================================================

library(ggplot2)
library(tidyverse)

# ============================================================================
# 1. CRIAR PASTA PARA GRÁFICOS (se não existir)
# ============================================================================

if (!dir.exists("graficos")) {
  dir.create("graficos")
  cat("✓ Pasta 'graficos' criada\n")
}

cat("\n=== EXPORTANDO GRÁFICOS ===\n")

# ============================================================================
# 2. GRÁFICOS INDIVIDUAIS: PROGRESSÃO POR ESTRUTURA (6 GRÁFICOS) - CORRIGIDO v5
# ============================================================================

estruturas_lista <- unique(resultados_completos$estrutura)

for (est in estruturas_lista) {
  
  # Filtrar dados da estrutura (TODOS OS SEMESTRES)
  dados_estrutura <- resultados_completos %>%
    filter(estrutura == est, semestre > 0) %>%
    select(semestre, patrimonio_liquido, come_cotas) %>%
    mutate(
      tempo_meses = semestre * 6,
      # Patrimônio ANTES do come-cotas
      patrimonio_antes_come_cotas = patrimonio_liquido + come_cotas
    ) %>%
    arrange(tempo_meses)
  
  # Criar dois pontos para cada semestre onde há come-cotas:
  # 1. Ponto ANTES (patrimonio_antes_come_cotas) 
  # 2. Ponto DEPOIS (patrimonio_liquido)
  # Isso cria a "queda" visual na linha
  
  dados_com_queda <- data.frame()
  
  for (i in 1:nrow(dados_estrutura)) {
    # Adicionar ponto normal
    dados_com_queda <- rbind(dados_com_queda, data.frame(
      tempo_meses = dados_estrutura$tempo_meses[i],
      patrimonio = dados_estrutura$patrimonio_liquido[i],
      tipo = "normal"
    ))
    
    # Se há come-cotas, adicionar ponto ANTES (para criar a queda)
    if (dados_estrutura$come_cotas[i] > 0) {
      # Adicionar ponto ANTES do come-cotas (com um pequeno offset para clareza)
      dados_com_queda <- rbind(dados_com_queda, data.frame(
        tempo_meses = dados_estrutura$tempo_meses[i] - 0.01,  # Offset mínimo
        patrimonio = dados_estrutura$patrimonio_antes_come_cotas[i],
        tipo = "antes_come_cotas"
      ))
    }
  }
  
  # Ordenar por tempo para a linha conectar corretamente
  dados_com_queda <- dados_com_queda %>% arrange(tempo_meses)
  
  # Criar gráfico
  grafico_individual <- dados_com_queda %>%
    ggplot(aes(x = tempo_meses, y = patrimonio / 1000000)) +
    # Linha contínua do patrimônio (mostra a queda do come-cotas)
    geom_line(color = "#2ecc71", linewidth = 1.2) +
    # Pontos em todos os semestres
    geom_point(color = "#2ecc71", size = 3) +
    # Adicionar anotações nos meses de maio e novembro (onde come-cotas é deduzido)
    geom_vline(
      data = dados_estrutura %>% filter(come_cotas > 0),
      aes(xintercept = tempo_meses),
      color = "#e74c3c",
      linetype = "dotted",
      linewidth = 0.8,
      alpha = 0.5
    ) +
    scale_x_continuous(
      breaks = seq(0, 60, 6),
      labels = seq(0, 60, 6)
    ) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
    labs(
      title = paste0("Progressão Patrimonial - ", est),
      subtitle = "Patrimônio Líquido ao longo de 5 anos | Juros compostos (8% a.a.) | Come-cotas deduzido em maio e novembro",
      x = "Tempo (Meses)",
      y = "Patrimônio Líquido (R$ Milhões)",
      caption = "Nota: Linha verde = Patrimônio Líquido | Quedas visíveis em maio (mês 5, 17, 29, 41, 53) e novembro (mês 11, 23, 35, 47, 59)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray50"),
      plot.caption = element_text(size = 8, color = "gray60", hjust = 0),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.4)
    )
  
  # Criar nome do arquivo
  nome_arquivo <- est %>%
    tolower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[/()/]", "") %>%
    str_replace_all("-", "_")
  
  # Salvar em PNG (300 DPI)
  ggsave(
    filename = paste0("graficos/progressao_", nome_arquivo, ".png"),
    plot = grafico_individual,
    width = 12,
    height = 7,
    dpi = 300,
    units = "in"
  )
  
  # Salvar em PDF (vetorial)
  ggsave(
    filename = paste0("graficos/progressao_", nome_arquivo, ".pdf"),
    plot = grafico_individual,
    width = 12,
    height = 7,
    units = "in"
  )
  
  cat("✓ Gráfico individual exportado: progressao_", nome_arquivo, ".png / .pdf\n", sep = "")
}
# ============================================================================
# 3. GRÁFICO COMPARATIVO 1: EVOLUÇÃO DO PATRIMÔNIO (TODAS AS ESTRUTURAS)
# ============================================================================

dados_grafico <- resultados_completos %>%
  filter(semestre %% 2 == 0)  # Apenas anos

grafico_evolucao <- dados_grafico %>%
  ggplot(aes(x = ano, y = patrimonio_liquido / 1000000, 
             color = estrutura, group = estrutura)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  labs(
    title = "Evolução do Patrimônio Líquido por Estrutura (5 Anos)",
    subtitle = "Patrimônio inicial: R$ 5M | Retorno: 8% a.a.",
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

ggsave(
  filename = "graficos/01_evolucao_patrimonio.png",
  plot = grafico_evolucao,
  width = 12,
  height = 7,
  dpi = 300,
  units = "in"
)

ggsave(
  filename = "graficos/01_evolucao_patrimonio.pdf",
  plot = grafico_evolucao,
  width = 12,
  height = 7,
  units = "in"
)

cat("✓ Gráfico comparativo 1 exportado: 01_evolucao_patrimonio.png / .pdf\n")

# ============================================================================
# 4. GRÁFICO COMPARATIVO 2: CUSTO FISCAL ACUMULADO
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

cat("✓ Gráfico comparativo 2 exportado: 02_custo_fiscal_acumulado.png / .pdf\n")

# ============================================================================
# 5. GRÁFICO COMPARATIVO 3: COMPOSIÇÃO DO CUSTO FISCAL
# ============================================================================

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

cat("✓ Gráfico comparativo 3 exportado: 03_composicao_custo_fiscal.png / .pdf\n")

# ============================================================================
# 6. GRÁFICO COMPARATIVO 4: PATRIMÔNIO FINAL vs CUSTO FISCAL
# ============================================================================

grafico_patrimonio_vs_custo <- analise_fiscal %>%
  pivot_longer(
    cols = c(patrimonio_final, custo_fiscal_total),
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "patrimonio_final" ~ "Patrimônio Final",
      metrica == "custo_fiscal_total" ~ "Custo Fiscal"
    )
  ) %>%
  ggplot(aes(x = reorder(estrutura, -valor), 
             y = valor / 1000000, 
             fill = metrica)) +
  geom_col(position = "dodge") +
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

cat("✓ Gráfico comparativo 4 exportado: 04_patrimonio_vs_custo.png / .pdf\n")

# ============================================================================
# 7. GRÁFICO COMPARATIVO 5: EVOLUÇÃO COM RECOMENDAÇÃO (Script 3)
# ============================================================================

if (exists("grafico_evolucao_recomendacao")) {
  
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
  
  cat("✓ Gráfico comparativo 5 exportado: 05_evolucao_recomendacao.png / .pdf\n")
  
} else {
  cat("⚠ Gráfico 5: Execute o Script 3 primeiro\n")
}

# ============================================================================
# 8. GRÁFICO COMPARATIVO 6: SCORES FINAIS (Script 3)
# ============================================================================

if (exists("grafico_scores")) {
  
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
  
  cat("✓ Gráfico comparativo 6 exportado: 06_scores_finais.png / .pdf\n")
  
} else {
  cat("⚠ Gráfico 6: Execute o Script 3 primeiro\n")
}

# ============================================================================
# 9. RESUMO FINAL
# ============================================================================

cat("\n=== RESUMO DAS EXPORTAÇÕES ===\n\n")
cat("GRÁFICOS INDIVIDUAIS (Progressão por Estrutura):\n")
for (est in estruturas_lista) {
  nome_arquivo <- est %>%
    tolower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[/()/]", "") %>%
    str_replace_all("-", "_")
  cat("  • progressao_", nome_arquivo, ".png / .pdf\n", sep = "")
}

cat("\nGRÁFICOS COMPARATIVOS:\n")
cat("  • 01_evolucao_patrimonio.png / .pdf\n")
cat("  • 02_custo_fiscal_acumulado.png / .pdf\n")
cat("  • 03_composicao_custo_fiscal.png / .pdf\n")
cat("  • 04_patrimonio_vs_custo.png / .pdf\n")
cat("  • 05_evolucao_recomendacao.png / .pdf (após Script 3)\n")
cat("  • 06_scores_finais.png / .pdf (após Script 3)\n")

cat("\nTOTAL: 12 gráficos individuais + 6 gráficos comparativos = 18 arquivos\n")
cat("(6 PNG + 6 PDF individuais + 12 PNG/PDF comparativos)\n")

cat("\nPróximo passo: Insira os gráficos no seu TCC Word (Apêndice 4.4)\n")

# ============================================================================
# FIM DO SCRIPT 5
# ============================================================================