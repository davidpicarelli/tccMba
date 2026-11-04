# ============================================================================
# SCRIPT 1: SIMULADOR DE PROGRESSÃO PATRIMONIAL - 5 ANOS
# COME-COTAS CALCULADO APENAS SOBRE O LUCRO SEMESTRAL
# ============================================================================

library(tidyverse)
library(ggplot2)

# ============================================================================
# 1. DEFINIR PARÂMETROS DE SIMULAÇÃO
# ============================================================================

patrimonio_inicial <- 5000000  # R$ 5 milhões
retorno_anual <- 0.08          # 8% a.a.
retorno_semestral <- sqrt(1 + retorno_anual) - 1  # Converter para semestral
inflacao_anual <- 0.04
anos <- 5
semestres <- anos * 2

# ============================================================================
# 2. DEFINIR CARACTERÍSTICAS TRIBUTÁRIAS POR ESTRUTURA
# ============================================================================

estruturas <- data.frame(
  estrutura = c(
    "Contas Diretas",
    "Fundos Exclusivos",
    "Fundos Multimercado",
    "Estruturas Corporativas (LLC/IBC)",
    "Seguros Unit-Linked",
    "Trusts Fiduciários"
  ),
  
  # Come-cotas: alíquota sobre o LUCRO (não sobre o total)
  # 15% para fundos longo prazo, 0% para estruturas que não têm come-cotas
  come_cotas_aliquota = c(0, 0.15, 0.15, 0, 0, 0),
  
  # ITCMD (Imposto sobre Transmissão Causa Mortis e Doação)
  itcmd_aliquota = c(0.04, 0.04, 0.04, 0.02, 0.02, 0.04),
  
  # Imposto de renda ordinário (alíquota progressiva média)
  ir_ordinario = c(0.275, 0.15, 0.15, 0.15, 0.10, 0.275),
  
  # Custo operacional anual (% do patrimônio)
  custo_operacional = c(0.005, 0.010, 0.008, 0.015, 0.012, 0.020),
  
  stringsAsFactors = FALSE
)

cat("=== ESTRUTURAS E CARACTERÍSTICAS TRIBUTÁRIAS ===\n")
print(estruturas)

# ============================================================================
# 3. FUNÇÃO PARA SIMULAR PROGRESSÃO PATRIMONIAL (CORRIGIDA)
# ============================================================================

simular_progressao_corrigida <- function(patrimonio_inicial, 
                                         retorno_semestral,
                                         retorno_anual,
                                         inflacao_anual,
                                         come_cotas_aliquota,
                                         itcmd_aliquota,
                                         ir_ordinario,
                                         custo_operacional,
                                         semestres) {
  
  # Criar data frame para armazenar resultados
  resultado <- data.frame(
    semestre = 0:semestres,
    ano = 0:semestres / 2,
    patrimonio_inicial_periodo = NA,
    lucro_semestral = NA,
    come_cotas = NA,  # CORRIGIDO: apenas sobre o lucro
    patrimonio_apos_come_cotas = NA,
    imposto_ir = NA,
    custo_operacional_valor = NA,
    patrimonio_liquido = NA,
    patrimonio_real = NA,
    retorno_acumulado_pct = NA
  )
  
  # Semestre 0 (inicial)
  resultado$patrimonio_inicial_periodo[1] <- patrimonio_inicial
  resultado$lucro_semestral[1] <- 0
  resultado$come_cotas[1] <- 0
  resultado$patrimonio_apos_come_cotas[1] <- patrimonio_inicial
  resultado$imposto_ir[1] <- 0
  resultado$custo_operacional_valor[1] <- 0
  resultado$patrimonio_liquido[1] <- patrimonio_inicial
  resultado$patrimonio_real[1] <- patrimonio_inicial
  resultado$retorno_acumulado_pct[1] <- 0
  
  # Simulação para cada semestre
  for (i in 2:(semestres + 1)) {
    
    # Patrimônio no início do semestre
    patrimonio_anterior <- resultado$patrimonio_liquido[i - 1]
    resultado$patrimonio_inicial_periodo[i] <- patrimonio_anterior
    
    # ========== PASSO 1: CALCULAR LUCRO DO SEMESTRE ==========
    # Lucro = Patrimônio anterior × retorno semestral
    lucro_semestral <- patrimonio_anterior * retorno_semestral
    resultado$lucro_semestral[i] <- lucro_semestral
    
    # Patrimônio após retorno (antes de impostos)
    patrimonio_apos_retorno <- patrimonio_anterior + lucro_semestral
    
    # ========== PASSO 2: APLICAR COME-COTAS (APENAS SOBRE O LUCRO) ==========
    # Come-cotas = Lucro × Alíquota
    # IMPORTANTE: Come-cotas é calculado APENAS sobre o lucro, não sobre o patrimônio total
    come_cotas <- lucro_semestral * come_cotas_aliquota
    resultado$come_cotas[i] <- come_cotas
    
    # Patrimônio após come-cotas
    patrimonio_apos_come_cotas <- patrimonio_apos_retorno - come_cotas
    resultado$patrimonio_apos_come_cotas[i] <- patrimonio_apos_come_cotas
    
    # ========== PASSO 3: APLICAR IR ORDINÁRIO ==========
    # IR ordinário é calculado sobre o lucro (após come-cotas)
    ir_ordinario_valor <- lucro_semestral * ir_ordinario
    resultado$imposto_ir[i] <- ir_ordinario_valor
    
    # ========== PASSO 4: APLICAR CUSTO OPERACIONAL ==========
    # Custo operacional é anual, então dividir por 2 para semestral
    custo_op_semestral <- patrimonio_anterior * (custo_operacional / 2)
    resultado$custo_operacional_valor[i] <- custo_op_semestral
    
    # ========== PASSO 5: CALCULAR PATRIMÔNIO LÍQUIDO ==========
    patrimonio_liquido <- patrimonio_apos_come_cotas - ir_ordinario_valor - custo_op_semestral
    resultado$patrimonio_liquido[i] <- patrimonio_liquido
    
    # ========== PASSO 6: AJUSTAR PELA INFLAÇÃO ==========
    # Patrimônio real (poder de compra)
    inflacao_semestral <- sqrt(1 + inflacao_anual) - 1
    patrimonio_real <- patrimonio_liquido / ((1 + inflacao_semestral) ^ (i - 1))
    resultado$patrimonio_real[i] <- patrimonio_real
    
    # ========== PASSO 7: CALCULAR RETORNO ACUMULADO ==========
    retorno_acumulado <- ((patrimonio_liquido / patrimonio_inicial) - 1) * 100
    resultado$retorno_acumulado_pct[i] <- retorno_acumulado
  }
  
  return(resultado)
}

# ============================================================================
# 4. EXECUTAR SIMULAÇÃO PARA CADA ESTRUTURA
# ============================================================================

resultados_todas_estruturas <- list()

for (j in 1:nrow(estruturas)) {
  
  resultado <- simular_progressao_corrigida(
    patrimonio_inicial = patrimonio_inicial,
    retorno_semestral = retorno_semestral,
    retorno_anual = retorno_anual,
    inflacao_anual = inflacao_anual,
    come_cotas_aliquota = estruturas$come_cotas_aliquota[j],
    itcmd_aliquota = estruturas$itcmd_aliquota[j],
    ir_ordinario = estruturas$ir_ordinario[j],
    custo_operacional = estruturas$custo_operacional[j],
    semestres = semestres
  )
  
  # Adicionar coluna com nome da estrutura
  resultado$estrutura <- estruturas$estrutura[j]
  
  # Armazenar na lista
  resultados_todas_estruturas[[j]] <- resultado
}

# Combinar todos os resultados em um único data frame
resultados_completos <- bind_rows(resultados_todas_estruturas)

# ============================================================================
# 5. EXEMPLO: DEMONSTRAR CÁLCULO CORRETO (FUNDOS MULTIMERCADO - PRIMEIROS 2 SEMESTRES)
# ============================================================================

cat("\n=== DEMONSTRAÇÃO DO CÁLCULO CORRETO (FUNDOS MULTIMERCADO) ===\n")
cat("Patrimônio inicial: R$ 5.000.000,00\n")
cat("Retorno semestral: ", round(retorno_semestral * 100, 4), "%\n")
cat("Come-cotas alíquota: 15% (APENAS sobre o lucro)\n\n")

demo <- resultados_completos %>%
  filter(estrutura == "Fundos Multimercado", semestre <= 2) %>%
  select(semestre, patrimonio_inicial_periodo, lucro_semestral, come_cotas, 
         patrimonio_apos_come_cotas, imposto_ir, patrimonio_liquido)

cat("Semestre 0 (Inicial):\n")
cat("  Patrimônio inicial: R$ 5.000.000,00\n\n")

cat("Semestre 1:\n")
cat("  Patrimônio inicial do semestre: R$", format(demo$patrimonio_inicial_periodo[2], big.mark=".", decimal.mark=",", nsmall=0), "\n")
cat("  Lucro do semestre: R$", format(demo$lucro_semestral[2], big.mark=".", decimal.mark=",", nsmall=0), "\n")
cat("  Come-cotas (15% × lucro): R$", format(demo$come_cotas[2], big.mark=".", decimal.mark=",", nsmall=0), "\n")
cat("  Patrimônio após come-cotas: R$", format(demo$patrimonio_apos_come_cotas[2], big.mark=".", decimal.mark=",", nsmall=0), "\n")
cat("  IR ordinário: R$", format(demo$imposto_ir[2], big.mark=".", decimal.mark=",", nsmall=0), "\n")
cat("  Patrimônio líquido final: R$", format(demo$patrimonio_liquido[2], big.mark=".", decimal.mark=",", nsmall=0), "\n\n")

# ============================================================================
# 6. EXIBIR RESULTADOS PARA FUNDOS MULTIMERCADO (ANUAL)
# ============================================================================

cat("=== PROGRESSÃO PATRIMONIAL - FUNDOS MULTIMERCADO (ANUAL) ===\n")
resultado_multimercado <- resultados_completos %>%
  filter(estrutura == "Fundos Multimercado", semestre %% 2 == 0) %>%  # Apenas anos completos
  select(ano, patrimonio_inicial_periodo, lucro_semestral, come_cotas, 
         patrimonio_liquido, patrimonio_real) %>%
  mutate(
    patrimonio_inicial_periodo = format(patrimonio_inicial_periodo, big.mark = ".", decimal.mark = ",", nsmall = 0),
    lucro_semestral = format(lucro_semestral, big.mark = ".", decimal.mark = ",", nsmall = 0),
    come_cotas = format(come_cotas, big.mark = ".", decimal.mark = ",", nsmall = 0),
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", nsmall = 0),
    patrimonio_real = format(patrimonio_real, big.mark = ".", decimal.mark = ",", nsmall = 0)
  ) %>%
  rename(
    "Ano" = ano,
    "Patrimônio Inicial (R$)" = patrimonio_inicial_periodo,
    "Lucro Semestral (R$)" = lucro_semestral,
    "Come-cotas (R$)" = come_cotas,
    "Patrimônio Líquido (R$)" = patrimonio_liquido,
    "Patrimônio Real (R$)" = patrimonio_real
  )

print(resultado_multimercado)

# ============================================================================
# 7. GRÁFICO: EVOLUÇÃO DO PATRIMÔNIO LÍQUIDO (5 ANOS)
# ============================================================================

# Filtrar apenas anos completos para o gráfico
dados_grafico <- resultados_completos %>%
  filter(semestre %% 2 == 0)  # Apenas anos completos

grafico_evolucao <- dados_grafico %>%
  ggplot(aes(x = ano, y = patrimonio_liquido / 1000000, color = estrutura, group = estrutura)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  labs(
    title = "Evolução do Patrimônio Líquido por Estrutura (5 Anos)",
    subtitle = "Patrimônio inicial: R$ 5M | Retorno: 8% a.a. | Come-cotas: 15% sobre LUCRO (não total)",
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

print(grafico_evolucao)

# ============================================================================
# 8. RESUMO FINAL (ANO 5)
# ============================================================================

cat("\n=== RESUMO FINAL - ANO 5 (PATRIMÔNIO LÍQUIDO) ===\n")
resumo_ano5 <- resultados_completos %>%
  filter(semestre == semestres) %>%  # Apenas final do ano 5
  select(estrutura, patrimonio_liquido, patrimonio_real, retorno_acumulado_pct) %>%
  arrange(desc(patrimonio_liquido)) %>%
  mutate(
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", nsmall = 0),
    patrimonio_real = format(patrimonio_real, big.mark = ".", decimal.mark = ",", nsmall = 0),
    retorno_acumulado_pct = paste0(round(retorno_acumulado_pct, 2), "%")
  ) %>%
  rename(
    "Estrutura" = estrutura,
    "Patrimônio Líquido (R$)" = patrimonio_liquido,
    "Patrimônio Real (R$)" = patrimonio_real,
    "Retorno Acumulado (%)" = retorno_acumulado_pct
  )

print(resumo_ano5)

# ============================================================================
# FIM DO SCRIPT 1 CORRIGIDO
# ============================================================================