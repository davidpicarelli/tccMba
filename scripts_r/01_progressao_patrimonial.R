# ============================================================================
# SCRIPT 1: SIMULADOR DE PROGRESSÃO PATRIMONIAL - 5 ANOS (10 SEMESTRES)
# COME-COTAS CALCULADO APENAS SOBRE O LUCRO SEMESTRAL (SEM DUPLICAÇÃO DE IR)
# ============================================================================

library(tidyverse)
library(ggplot2)

# ============================================================================
# 1. DEFINIR PARÂMETROS DE SIMULAÇÃO
# ============================================================================

patrimonio_inicial <- 5000000  # R$ 5 milhões
retorno_anual <- 0.08          # 8% a.a.
retorno_semestral <- sqrt(1 + retorno_anual) - 1  # 3,923048% a.s.
inflacao_anual <- 0.04
anos <- 5
semestres <- anos * 2

# ============================================================================
# 2. DEFINIR CARACTERÍSTICAS TRIBUTÁRIAS POR ESTRUTURA (SEM DUPLICAÇÃO)
# ============================================================================
# Nota Metodológica: Nos Fundos (Exclusivos e Multimercado), a alíquota de 15%
# de longo prazo é recolhida semestralmente via come-cotas (ir_ordinario = 0.00),
# eliminando qualquer dupla contagem tributária.

estruturas <- data.frame(
  estrutura = c(
    "Contas Diretas",
    "Fundos Exclusivos",
    "Fundos Multimercado",
    "Estruturas Corporativas (LLC/IBC)",
    "Seguros Unit-Linked",
    "Trusts Fiduciários"
  ),
  
  # Come-cotas semestral sobre o lucro (15% apenas para fundos domésticos)
  come_cotas_aliquota = c(0.00, 0.15, 0.15, 0.00, 0.00, 0.00),
  
  # ITCMD referencial
  itcmd_aliquota = c(0.04, 0.04, 0.04, 0.02, 0.02, 0.04),
  
  # Imposto de renda ordinário (0% nos fundos pois já tributam 15% via come-cotas)
  ir_ordinario = c(0.275, 0.00, 0.00, 0.15, 0.10, 0.275),
  
  # Custo operacional anual total (taxa adm + gestão + feeder/master + custódia/auditoria)
  # Obs: Se desejar usar as taxas de 1,0% e 0,8% sem custo de feeder externo,
  # altere os dois valores centrais (0.020 e 0.018) para 0.010 e 0.008.
  custo_operacional = c(0.005, 0.020, 0.018, 0.015, 0.012, 0.020),
  
  stringsAsFactors = FALSE
)

cat("=== ESTRUTURAS E CARACTERÍSTICAS TRIBUTÁRIAS ===\n")
print(estruturas)

# ============================================================================
# 3. FUNÇÃO PARA SIMULAR PROGRESSÃO PATRIMONIAL
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
  
  resultado <- data.frame(
    semestre = 0:semestres,
    ano = 0:semestres / 2,
    patrimonio_inicial_periodo = NA,
    lucro_semestral = NA,
    come_cotas = NA,
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
    
    patrimonio_anterior <- resultado$patrimonio_liquido[i - 1]
    resultado$patrimonio_inicial_periodo[i] <- patrimonio_anterior
    
    # Passo 1: Lucro bruto do semestre
    lucro_semestral <- patrimonio_anterior * retorno_semestral
    resultado$lucro_semestral[i] <- lucro_semestral
    patrimonio_apos_retorno <- patrimonio_anterior + lucro_semestral
    
    # Passo 2: Come-cotas (exclusivo para fundos)
    come_cotas <- lucro_semestral * come_cotas_aliquota
    resultado$come_cotas[i] <- come_cotas
    patrimonio_apos_come_cotas <- patrimonio_apos_retorno - come_cotas
    resultado$patrimonio_apos_come_cotas[i] <- patrimonio_apos_come_cotas
    
    # Passo 3: IR Ordinário (apenas para veículos sem come-cotas)
    ir_ordinario_valor <- lucro_semestral * ir_ordinario
    resultado$imposto_ir[i] <- ir_ordinario_valor
    
    # Passo 4: Custo operacional semestral
    custo_op_semestral <- patrimonio_anterior * (custo_operacional / 2)
    resultado$custo_operacional_valor[i] <- custo_op_semestral
    
    # Passo 5: Patrimônio líquido ao final do semestre
    patrimonio_liquido <- patrimonio_apos_come_cotas - ir_ordinario_valor - custo_op_semestral
    resultado$patrimonio_liquido[i] <- patrimonio_liquido
    
    # Passo 6: Ajuste pela inflação projetada (poder de compra real)
    inflacao_semestral <- sqrt(1 + inflacao_anual) - 1
    patrimonio_real <- patrimonio_liquido / ((1 + inflacao_semestral) ^ (i - 1))
    resultado$patrimonio_real[i] <- patrimonio_real
    
    # Passo 7: Retorno líquido acumulado (%)
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
  resultado$estrutura <- estruturas$estrutura[j]
  resultados_todas_estruturas[[j]] <- resultado
}

resultados_completos <- bind_rows(resultados_todas_estruturas)

# ============================================================================
# 5. RESUMO FINAL (ANO 5)
# ============================================================================

cat("\n=== RESUMO FINAL - ANO 5 (PATRIMÔNIO LÍQUIDO) ===\n")
resumo_ano5 <- resultados_completos %>%
  filter(semestre == semestres) %>%
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