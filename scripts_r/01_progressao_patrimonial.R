# ============================================================================
# SCRIPT 1: SIMULADOR DE PROGRESSÃO PATRIMONIAL - 5 ANOS
# Análise de crescimento patrimonial sob diferentes estruturas offshore
# TCC: Análise Comparativa de Estruturas de Investimento Offshore
# ============================================================================

# Carregar bibliotecas necessárias
library(tidyverse)
library(ggplot2)

# ============================================================================
# 1. DEFINIR PARÂMETROS DE SIMULAÇÃO
# ============================================================================

# Patrimônio inicial (em reais)
patrimonio_inicial <- 5000000  # R$ 5 milhões (faixa R$ 1-10M)

# Taxa de retorno anual esperado (%)
retorno_anual <- 0.08  # 8% a.a.

# Taxa de inflação anual (%)
inflacao_anual <- 0.04  # 4% a.a.

# Período de análise (anos)
anos <- 5

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
  
  # Come-cotas semestral (% ao semestre para fundos brasileiros)
  come_cotas_semestral = c(0, 0.15, 0.15, 0, 0, 0),
  
  # ITCMD (Imposto sobre Transmissão Causa Mortis e Doação) - alíquota média
  itcmd_aliquota = c(0.04, 0.04, 0.04, 0.02, 0.02, 0.04),
  
  # Imposto de renda ordinário (alíquota progressiva média)
  ir_ordinario = c(0.275, 0.15, 0.15, 0.15, 0.10, 0.275),
  
  # Custo operacional anual (% do patrimônio)
  custo_operacional = c(0.005, 0.010, 0.008, 0.015, 0.012, 0.020),
  
  stringsAsFactors = FALSE
)

print("Estruturas e características tributárias:")
print(estruturas)

# ============================================================================
# 3. FUNÇÃO PARA SIMULAR PROGRESSÃO PATRIMONIAL
# ============================================================================

simular_progressao <- function(patrimonio_inicial, 
                               retorno_anual, 
                               inflacao_anual,
                               come_cotas_semestral,
                               itcmd_aliquota,
                               ir_ordinario,
                               custo_operacional,
                               anos) {
  
  # Criar data frame para armazenar resultados
  resultado <- data.frame(
    ano = 0:anos,
    patrimonio_bruto = NA,
    imposto_come_cotas = NA,
    imposto_ir = NA,
    custo_operacional_valor = NA,
    patrimonio_liquido = NA,
    patrimonio_real = NA,  # Ajustado por inflação
    retorno_acumulado_pct = NA
  )
  
  # Ano 0 (inicial)
  resultado$patrimonio_bruto[1] <- patrimonio_inicial
  resultado$imposto_come_cotas[1] <- 0
  resultado$imposto_ir[1] <- 0
  resultado$custo_operacional_valor[1] <- 0
  resultado$patrimonio_liquido[1] <- patrimonio_inicial
  resultado$patrimonio_real[1] <- patrimonio_inicial
  resultado$retorno_acumulado_pct[1] <- 0
  
  # Simulação para cada ano
  for (i in 2:(anos + 1)) {
    
    # Patrimônio anterior
    patrimonio_anterior <- resultado$patrimonio_liquido[i - 1]
    
    # Retorno bruto do período
    retorno_bruto <- patrimonio_anterior * retorno_anual
    
    # Come-cotas (aplicado 2x ao ano - semestral)
    # Simplificação: aplicar taxa semestral 2x
    come_cotas <- patrimonio_anterior * come_cotas_semestral * 2
    
    # Patrimônio após come-cotas
    patrimonio_apos_come_cotas <- patrimonio_anterior + retorno_bruto - come_cotas
    
    # IR ordinário (sobre ganhos de capital)
    ir_ordinario_valor <- retorno_bruto * ir_ordinario
    
    # Custo operacional
    custo_op <- patrimonio_anterior * custo_operacional
    
    # Patrimônio líquido (após todos os descontos)
    patrimonio_liquido <- patrimonio_apos_come_cotas - ir_ordinario_valor - custo_op
    
    # Patrimônio ajustado por inflação (poder de compra)
    patrimonio_real <- patrimonio_liquido / ((1 + inflacao_anual) ^ (i - 1))
    
    # Retorno acumulado (%)
    retorno_acumulado <- ((patrimonio_liquido / patrimonio_inicial) - 1) * 100
    
    # Armazenar resultados
    resultado$patrimonio_bruto[i] <- patrimonio_apos_come_cotas
    resultado$imposto_come_cotas[i] <- come_cotas
    resultado$imposto_ir[i] <- ir_ordinario_valor
    resultado$custo_operacional_valor[i] <- custo_op
    resultado$patrimonio_liquido[i] <- patrimonio_liquido
    resultado$patrimonio_real[i] <- patrimonio_real
    resultado$retorno_acumulado_pct[i] <- retorno_acumulado
  }
  
  return(resultado)
}

# ============================================================================
# 4. EXECUTAR SIMULAÇÃO PARA CADA ESTRUTURA
# ============================================================================

# Lista para armazenar resultados de todas as estruturas
resultados_todas_estruturas <- list()

# Simular para cada estrutura
for (j in 1:nrow(estruturas)) {
  
  resultado <- simular_progressao(
    patrimonio_inicial = patrimonio_inicial,
    retorno_anual = retorno_anual,
    inflacao_anual = inflacao_anual,
    come_cotas_semestral = estruturas$come_cotas_semestral[j],
    itcmd_aliquota = estruturas$itcmd_aliquota[j],
    ir_ordinario = estruturas$ir_ordinario[j],
    custo_operacional = estruturas$custo_operacional[j],
    anos = anos
  )
  
  # Adicionar coluna com nome da estrutura
  resultado$estrutura <- estruturas$estrutura[j]
  
  # Armazenar na lista
  resultados_todas_estruturas[[j]] <- resultado
}

# Combinar todos os resultados em um único data frame
resultados_completos <- bind_rows(resultados_todas_estruturas)

# ============================================================================
# 5. EXIBIR RESULTADOS PARA FUNDO MULTIMERCADO (exemplo)
# ============================================================================

cat("\n=== PROGRESSÃO PATRIMONIAL - FUNDOS MULTIMERCADO (5 ANOS) ===\n")
resultado_multimercado <- resultados_completos %>%
  filter(estrutura == "Fundos Multimercado") %>%
  select(ano, patrimonio_bruto, imposto_come_cotas, imposto_ir, 
         patrimonio_liquido, patrimonio_real) %>%
  mutate(
    patrimonio_bruto = format(patrimonio_bruto, big.mark = ".", decimal.mark = ",", 
                              nsmall = 0),
    imposto_come_cotas = format(imposto_come_cotas, big.mark = ".", decimal.mark = ",", 
                                nsmall = 0),
    imposto_ir = format(imposto_ir, big.mark = ".", decimal.mark = ",", nsmall = 0),
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", 
                                nsmall = 0),
    patrimonio_real = format(patrimonio_real, big.mark = ".", decimal.mark = ",", 
                             nsmall = 0)
  )

print(resultado_multimercado)

# ============================================================================
# 6. GRÁFICO: EVOLUÇÃO DO PATRIMÔNIO LÍQUIDO (5 ANOS)
# ============================================================================

grafico_evolucao <- resultados_completos %>%
  ggplot(aes(x = ano, y = patrimonio_liquido / 1000000, color = estrutura, group = estrutura)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", suffix = "M")) +
  labs(
    title = "Evolução do Patrimônio Líquido por Estrutura (5 Anos)",
    subtitle = "Patrimônio inicial: R$ 5 milhões | Retorno anual: 8% | Inflação: 4%",
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
# 7. TABELA RESUMIDA: COMPARATIVO FINAL (ANO 5)
# ============================================================================

cat("\n=== RESUMO FINAL - ANO 5 (PATRIMÔNIO LÍQUIDO) ===\n")
resumo_ano5 <- resultados_completos %>%
  filter(ano == 5) %>%
  select(estrutura, patrimonio_liquido, patrimonio_real, retorno_acumulado_pct) %>%
  arrange(desc(patrimonio_liquido)) %>%
  mutate(
    patrimonio_liquido = format(patrimonio_liquido, big.mark = ".", decimal.mark = ",", 
                                nsmall = 0),
    patrimonio_real = format(patrimonio_real, big.mark = ".", decimal.mark = ",", 
                             nsmall = 0),
    retorno_acumulado_pct = paste0(round(retorno_acumulado_pct, 2), "%")
  )

print(resumo_ano5)

# ============================================================================
# FIM DO SCRIPT 1
# ============================================================================