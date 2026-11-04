library(tidyverse)
library(knitr)

# Parâmetros iniciais
capital_inicial <- 10000000
anos <- 5

# Definição das estruturas
estruturas <- tibble(
  nome = c(
    "Contas internacionais diretas",
    "Fundo Exclusivo de Investimento no Exterior",
    "Fundos multimercado com investimento internacional",
    "Estruturas societárias offshore (LLC Americana)",
    "Seguros internacionais (Unit-Linked)",
    "Trusts fiduciários"
  ),
  retorno = c(0.04, 0.07, 0.05, 0.04, 0.04, 0.06),
  custo = c(0.005, 0.015, 0.02, 0.008, 0.015, 0.01),
  ir_anual = c(0.15, 0.15, 0.15, 0.15, 0, 0.15),
  stringsAsFactors = FALSE
)

# Função para simular uma estrutura
evoluir_estrutura <- function(retorno, custo, ir_anual, capital_inicial, anos) {
  capital <- capital_inicial
  resultados <- vector("list", anos)
  for (ano in 1:anos) {
    rend_bruto <- capital * retorno
    custos <- capital * custo
    base_ir <- rend_bruto - custos
    ir_devido <- max(0, base_ir * ir_anual)
    rend_liquido <- base_ir - ir_devido
    capital <- capital + rend_liquido
    if (ir_anual == 0) {
      # Unit-Linked: diferimento total, IR só no último ano sobre ganho total
      if (ano == anos) {
        ganho_total <- capital - capital_inicial
        ir_devido_final <- ganho_total * 0.15
        capital <- capital - ir_devido_final
        rend_liquido <- rend_liquido - ir_devido_final
      }
    }
    resultados[[ano]] <- tibble(
      Ano = ano,
      Capital_Inicial = ifelse(ano == 1, capital_inicial, lag(capital, default = NA)),
      Rendimento_Bruto = rend_bruto,
      Custos = custos,
      Base_IR = base_ir,
      IR_Devido = ifelse(ir_anual == 0 & ano < anos, 0, ir_devido),
      Rendimento_Liquido = rend_liquido,
      Capital_Final = capital
    )
  }
  bind_rows(resultados)
}

# Simular todas as estruturas
resultados_geral <- estruturas %>%
  mutate(
    dados = pmap(list(retorno, custo, ir_anual), ~ evoluir_estrutura(..1, ..2, ..3, capital_inicial, anos))
  ) %>%
  unnest(dados) %>%
  select(Estrutura = nome, everything(), -retorno, -custo, -ir_anual)

# Tabela formatada com kable
resultados_geral %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  kable(
    caption = "Evolução Patrimonial por Estrutura (R$)",
    digits = 2,
    format = "pipe"
  ) %>%
  print()

# Gráfico ggplot2
resultados_geral %>%
  mutate(
    Capital_Final_Milhao = Capital_Final / 1e6
  ) %>%
  ggplot(aes(x = Ano, y = Capital_Final_Milhao, color = Estrutura)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar_format(suffix = " M", prefix = "R$ ")) +
  labs(
    title = "Evolução Patrimonial (R$ Milhões)",
    x = "Ano",
    y = "Capital Final (R$ milhões)",
    color = "Estrutura"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")