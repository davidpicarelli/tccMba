set.seed(123)

# Capital inicial
capital_inicial <- 10e6

# Estruturas e parâmetros de retorno (média e desvio padrão)
estruturas <- data.frame(
  nome = c("Contas diretas", "Fundo Exclusivo", "Fundos Multimercado",
           "LLC Americana", "Seguros Unit-Linked", "Trusts"),
  media = c(0.04, 0.07, 0.05, 0.04, 0.04, 0.06),
  sd = c(0.015, 0.025, 0.020, 0.018, 0.012, 0.022),
  stringsAsFactors = FALSE
)

# Custos operacionais anuais (percentual sobre o valor aplicado)
custos <- c(0.005, 0.008, 0.010, 0.003, 0.006, 0.004)
names(custos) <- estruturas$nome

# Alocação inicial (proporções)
alocacao <- c(0.10, 0.10, 0.10, 0.10, 0.10, 0.10)
names(alocacao) <- estruturas$nome

# Número de anos
anos <- 5

# Matriz para armazenar valores
valores <- matrix(NA, nrow = anos + 1, ncol = length(estruturas$nome))
colnames(valores) <- estruturas$nome
rownames(valores) <- 0:anos

# Ano 0: capital inicial sem rendimento
valores[1, ] <- capital_inicial * alocacao

# Simulação para anos 1 a 5
for (ano in 1:anos) {
  for (i in seq_along(estruturas$nome)) {
    nome_estr <- estruturas$nome[i]
    # Retorno aleatório com distribuição normal
    retorno <- rnorm(1, mean = estruturas$media[i], sd = estruturas$sd[i])
    # Valor anterior
    v_anterior <- valores[ano, i]
    # Novo valor com retorno e custo
    v_novo <- v_anterior * (1 + retorno) * (1 - custos[i])
    # Tratamento especial para Unit-Linked: IR somente no ano 5
    if (ano == 5 && nome_estr == "Seguros Unit-Linked") {
      ganho_acum <- v_novo - (capital_inicial * alocacao["Seguros Unit-Linked"])
      ir <- ifelse(ganho_acum > 0, ganho_acum * 0.15, 0)
      v_novo <- v_novo - ir
    }
    valores[ano + 1, i] <- v_novo
  }
}

# Tabela com total por ano
tabela <- data.frame(
  Ano = 0:anos,
  valores,
  Total = rowSums(valores)
)

print(tabela)

# Preparar dados para ggplot2
library(tidyr)
library(dplyr)
library(ggplot2)

df_long <- tabela %>%
  select(-Total) %>%
  pivot_longer(-Ano, names_to = "Estrutura", values_to = "Valor") %>%
  mutate(Ano = as.factor(Ano))

ggplot(df_long, aes(x = Ano, y = Valor / 1e6, color = Estrutura, group = Estrutura)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "Evolução Patrimonial por Estrutura",
    x = "Ano",
    y = "Valor (R$ milhões)",
    color = "Estrutura"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")