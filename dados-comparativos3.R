library(tidyverse)
library(ggplot2)
library(RColorBrewer)

set.seed(42)

# Estruturas
estruturas <- tibble(
  nome = c("Contas internacionais diretas", "Fundo Exclusivo", "Fundos Multimercado",
           "LLC Americana", "Seguros Unit-Linked", "Trusts"),
  capital_inicial = c(500000, 1000000, 100000, 2000000, 500000, 5000000),
  retorno_medio = c(0.04, 0.07, 0.05, 0.04, 0.04, 0.06),
  custo_anual = c(0.005, 0.015, 0.02, 0.008, 0.015, 0.01),
  ir_anual = c(0.15, 0.15, 0.15, 0.15, 0, 0.15),
  ir_final = c(0, 0, 0, 0, 0.15, 0)
)

# Simulação
anos <- 0:5
n_est <- nrow(estruturas)
capital <- matrix(NA, nrow = n_est, ncol = length(anos))
capital[, 1] <- estruturas$capital_inicial

for (i in 1:n_est) {
  for (t in 2:length(anos)) {
    retorno <- rnorm(1, mean = estruturas$retorno_medio[i], sd = 0.02)
    custo <- estruturas$custo_anual[i]
    ir_anual <- estruturas$ir_anual[i]
    ir_final <- estruturas$ir_final[i]
    
    # Aplicar retorno e custo
    cap_bruto <- capital[i, t-1] * (1 + retorno - custo)
    
    if (t == length(anos) && ir_final > 0) {
      ganho_acum <- cap_bruto - estruturas$capital_inicial[i]
      ir_pago <- ganho_acum * ir_final
      cap_liquido <- cap_bruto - ir_pago
    } else {
      ir_pago_anual <- (cap_bruto - capital[i, t-1]) * ir_anual
      cap_liquido <- cap_bruto - ir_pago_anual
    }
    capital[i, t] <- cap_liquido
  }
}

# Tabela detalhada
colnames(capital) <- paste0("Ano ", anos)
tabela_detalhada <- bind_cols(Estrutura = estruturas$nome, as_tibble(capital))
print("Tabela Detalhada (valores em R$):")
print(tabela_detalhada)

# Tabela resumida
capital_final <- capital[, ncol(capital)]
ganho_liquido <- capital_final - estruturas$capital_inicial
rentabilidade <- ganho_liquido / estruturas$capital_inicial * 100
tabela_resumida <- tibble(
  Estrutura = estruturas$nome,
  Capital_Inicial = estruturas$capital_inicial,
  Capital_Final = capital_final,
  Ganho_Liquido = ganho_liquido,
  Rentabilidade_perc = rentabilidade
)
print("\nTabela Resumida:")
print(tabela_resumida)

# Decomposição AHP
# Peso de cada critério
peso_sucessorio <- 0.4
peso_tributario <- 0.35
peso_operacional <- 0.15
peso_regulatorio <- 0.10

# Score sucessório (0-1)
score_sucessorio <- c(0.6, 0.5, 0.4, 0.7, 0.7, 0.9)  # trust maior, llc e unit medio

# Score tributário: baseado na taxa efetiva de IR (quanto menor, melhor)
ir_total_pago <- numeric(n_est)
ganho_bruto <- numeric(n_est)
for (i in 1:n_est) {
  ganho_bruto[i] <- capital[i, ncol(capital)] - estruturas$capital_inicial[i]
  # Calcular IR pago total (aproximação: IR anual + final)
  # Para simplificar, calcular IR efetivo sobre ganho bruto
  # Unit-Linked: IR final, outros: IR anual
  if (estruturas$ir_anual[i] > 0) {
    # IR pago anualmente, mas não temos o valor exato; aproximar pela taxa média
    ir_pago_aprox <- ganho_bruto[i] * estruturas$ir_anual[i]
  } else {
    ir_pago_aprox <- ganho_bruto[i] * estruturas$ir_final[i]
  }
  ir_total_pago[i] <- ir_pago_aprox
}
taxa_ir_efetiva <- ifelse(ganho_bruto > 0, ir_total_pago / ganho_bruto, 0)
score_tributario <- 1 - taxa_ir_efetiva  # quanto menor IR efetivo, maior score

# Score operacional: baseado nos custos anuais (menor custo = maior score)
taxa_custo_efetiva <- estruturas$custo_anual
score_operacional <- 1 - taxa_custo_efetiva  # quanto menor custo, maior score

# Score regulatório: fixo 0.8 para todas
score_regulatorio <- rep(0.8, n_est)

# Cálculo dos impactos
impacto_sucessorio <- score_sucessorio * peso_sucessorio
impacto_tributario <- score_tributario * peso_tributario
impacto_operacional <- score_operacional * peso_operacional
impacto_regulatorio <- score_regulatorio * peso_regulatorio

score_ahp_simulado <- impacto_sucessorio + impacto_tributario + impacto_operacional + impacto_regulatorio

tabela_ahp <- tibble(
  Estrutura = estruturas$nome,
  Impacto_Sucessorio = impacto_sucessorio,
  Impacto_Tributario = impacto_tributario,
  Impacto_Operacional = impacto_operacional,
  Impacto_Regulatorio = impacto_regulatorio,
  Score_AHP_Simulado = score_ahp_simulado
)
print("\nTabela de Decomposição AHP:")
print(tabela_ahp)

# Gráfico 1: evolução patrimonial
df_evol <- tabela_detalhada %>%
  pivot_longer(cols = starts_with("Ano"), names_to = "Ano", values_to = "Capital") %>%
  mutate(Ano = as.numeric(str_extract(Ano, "\\d+")))

ggplot(df_evol, aes(x = Ano, y = Capital, color = Estrutura)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", scale = 1e-6, suffix = "M")) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Evolução Patrimonial - Simulação 5 Anos (Lei nº 14.754/2023)",
       x = "Ano", y = "Capital (R$ milhões)",
       color = "Estrutura") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico 2: barras empilhadas AHP
df_ahp <- tabela_ahp %>%
  pivot_longer(cols = starts_with("Impacto"), names_to = "Criterio", values_to = "Impacto") %>%
  mutate(Criterio = factor(Criterio, levels = c("Impacto_Sucessorio", "Impacto_Tributario", "Impacto_Operacional", "Impacto_Regulatorio")))

ggplot(df_ahp, aes(x = Estrutura, y = Impacto, fill = Criterio)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Contribuição de cada Critério AHP para o Desempenho Final",
       x = "Estrutura", y = "Impacto",
       fill = "Critério") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Resumo
melhor_estrutura <- tabela_ahp$Estrutura[which.max(tabela_ahp$Score_AHP_Simulado)]
cat("\nResumo:\n")
cat(paste("Estrutura que melhor valida o AHP:", melhor_estrutura, "\n"))
# Critério que mais impactou (maior impacto médio)
impactos_medios <- c(mean(impacto_sucessorio), mean(impacto_tributario), mean(impacto_operacional), mean(impacto_regulatorio))
nomes_criterios <- c("Sucessório", "Tributário", "Operacional", "Regulatório")
cat(paste("Critério que mais impactou o resultado:", nomes_criterios[which.max(impactos_medios)], "\n"))
