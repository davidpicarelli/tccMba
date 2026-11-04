library(tidyverse)
library(ggplot2)

# Parameters
initial_capital <- 1e7
gross_return <- 0.08
tax_rate <- 0.15
horizon <- 5

# Structure definitions: (name, cost_rate, tax_deferred)
structures <- tribble(
  ~name, ~cost_rate, ~tax_deferred,
  "Contas Diretas", 0.005, FALSE,
  "Fundo Exclusivo", 0.010, FALSE,
  "Fundos Multimercado", 0.020, FALSE,
  "LLC", 0.008, FALSE,
  "Seguros Unit-Linked", 0.015, TRUE,
  "Trusts", 0.010, FALSE
)

# Simulation
simulate <- function(name, cost_rate, tax_deferred) {
  capital <- initial_capital
  results <- tibble(Year = 0, Capital = capital, Structure = name)
  for (year in 1:horizon) {
    gross <- capital * gross_return
    costs <- capital * cost_rate
    net_income <- gross - costs
    if (!tax_deferred) {
      tax <- net_income * tax_rate
      net_income <- net_income - tax
    }
    capital <- capital + net_income
    results <- bind_rows(results, tibble(Year = year, Capital = capital, Structure = name))
  }
  results
}

# Apply simulation to all structures
results <- structures %>%
  pmap_dfr(simulate)

# For Unit-Linked, apply final tax on total gain at end of 5 years
final_year <- results %>% filter(Year == horizon)
unit_linked <- final_year %>% filter(Structure == "Seguros Unit-Linked")
total_gain <- unit_linked$Capital - initial_capital
final_after_tax <- initial_capital + total_gain * (1 - tax_rate)
results <- results %>%
  mutate(Capital = ifelse(Structure == "Seguros Unit-Linked" & Year == horizon, final_after_tax, Capital))

# Pivot wider for table
table_data <- results %>%
  pivot_wider(names_from = Structure, values_from = Capital)

# Print table (using knitr::kable if available, else just print)
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::kable(table_data, digits = 0, format = "markdown", caption = "Wealth Progression (R$) After Tax and Costs")
} else {
  print(table_data)
}

# Elegant line plot
p <- results %>%
  ggplot(aes(x = Year, y = Capital, color = Structure, linetype = Structure)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Wealth Progression of Offshore Structures (5 Years)",
    subtitle = "Initial Capital: R$ 10,000,000 | Gross Return: 8% p.a. | Tax: 15% per Lei 14.754/2023",
    x = "Year",
    y = "Capital (R$)",
    color = "Structure",
    linetype = "Structure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)