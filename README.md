# Análise de Progressão Patrimonial e Eficiência Fiscal de Estruturas
Offshore (5 Anos)

Este é o meu repositório do Trabalho de Conclusão de Curso (TCC) do 
MBA Executivo em Contabilidade e Finanças Corporativas da [Unifatecie]
(https://www.unifatecie.edu.br/).

Utilizo este repositório para organizar os arquivos relacionados à análise 
de progressão patrimonial, como scripts R, dados simulados, gráficos e tabelas.
Além disso, mantenho um diário de aprendizagem para registrar o que aprendi, as
dificuldades que tive e o que preciso revisar.

---

## Como usar este projeto

Devo abrir este projeto sempre pelo arquivo `.Rproj` (se estiver usando RStudio)
e manter os arquivos organizados nas pastas corretas.

Passo 1: Instale os pacotes necessários

install.packages(c("tidyverse", "ggplot2", "flextable")

Passo 2: Execute o script mastersource("master_analise_completa.R")

Passo 3: Verifique as saídas nas pastas graficos/ e tabelas/
Estrutura básica
projeto_tcc/
├── README.md                          # Este arquivo
├── tccMba.Rproj                  # Projeto RStudio
├── master_analise_completa.R          # Script orquestrador
├── scripts_r/                         # Scripts modulares
│   ├── 01_progressao_patrimonial.R    # Simulação de crescimento (5 anos)
│   ├── 02_analise_fiscal.R            # Cálculo de impostos acumulados
│   ├── 03_recomendacao_dinamica.R     # Cruzamento com AHP
│   ├── 04_exportar_tabelas.R          # Exporta .docx formatados
│   └── 05_exportar_graficos.R         # Exporta PNG/PDF em alta resolução
├── dados/                             # Dados processados
│   └── raw/                           # Dados brutos (se houver)
├── graficos/                          # Visualizações geradas
│   ├── 01_evolucao_patrimonio.png
│   ├── 02_custo_fiscal_acumulado.png
│   ├── 03_composicao_custo_fiscal.png
│   ├── 04_patrimonio_vs_custo.png
│   ├── 05_evolucao_recomendacao.png
│   └── 06_scores_finais.png
└── tabelas/                           # Tabelas para Word
    ├── 01_progressao_patrimonial.docx
    ├── 02_analise_fiscal.docx
    ├── 03_recomendacao_dinamica.docx
    └── 04_scores_combinados.docx
    
Descrição das pastas
scripts_r/: Contém os 5 scripts R que executam a análise
dados/: Armazena dados processados e outputs intermediários
graficos/: Repositório das visualizações (PNG para Word, PDF para impressão)
tabelas/: Repositório dos documentos Word com tabelas formatadas

 Descrição dos Scripts
 Script Função Entrada Saída
 01_progressao_patrimonial.R Simula evolução do patrimônio (5 anos)
 Patrimônio inicial, retorno, inflação Tabelas + Gráfico
 02_analise_fiscal.R Calcula custo tributário acumulado Dados do Script 1
 Tabelas + Gráficos
 03_recomendacao_dinamica.R Integra AHP + análise fiscalDados
 dos Scripts 1 e 2 Tabelas + Gráficos
 04_exportar_tabelas.R Exporta tabelas para Word (.docx)
 Dados dos Scripts 1, 2 e 3 Arquivos .docx
 05_exportar_graficos.R Exporta gráficos em alta resolução
 Gráficos dos Scripts 1, 2 e 3PNG (300 DPI) + PDF
 
 Contexto do Projeto (TCC)Estruturas Analisadas
Contas Diretas — Investimento direto no exterior
Fundos Exclusivos — Fundos de investimento brasileiros exclusivos
Fundos Multimercado — Fundos com múltiplas estratégias
Estruturas Corporativas (LLC/IBC) — Entidades offshore
Seguros Unit-Linked — Seguros com investimento
Trusts Fiduciários — Estruturas de confiança
Faixas de Patrimônio
Até R$ 1M → Fundos Multimercado
R$ 1-10M → Estruturas Corporativas (LLC/IBC)
R$ 10-20M → Seguros Unit-Linked
Acima de R$ 20M → Trusts Fiduciários
Critérios AHP
Sucessório (40%) — Eficácia na transmissão patrimonial
Tributário (35%) — Eficiência fiscal
Operacional (15%) — Custos e complexidade
Regulatório (10%) — Conformidade legal

Autor
David Picarelli Gonçalves