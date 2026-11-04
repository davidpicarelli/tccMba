# Análise de Progressão Patrimonial e Eficiência Fiscal de Estruturas Offshore (5 Anos)

Repositório de reprodutibilidade computacional do Trabalho de Conclusão de Curso (TCC) apresentado ao **MBA Executivo em Contabilidade e Finanças Corporativas** do Centro Universitário UniFatecie (2026).

**Título do Artigo:** *Análise Comparativa de Estruturas de Investimento Offshore para a Eficiência Tributária e Sucessória de Investidores Brasileiros*  
**Autor:** David Picarelli Gonçalves  
**Orientador:** Prof. Dr. Edi Carlos de Oliveira da Silva

---

## Como usar este projeto

Abra este projeto sempre pelo arquivo `.Rproj` no RStudio para garantir que o diretório de trabalho raiz esteja configurado corretamente.

### Passo 1: Instale os pacotes necessários
```r
install.packages(c("tidyverse", "ggplot2", "flextable"))

```

### Passo 2: Execute o script mestre para rodar toda a análise

```r
source("master_analise_completa.R")

```

### Passo 3: Verifique as saídas geradas

* **Gráficos:** Os gráficos em alta resolução (PNG e PDF) serão salvos automaticamente na pasta `graficos/`.
* **Tabelas:** As tabelas formatadas em Word (`.docx`) serão exportadas para a pasta `tabelas/`.

---

## Arquitetura do Repositório

```text
tccMba/
├── README.md                      # Documentação do projeto
├── tccMba.Rproj                   # Ficheiro de Projeto do RStudio
├── master_analise_completa.R      # Script orquestrador principal
├── scripts_r/                     # Scripts modulares em R
│   ├── 01_progressao_patrimonial.R
│   ├── 02_analise_fiscal.R
│   ├── 03_recomendacao_dinamica.R
│   ├── 04_exportar_tabelas.R
│   └── 05_exportar_graficos.R
├── graficos/                      # Visualizações geradas (PNG/PDF)
└── tabelas/                       # Tabelas formatadas para o Word (.docx)

```

---

## Descrição dos Scripts

| Script | Função Técnica | Saída Gerada |
| --- | --- | --- |
| `01_progressao_patrimonial.R` | Simula a evolução semestral de R$ 5 milhões por 5 anos (retorno bruto de 8% a.a.) | Dataframes e curvas de evolução |
| `02_analise_fiscal.R` | Consolida *come-cotas*, IR ordinário e custos operacionais acumulados | Métricas fiscais comparativas |
| `03_recomendacao_dinamica.R` | Integra os escores multicritério (AHP) com o impacto fiscal quinquenal | Ranking dinâmico por faixa |
| `04_exportar_tabelas.R` | Formata e exporta tabelas estruturadas via `flextable` | Ficheiros `.docx` em `tabelas/` |
| `05_exportar_graficos.R` | Renderiza gráficos comparativos e individuais em alta resolução (300 DPI) | Ficheiros `.png` e `.pdf` em `graficos/` |

---

## Síntese Metodológica e Escopo

* **Estruturas Analisadas:** Contas Diretas, Fundos Exclusivos, Fundos Multimercado, Estruturas Societárias (LLC/IBC), Seguros *Unit-Linked* e *Trusts* Fiduciários.
* **Horizonte de Simulação:** 5 anos (60 meses / 10 semestres) com aporte inicial de R$ 5.000.000,00 e retorno bruto constante de 8% a.a.
* **Critérios AHP:** Sucessório (40%), Tributário (35%), Operacional (15%) e Regulatório (10%).

---

## Autor

**David Picarelli Gonçalves**
Lattes: https://lattes.cnpq.br/5376879868796401

```
```