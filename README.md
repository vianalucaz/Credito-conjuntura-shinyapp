# Conjuntura-de-credito-shinyapp

##Conjuntura do Crédito no Brasil 🏦📊

>Este projeto é uma continuação de um relatório feito com o mesmo objetivo que é o de analisar a conjuntura de crédito no Brasil, o relatório pode ser visualizado em: 
🔗 [Relatório Conjuntura de Crédito](https://github.com/vianalucaz/Macro/tree/main/Econ%20outlook/Credito-brasil)

> Este Dashboard interativo desenvolvido em Shiny tem como objetivo monitorar e analisar a dinâmica recente do mercado de crédito brasileiro. O projeto foca na visualização de séries temporais reais e nominais, permitindo uma análise profunda da transmissão da política monetária via canal de crédito.

🔗 Acesse o App: [Conjuntura de Crédito no Brasil (JAN/2019 - JAN/2026)] (https://vianalucaz.shinyapps.io/conjuntura_credito_brasil_files/)

## Sobre o Projeto

O dashboard consome dados em tempo real do Sistema Gerenciador de Séries Temporais (SGS) do Banco Central do Brasil. Ele permite que economistas e analistas visualizem o comportamento das concessões, taxas de juros e inadimplência, com a opção crítica de deflacionar os valores pelo IPCA para uma análise em termos reais.

Principais Funcionalidades:
KPIs em Tempo Real: Visualização imediata dos últimos dados de concessões, juros e inadimplência.

Deflação Dinâmica: Conversão de valores nominais para preços de janeiro de 2026 utilizando o índice IPCA.

Gráficos Interativos: Séries temporais de concessões (com média móvel de 12 meses), participação de PF vs. PJ, e ciclos de juros desenvolvidos com plotly.

Exportação de Dados: Aba dedicada para consulta de tabelas históricas e download em formato CSV.

##️ Bibliotecas Utilizadas

Este projeto retoma pacotes utilizados no projeto anterior, porém adicionando as bibliotecas de visualização interativa e criação de dashboard.

Shiny + bslib: Interface moderna com componentes Bootstrap 5.

Bibliotecas Principais:
GetBCBData: Acesso direto à API do SGS/BCB.

deflateBR: Tratamento de inflação e correção monetária.

plotly: Visualizações dinâmicas e interativas.

tidyverse (dplyr, tidyr): Manipulação e tratamento de dados.

zoo: Cálculos de estatísticas móveis.

## Metodologia Econômica
As séries utilizadas incluem:

Concessões de Crédito (Total, PF e PJ) - Códigos SGS: 20631, 20632, 20633.

Taxa Média de Juros - Código SGS: 20714.

Inadimplência da Carteira - Código SGS: 21082.

A análise destaca períodos críticos, como o Ciclo Pandêmico (2020-2021) e o recente aperto monetário, comparando os níveis atuais de inadimplência com o patamar pré-pandemia de 3,0%.