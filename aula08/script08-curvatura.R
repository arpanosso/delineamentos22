## Carregando os pacotes exigidos
library(tidyverse)
library(emmeans)

## 1) Lendo o banco de dados que está em aula8-curvatura.xlsx
## passe as colunas do tipo character para fator

## 2) Criar as variáveis auxiliares para DOSEA e DOSEB em função de TRAT
## também conhecida como variáveis dummy /dummies

## 3) Crie o gráfico de dispersão da resposta por dose
## o gráfico pode ser feito para cada tratamento

## 4) Análise de variância utilizando tratamentos como combinação
## TRAT_DOSE.

## 5) Extrair a soma de  quadrados dos resíduos, seus graus de liberdade e
## calcular CoefVar, Desvio Padrão e Media Geral

## 6) Faça o estudo da Interação, considerando DOSE como fator

## 7) COnstrua os Boxplots por trat_dose, se necessário, utilize a
## função fct_relevel para reordenar o níveis dos fatores

## 8) Construa uma tabela de estatística descritiva com os trat, doses
## e médias de respostas com respectivos desvios padrões, semelhante à do SAS

## 9) Faça a comparação das médias pelo teste de tukey, utilize emmeans
## juntamnete com pairwise ~ TRAT:DOSE

## 10) Faça acomparação pelo test t, utilize a função LSD.test do pacote
## agricolae

## Estudo os efeitos linear em A e Linear em B, quadrático em A e quadratico
## em B, para isso construa as variáveis auxiliares TRAT_DOASEA e TRAT_DOSEB
## utilize o modelo
## RESP ~ TRAT + DOSEA + DOSEA:TRAT_DOSEA + TRAT/DOSEB + DOSEB:TRAT_DOSEB
