## Carregando as bibliotecas necessárias
library(ExpDes.pt)
library(patchwork)
library(tidyverse)
library(agricolae)
library(readxl)
library(nlme) # novo pacote
library(lme4) # novo pacote

## Entradada de dados aula6-exemplo-modelos-mistos.xlsx

## Interaction Plot - Resposta por bloco

## Boxplot por PROG

## *ANÁLISE DE VARIÂNCIA INTRABLOCO;

## Comparação de médias

## MODELO MISTO COM PROGENIE ALEATÓRIO - pacote nlme
mod_misto <- lme(y ~ bloco, random = ~1|prog, method = "REML")
anova(mod_misto)
summary(mod_misto)

# Solution for Random Effects
ranef(mod_misto)

# Amplitude - Estreitamento do modelo
ranef(mod_misto) %>% range() %>% diff()

# VALOR GENOTÍPICO= (DEP=ESTIMATE) + MÉDIA GERAL
ranef(mod_misto) + mean(y)

## MODELO MISTO COM PROGENIE ALEATÓRIO - pacote lme4
mod_mixed <- lmer(y ~ bloco + (1 | prog))
summary(mod_mixed)
as.data.frame(ranef(mod_mixed))
ranef(mod_mixed)$prog %>% head(10)
coef(mod_mixed)$prog %>% head(10)
VarCorr(mod_mixed) # Variância
confint(mod_mixed) # intervalo de confiânça
