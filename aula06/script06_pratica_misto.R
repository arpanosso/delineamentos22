## Carregando as bibliotecas necessárias
library(ExpDes.pt)
library(patchwork)
library(tidyverse)
library(agricolae)
library(readxl)
library(nlme)
library(lme4)

## *1)SEJA UM EXPERIMENTO DE COMPETIÇÃO DE CULTIVARES T1  A T15;
## AULA PRATICA 6, SUPOR REPETIÇÕES CLONADAS NOS BLOCOS;

## Entradada de dados
aula6 <- read_excel("data/aula6-pratica-modelos-mistos.xlsx")
glimpse(aula6)

## *a)SOB HIPÓSE DE CULTIVAR FIXO;
# Nesse caso, QM(BLOCO*CULTIVAR), ESTIMA VARIÂNCIA DO AMBIENTE;
cultivar <- aula6 %>%  pull(CULTIVAR) %>% as_factor()
bloco <- aula6 %>%  pull(BLOCO) %>% as_factor()
y <- aula6 %>%  pull(RESP)
mod <- aov(y ~bloco + cultivar)
anova(mod)

## Comparação de médias
LSD.test(mod, "cultivar", group=TRUE, console = TRUE)


# *b)SOB HIPÓTESE DE CULTIVAR ALEATÓRIO E BLOCO FIXO;
mod_misto <- lme(y ~ bloco, random = ~1|cultivar, method = "REML")
anova(mod_misto)
summary(mod_misto)

# Solution for Random Effects
ranef(mod_misto)

# VALOR GENOTÍPICO= (DEP=ESTIMATE) + MÉDIA GERAL
ranef(mod_misto) + mean(y)


mod_mixed <- lmer(y ~ bloco + (1 | cultivar))
summary(mod_mixed)
as.data.frame(ranef(mod_mixed))
confint(mod_mixed)
ranef(mod_mixed)$cultivar %>% head(10)
coef(mod_mixed)$cultivar %>% head(10)

ranef(mod_mixed)$cultivar %>% head(10) + mean(y)
