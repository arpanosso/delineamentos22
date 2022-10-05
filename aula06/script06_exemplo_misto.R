## Carregando as bibliotecas necessárias
library(ExpDes.pt)
library(patchwork)
library(tidyverse)
library(agricolae)
library(readxl)
library(nlme)
library(lme4)

## Entradada de dados
aula6_p_misto <- read_excel("data/aula6-exemplo-modelos-mistos.xlsx")

## *ANÁLISE INTRABLOCO;
prog <- aula6_p_misto %>%  pull(PROG) %>% as_factor()
bloco <- aula6_p_misto %>%  pull(BLOCO) %>% as_factor()
y <- aula6_p_misto %>%  pull(RESP)
modelo <- aov(y ~bloco + prog)
anova(modelo)

## Interaction Plot
aula6_p_misto %>%
  ggplot(aes(x=BLOCO, y=RESP, color=as_factor(PROG))) +
  geom_point() +
  geom_line()

## Boxplot
aula6_p_misto %>%
  ggplot(aes(x=PROG, y=RESP, fill = as_factor(PROG))) +
  geom_boxplot() +
  scale_fill_viridis_d()

## Comparação de médias
LSD.test(mod, "prog", group=TRUE, console = TRUE)

## MODELO MISTO COM PROGENIE ALEATÓRIO - pacote nlme
mod_misto <- lme(y ~ bloco, random = ~1|prog, method = "REML")
anova(mod_misto)
summary(mod_misto)

# Solution for Random Effects
ranef(mod_misto)

# VALOR GENOTÍPICO= (DEP=ESTIMATE) + MÉDIA GERAL
ranef(mod_misto) + mean(y)

## MODELO MISTO COM PROGENIE ALEATÓRIO - pacote lme4
mod_mixed <- lmer(y ~ bloco + (1 | prog))
summary(mod_mixed)
as.data.frame(ranef(mod_mixed))
confint(mod_mixed)
ranef(mod_mixed)$prog %>% head(10)
coef(mod_mixed)$prog %>% head(10)
ranef(mod_mixed)$prog %>% head(10) + mean(y)