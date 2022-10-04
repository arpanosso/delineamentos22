## Carregando as bibliotecas necessárias
library(ExpDes.pt)
library(patchwork)
library(tidyverse)
library(agricolae)
library(readxl)

## Entradada de dados
aula6_ad <- read_excel("data/aula6-aditividade.xlsx")

## Gráfico de linhas por blocos
pt_ad_1 <- aula6_ad %>%
  ggplot(aes(x=as.numeric(as_factor(cultivar)), y=resp,
             color = as_factor(bloco))) +
  geom_point() +
  geom_line() +
  labs(x="Cultivar", color="Bloco") +
  theme_bw()
pt_ad_1

## Análise de variância.
trat <- aula6_ad %>%  pull(cultivar) %>% as_factor()
bloco <- aula6_ad %>%  pull(bloco) %>% as_factor()
y <- aula6_ad %>%  pull(resp)

## Excluindo o valor 35
pt_ad_2 <- aula6_ad %>%
  filter(resp != 35) %>%
  ggplot(aes(x=as.numeric(as_factor(cultivar)), y=resp,
             color = as_factor(bloco))) +
  geom_point() +
  geom_line() +
  labs(x="Cultivar", color="Bloco") +
  theme_bw()
pt_ad_2

## Vamos usar o lógica do patchwork
pt_ad_1 | pt_ad_2

## ou
pt_ad_1 / pt_ad_2

# Sem aditividade
## Análise de variância
mod <- aov(y ~bloco + trat)
anova(mod)

## Comparação de médias
LSD.test(mod, "trat", group=TRUE, console = TRUE)

# Com aditividade
trat <- aula6_ad %>%  filter(resp != 35) %>% pull(cultivar) %>% as_factor()
bloco <- aula6_ad %>%  filter(resp != 35) %>% pull(bloco) %>% as_factor()
y <- aula6_ad %>%  filter(resp != 35) %>% pull(resp)

mod <- lm(y ~bloco + trat)
anova(mod)

## Comparação de médias
LSD.test(mod, "trat", group=TRUE, console = TRUE)




