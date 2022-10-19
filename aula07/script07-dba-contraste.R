# Blocos Aumentados Simples
## Carregando os Pacotes
library(tidyverse)
library(agricolae)
library(car)
library(emmeans)
library(lme4)

## Entrada de dados
dba_ctr <- read_xlsx("data/aula7-DBA-contraste.xlsx")

## Construir as colunas XouC, X e C

## Visualização dos blocos
dba_ctr %>%
  ggplot(
    aes(y = RESP,
        x = TRAT,
        color = TRAT,
        shape = BLOCO)) +
  geom_point(size = 2) +
  ylim(0, NA) +
  guides(color = "none") +
  theme_classic() +
  theme(legend.position = "top")

## Análise usando os contrastes
modelo <- lm(
  terms(RESP ~ XouC + C + X + BLOCO, keep.order = TRUE),
  data = dba_ctr
)
anova(modelo)

emmeans(modelo, spec=~"XouC") %>% data.frame()
emmeans(modelo, spec=~"C") %>% data.frame()
emmeans(modelo, spec=~"X") %>% data.frame()


## Efeito Aleatório
mod_mixed <- lmer(RESP ~ BLOCO + XouC + XouC/C +
                 (1| XouC/X),
               data= dba_ctr)
anova(mod_mixed)
summary(mod_mixed)
as.data.frame(ranef(mod_mixed))
confint(mod_mixed)
VarCorr(mod_mixed) # Variância
confint(mod_mixed) # intervalo de confiânça

