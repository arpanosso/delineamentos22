library(tidyverse)
library(agricolae)
library(readxl)
library(car)
library(emmeans)
library(lme4)

d_lattice <- read_xlsx("data/aula7-lattice-resumo.xlsx") %>%
  mutate(GROUP_BLOC = interaction(GROUP, BLOCK, sep="-")) %>%
  mutate_at(vars(GROUP, TREATMNT, BLOCK, GROUP_BLOC), as_factor)

mod <- lmer(
    RESP ~ GROUP + TREATMNT + (1|GROUP_BLOC),
    data= d_lattice)
anova(mod)
summary(mod)
VarCorr(mod) # Variância
confint(mod)
Anova(mod, type =3)

emmeans(mod, specs = ~TREATMNT) %>% data.frame()
