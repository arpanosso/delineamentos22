# Carregando os pacotes necessáros
# library(devtools)
# install_github(repo = "walmes/wzRfun", ref = "master")

# install.packages("remotes")
# remotes::install_github("elsayed-lab/hpgltools")

library(tidyverse)
library(agricolae)
library(readxl)
library(car)
library(emmeans)
library(multcomp)
library(wzRfun)
library(hpgltools)
library(ballgown)

phylobase::pDat

# DBA - Blocos aumentados Simples -----------------------------------------
dba_simples <- read_xlsx("data/aula7-DBA-simples.xlsx")
DAU.test(dba_simples$BLOCO,dba_simles$TRAT,dba_simples$RESP,
         method = "lsd",alpha=0.05,group=TRUE,console = TRUE)
bloco <- as.factor(dba_simples$BLOCO)
trat <- as_factor(dba_simles$TRAT)
y <- dba_simples$RESP
mod <-aov( terms(y ~ bloco  + trat,
                 keep.order = TRUE ))
Anova(mod,type=3)

LSD.test(mod, "trat", group=TRUE, console = TRUE)

# Médias marginais ajustadas.
emm <- emmeans(mod, specs = ~trat)

# Extração da matriz de funções lineares.
L <- attr(emm, "linfct")
grid <- attr(emm, "grid")
rownames(L) <- grid[[1]]

# Entenda como são obtidas as médias marginais.
fractions(t(L))

# Contrastes par a par.
ctr <- summary(glht(m0, linfct = all_pairwise(L)),
               test = adjusted(type = "fdr"))

# Comparações múltiplas a 10%.
results_m0 <- wzRfun::apmc(X = L,
                           model = mod,
                           focus = "trat",
                           test = "fdr")
results_m0 %>% arrange(desc(fit))

ggplot(data = results_m0,
       mapping = aes(x = fit, y = reorder(trat, fit))) +
  geom_point() +
  geom_errorbarh(mapping = aes(xmin = lwr, xmax = upr),
                 height = 0) +
  geom_label(mapping = aes(label = sprintf("%0.2f%s", fit, cld)),
             label.padding = unit(0.15, "lines"),
             fill = "black",
             colour = "white",
             size = 3,
             nudge_x = 0.25,
             vjust = 0.5) +
  labs(x = "Produção",
       y = "Linhagens")

# Blocos aumentados contraste ---------------------------------------------
dba_ctr <- read_xlsx("data/aula7-DBA-contraste.xlsx")

# Blocos incompletos ------------------------------------------------------
dbib <- read_xlsx("data/aula7-BIB.xlsx")

# LATTICE -----------------------------------------------------------------
d_lattice <- read_xlsx("data/aula7-lattice.xlsx")

# LATTICE RESUMO -----------------------------------------------------------------
d_resumo <- read_xlsx("data/aula7-lattice-resumo.xlsx")
