# BLOCOS COM FAMÍLIAS E PROGENIES  DENTRO DE FAMÍLIAS
## Carregando os pacotes
##https://bookdown.org/hhwagner1/LandGenCourse_book/video_6.html
# https://cran.r-project.org/web/packages/VCA/vignettes/VCA_package_vignette.html
library(tidyverse)
library(agricolae)
library(readxl)
library(ExpDes.pt)
library(lme4)
library(VCA)

## Entrada de dados
aula5 <- read_excel("data/aula5.xlsx")
familia <- aula5 %>% pull(familia) %>%  as_factor()
bloco <- aula5 %>% pull(bloco) %>%  as_factor()
genotipo <- aula5 %>% pull(genotipo) %>%  as_factor()
y <- aula5 %>% pull(resp)
a5<- as.data.frame(aula5)

## a)	ANOVA, usando as progenies:
modelo <- aov(y ~ bloco + familia  + Error(bloco/familia) + genotipo:bloco )
summary(modelo)

## b)	ANOVA,usando média das progênies, em cada combinação de bloco e familia:
modelo <- aov(y ~ bloco + familia  + Error(familia*bloco) )

a5$familia <- as_factor(a5$familia)
a5$bloco <- as_factor(a5$bloco)
fitVCA(resp~bloco + familia/bloco, a5)

