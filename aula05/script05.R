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
library(nlme)

## Entrada de dados
aula5 <- read_excel("data/aula5.xlsx")
familia <- aula5 %>% pull(familia) %>%  as_factor()
bloco <- aula5 %>% pull(bloco) %>%  as_factor()
genotipo <- aula5 %>% pull(genotipo) %>%  as_factor()
genotipo_b <- aula5 %>% pull(genotipo_b) %>%  as_factor()
y <- aula5 %>% pull(resp)

## a)	ANOVA, usando as progenies:
modelo_01 <- aov(y ~ bloco + familia  + Error(bloco/familia) + genotipo:bloco )
summary(modelo_01)

a5<- as.data.frame(aula5)
a5$familia <- as_factor(a5$familia)
a5$bloco <- as_factor(a5$bloco)
fitVCA(resp~bloco + familia/bloco, a5)

## b)	ANOVA,usando média das progênies, em cada combinação de bloco e familia:
modelo_02 <- aov(y ~ bloco + familia  + Error(familia/bloco) )
summary(modelo_02)
fitVCA(resp~bloco + familia + familia/bloco, a5)

## Componentes de veriância utilizando o delineamento em quadrado latino
aula5_ql <- read.table("data/aula5_ql.txt",
                       h=TRUE)
head(aula5_ql)

## Análise, delineamento quadrado latino
linha <- aula5_ql %>% pull(LINHA) %>% as_factor()
coluna  <- aula5_ql %>% pull(COLUNA) %>% as_factor()
trat  <- aula5_ql %>% pull(TRAT) %>% as_factor()
y  <- aula5_ql %>% pull(RESP)

m0 <- aov(y ~ linha + coluna + trat)
anova(m0)

## usando o Exp.Des.pt
ExpDes.pt::dql(trat, linha, coluna, y, quali = TRUE,
               mcomp = "tukey")


## Vamos especificar os efeito fixos e aleatório, usando
## o pacote nlme
m1 <- lme(y ~ trat, random = ~1|linha/coluna)
anova.lme(m1)
summary(m1)


m1 <- lme(y ~ linha + coluna, random = ~1|trat)
anova.lme(m1)
summary(m1)
