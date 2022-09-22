# CONFORME OS TRATAMENTOS, UMA ANÁLISE MAIS APROPRIADA.
## Carregando os pacotes
library(tidyverse)
library(agricolae)
library(readxl)
library(ExpDes.pt)

## Entrada de dados
aula4 <- read_excel("data/aula4.xlsx")
aula4


# caso 1: sem informação prévia sobre os trat;
## Anova pelo ExpDes
trat <- aula4 %>% pull(trat) %>% as_factor()
y <- aula4 %>% pull(resp)

dic(trat, y, quali = TRUE, mcomp = "tukey",
    sigT = 0.05, sigF = 0.05)
## https://www.r-bloggers.com/2017/06/linear-models-anova-glms-and-mixed-effects-models-in-r/
mod1 <- aov(y ~ trat)
model.tables(mod1, type="effects")
model.tables(mod1, type="means")
tab <- model.tables(mod1, type="effects")
pluck(tab,1)$trat + mean(y)


mod2 <- lm(y~trat)
summary(mod2)
# trat1 não é mostrado
# pois ele é considerado o nivel de referencia
# 15 é a média para o TRAT1
# para calcula a média, é so somar o valor de ref
# com os demais slopes
# trat2 = 15 + 2 = 17

# pode-se mudar a referencia com o relevel
trat = relevel(trat, ref=2)
mod3 <- lm(y ~ trat)
summary(mod3)

# mudar a soma para não balanceados
library(car)
Anova(mod2,type = "III")

# caso 2: com duas origens ou acessos;
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_01 <- cbind(
  c(-1, -1, 1, 1),
  c(-1, 1, 0, 0),
  c(0, 0, -1, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_01
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_01<-aov(y~trat)
summary(modelo_01,
        split= list(trat=
                      list("origem A vs origem B"= 1,
                           "trat dentro da origem A"= 2,
                           "trat dentro da origem B"= 3)))

a4 <- aula4 %>%
  mutate(
    origem = ifelse(origem=="A",-1,1),
    origemA = ifelse(trat==1,-1,ifelse(trat==2,1,0)),
    origemB = ifelse(trat==3,-1,ifelse(trat==4,1,0))
  )
model <- lm(resp ~ origem + origemA + origemB, data=a4)
anova(model)


origem <- aula4 %>% pull(origem) %>% as.factor()
model <- lm(y ~ origem)
anova(model)

# caso 3: com uma testemunha;
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_02 <- cbind(
  c(-3, 1, 1, 1),
  c(0, -2, 1, 1),
  c(0, 0, -1, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_02
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_02<-aov(y~trat)
summary(modelo_02,
        split= list(trat=
                      list("testemunha vs demais"= 1,
                           "origem dentro dos novos"= 2,
                           "trat dentro da origem B"= 3)))


# caso 4: dialelos ou fatorial;
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_03 <- cbind(
  c(-1, -1, 1, 1),
  c(-1, 1, -1, 1),
  c(1, -1, -1, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_03
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_03<-aov(y~trat)
summary(modelo_03,
        split= list(trat=
                      list("mae a1 vs mae a2"= 1,
                           "pai b1 vs pai b2"= 2,
                           "interação mae e pai"= 3)))

mae <- aula4 %>% pull(mae) %>%  as_factor()
pai <- aula4 %>% pull(pai) %>%  as_factor()
model <- lm(y ~ mae*pai)
anova(model)
# caso 5: ajuste de regressão
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_04 <- cbind(
  c(-3, -1, 1, 3),
  c(1, -1, -1, 1),
  c(-1, 3, -3, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_04
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_04<-aov(y~trat)
summary(modelo_04,
        split= list(trat=
                      list("linear"= 1,
                           "quadrático"= 2,
                           "cúbico"= 3)))

# analise de regressão
dic(trat, y, quali=FALSE)

## https://mspeekenbrink.github.io/sdam-r-companion/contrast-coding-and-oneway-anova.html
