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

