## Carregando os pacotes exigidos
library(tidyverse)
library(emmeans)

## Lendo o banco de dados
dados <- readxl::read_xlsx("data/aula8-superficie.xlsx")

## Criar as variáveis auxiliares FA = TRAT e NK
dados <- dados %>%
  mutate(FA = TRAT,
         NK= interaction(N,K,sep="-"))
dplyr::glimpse(dados)

# Análise de variância preliminar.
m0 <- aov(RESP ~ NK, data=dados)
anova(m0)

## Extrair a soma de  quadrados dos resíduos, seus graus de liberdade e
## calcular CoefVar, Desvio Padrão, Media Geral
y <- dados %>% pull(RESP)
SQr <- deviance(m0)
GLr <- df.residual(m0)
QMr <- SQr/GLr
yp <-  predict(m0)
mean(y)
100*QMr^(.5)/mean(y)
sqrt(QMr)


## Interação
modelo <- aov(
    RESP ~ N*K,
    data=dados %>% mutate_at(vars(N,K),as_factor)
)
anova(modelo)
car::Anova(modelo, type=3)

## Crie o gráfico de Interação
dados %>%
  group_by(N, K) %>%
  summarise(
    RESP = mean(RESP)
  ) %>%
  ggplot(aes(x=N, y=RESP, color=as_factor(K))) +
  geom_point() +
  geom_line()


## Análise de regressão linear
library(ExpDes.pt)
N <- dados %>% pull(N)
K <- dados %>% pull(K)
RESP <- dados %>% pull(RESP)
fat2.dic(N, K, RESP, quali = c(FALSE,FALSE),fac.names = c("N","K"))

## Lack Fi in R
mod <- lm(RESP ~ N + K +
                  N2 + K2 + N:K, data=dados %>%
                  mutate(K2=K*K, N2=N*N))
anova(mod)
summary(mod)
