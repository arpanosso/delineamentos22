## Carregando os pacotes exigidos
library(tidyverse)
library(emmeans)

## 1) Lendo o banco de dados que está em aula8-curvatura.xlsx
## passe as colunas do tipo character para fator
dados <- readxl::read_xlsx("data/aula8-curva.xlsx") %>%
  mutate(across(is.character,as_factor))
glimpse(dados)

## 2) Criar as variáveis auxiliares para DOSEA e DOSEB
## em função de TRAT
## também conhecida como variáveis dummy/dummies
dados <- dados %>%
  mutate(
    DOSEA = ifelse(TRAT == "A", DOSE, 0),
    DOSEB = ifelse(TRAT == "B", DOSE, 0)
  )
glimpse(dados)

## 3) Crie o gráfico de dispersão da resposta
## por dose, o gráfico pode ser feito para
## cada tratamento ou não...
dados %>%
  ggplot(aes(x=DOSE, y=RESP, color=TRAT)) +
  geom_point() +
  facet_wrap(~TRAT, nrow=2) ## um gráfico para cada Nivel de TRAT

## 4) Análise de variância utilizando
## tratamentos como combinação
## TRAT_DOSE.
m0 <- aov(RESP ~ TRAT_DOSE,
          data = dados %>%
            mutate(
              TRAT_DOSE = interaction(TRAT,DOSE)
            )
          )
anova(m0)

## 5) Extrair a soma de  quadrados dos resíduos,
## seus graus de liberdade e
## calcular CoefVar, Desvio Padrão e Media Geral
## média geral
media_geral <- dados %>% pull(RESP) %>% mean()

## extrair a soma de quadrados dos resíduos
SQr <- deviance(m0)

## extrair os graus de liberdade dos  resíduos
GLr <- df.residual(m0)

## Quadrado médio do resíduo
QMr <- SQr/GLr

## Desvio padrão
DP <- sqrt(QMr)

## Coeficiente de Variação
100* DP/media_geral

## 6) Faça o estudo da Interação, considerando
## DOSE como fator
m1 <- aov(RESP ~ TRAT*DOSE,
          data = dados %>%
            mutate_at(vars(DOSE),as_factor))
anova(m1)

## 7) Construa os Boxplots por trat_dose, se
## necessário, utilize a função fct_relevel para
## reordenar o níveis dos fatores
dados %>%
  mutate(
    TRAT_DOSE = interaction(TRAT,DOSE),
    TRAT_DOSE = fct_relevel(TRAT_DOSE,"A.5","A.10","A.13")
  ) %>%
  ggplot(aes(x=TRAT_DOSE,y=RESP))+
  geom_violin(trim = FALSE,
              fill="grey") +
  geom_boxplot(width=0.25) +
  theme_bw()

## 8) Construa uma tabela de estatística
## descritiva com os trat, doses
## e médias de respostas com respectivos
## desvios padrões, semelhante à do SAS
dados %>%
  group_by(TRAT, DOSE) %>%
  summarise(
    N = n(),
    Media = mean(RESP),
    DesvPad = sd(RESP)
  )

## 9) Faça a comparação das médias
## pelo teste de tukey, utilize emmeans
## juntamente com pairwise ~ TRAT:DOSE
emmeans(m1, pairwise ~ TRAT:DOSE)


## 10) Faça a comparação pelo test t, utilize a
## função LSD.test do pacote agricolae
agricolae::LSD.test(m0, "TRAT_DOSE", console=TRUE)

## Estudo os efeitos linear em A e
## Linear em B, quadrático em A e
## quadratico em B, para isso construa as variáveis auxiliares TRAT_DOASEA e TRAT_DOSEB
## utilize o modelo
## RESP ~ TRAT + DOSEA + DOSEA:TRAT_DOSEA + TRAT/DOSEB + DOSEB:TRAT_DOSEB
m2 <- aov(RESP ~ TRAT + DOSEA + DOSEA:TRAT_DOSEA +
            TRAT/DOSEB + DOSEB:TRAT_DOSEB,
          data = dados %>%
            mutate(
              TRAT_DOSEA = ifelse(TRAT =="A",DOSE,0),
              TRAT_DOSEB = ifelse(TRAT =="B",DOSE,0)
            )
          )
anova(m2)




















