## Carregando os pacotes exigidos
library(tidyverse)
library(emmeans)
## Lendo o banco de dados
dados <- readxl::read_xlsx("data/aula8-curva.xlsx") %>%
  mutate(across(is.character, as_factor))

## Criar as variáveis auxiliares para DOSEA e DOSEB em função de TRAT
dados <- dados %>%
  mutate(DOSEA = ifelse(TRAT == 'A',DOSE,0),
         DOSEB = ifelse(TRAT == 'B',DOSE,0))
dplyr::glimpse(dados)

## Crie o gráfico de dispersão da resposta por dose
dados %>%
  ggplot(aes(x=DOSE, y=RESP, shape=TRAT, color=TRAT)) +
  geom_point()

# Análise de variância preliminar.
m0 <- aov(RESP ~ TRAT_DOSE, data=dados%>%
           mutate(
             TRAT_DOSE = interaction(TRAT, DOSE)
           ))
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
    RESP ~ TRAT*DOSE,
    data=dados %>% mutate_at(vars(DOSE),as_factor)
)
anova(modelo)


## Boxplot por trat_dose

dados %>%
  mutate(
    TRAT_DOSE = interaction(TRAT, DOSE,sep=""),
    TRAT_DOSE = fct_relevel(TRAT_DOSE,"A5","A10","A13")
  ) %>%
  ggplot(aes(x=TRAT_DOSE, y=RESP)) +
  geom_boxplot()

dados %>%
  group_by(TRAT,DOSE) %>%
  summarise(
    n=n(),
    Mean = mean(RESP),
    Std_Dev = sd(RESP)
  )

emmeans(modelo, pairwise ~ TRAT:DOSE,
        adjust = "tukey")

agricolae::LSD.test(m0,"TRAT_DOSE",
                    console = TRUE)

m1 <- aov(
  RESP ~ TRAT + TRAT/DOSEA + DOSEA:TRAT_DOSEA + TRAT/DOSEB +
    DOSEB:TRAT_DOSEB,
  data=dados %>%
    mutate(
      TRAT_DOSEA = interaction(TRAT,DOSEA),
      TRAT_DOSEB = interaction(TRAT,DOSEB)
    )
)
summary(m1)
