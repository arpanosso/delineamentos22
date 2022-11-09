## Carregar os pacotes necessários
library(tidyverse)
library(skimr)
library(lme4)
library(readxl)
library(VCA)

# 1) Utilizando a função "excel_sheets" do pacote {readxl} extraia
# os nomes das planilhas do arquivo aula9.xlsx, da pasta data

# COM CONFUNDIMENTO, BLOCOS DE TAMANHO 9 -----------------------------------
# Leia o banco de dados da planilha "Fat3_confundimento"

# Construa um resumo simples do banco de dados, utilize glimpse e skim

# Realizar a análise de variância com os fatores A, B, C e BL
# definição do modelo

# Análise de variância

# "EXEMPLO BLOCO metade de 2^5" --------------------------------------------
# Ler o banco de dados da planilha "Metades2a5"

# Vislumbre dos dados

# Realizar a análise de variância com os fatores A, B, C, D, E,
# e a interações duplas e BL
# definição do modelo

# Análise de variância

# FATORIAL 3 FATORES - modelos mistos --------------------------------------
# lendo
tres_fat <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = "Fat3_misto")
glimpse(tres_fat)

## Análise preliminar com os tratamentos

## Análise com 3 fatores
mod <- aov(terms(Resp ~ A+ B +A:B +C + A:C + B:C + A:B:C),
           data = tres_fat %>% mutate_at(vars(A,B,C), as_factor))
anova(mod)
mod <- lme4::lmer(Resp ~ A + (1 | B) + (1 | C)+
                    (1 | A:B) + (1 | A:C) + (1 | B:C),
                  data = tres_fat %>%
                    mutate_at(vars(A,B,C), as_factor)
)
summary(mod)
fitVCA(Resp ~ A*B*C, tres_fat %>% data.frame() )
VarCorr(mod) # Variância
# data.frame(ranef(mod))
emm <- emmeans::emmeans(mod, specs = ~A)
pairs(emm,
      adjust="tukey")

