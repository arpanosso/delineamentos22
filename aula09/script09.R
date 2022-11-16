## Carregar os pacotes necessários
library(tidyverse)
library(skimr)
library(lme4)
library(readxl)
library(VCA)
library(janitor)

# 1) Utilizando a função "excel_sheets" do pacote {readxl} extraia
# os nomes das planilhas do arquivo aula9.xlsx, da pasta data
nomes_planilhas <- excel_sheets("data/aula9.xlsx")
nomes_planilhas[1]
nomes_planilhas[2]
nomes_planilhas[3]

# COM CONFUNDIMENTO, BLOCOS DE TAMANHO 9 -----------------------------------
# Leia o banco de dados da planilha "Fat3_confundimento"
dados <- read_xlsx("data/aula9.xlsx",
                   sheet = nomes_planilhas[1]) %>%
  clean_names()
glimpse(dados)

# Construa um resumo simples do banco de dados, utilize glimpse e skim
skim(dados)

# Realizar a análise de variância com os fatores A, B, C e BL
# definição do modelo
mod <- aov(resp ~ a*b*c + bl,
           data = dados %>%
             mutate_at(vars(a,b,c,bl), as_factor)
           )

# Análise de variância
anova(mod)

# "EXEMPLO BLOCO metade de 2^5" --------------------------------------------
# Ler o banco de dados da planilha "Metades2a5"
dados_2a5 <- read_xlsx("data/aula9.xlsx",
                   sheet = nomes_planilhas[2]) %>%
  clean_names()
glimpse(dados_2a5)

# Vislumbre dos dados
skim(dados_2a5)

# Realizar a análise de variância com os fatores A, B, C, D, E,
# e a interações duplas e BL
# definição do modelo
mod_2a5 <- aov(resp ~ a + b + c + d + e +
                 ab + ac + ad + ae +
                 bc + bd + be+
                 cd + ce+
                 de + bloco,
               data = dados_2a5 %>%
                 mutate_at(vars(a:de,bloco), as_factor)
               )

# Análise de variância
anova(mod_2a5)

# FATORIAL 3 FATORES - modelos mistos --------------------------------------
# lendo
tres_fat <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = "Fat3_misto")
glimpse(tres_fat)

## Análise preliminar com os tratamentos

## Análise com 3 fatores
mod_fixo <- aov(terms(Resp ~ A+ B +A:B +C + A:C + B:C + A:B:C),
           data = tres_fat %>% mutate_at(vars(A,B,C), as_factor))
anova(mod_fixo)

mod_misto <- lmer(Resp ~ A + (1|B) + (1|C)+
                    (1|A:B) + (1|A:C) + (1|B:C),
                  data = tres_fat %>%
                    mutate_at(vars(A,B,C), as_factor)
)
summary(mod_misto)
fitVCA(Resp ~ A + (1|B), tres_fat %>% data.frame() )
VarCorr(mod) # Variância
# data.frame(ranef(mod))
emm <- emmeans::emmeans(mod, specs = ~A)
pairs(emm,
      adjust="tukey")

