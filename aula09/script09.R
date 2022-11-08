## Carregar os pacotes necessários
library(tidyverse)
library(skimr)
library(lme4)

# 1) Utilizando a função "excel_sheets" do pacote {readxl} extraia
# os nomes das planilhas do arquivo aula9.xlsx
pla <-readxl::excel_sheets("data/aula9.xlsx")

# COM CONFUNDIMENTO, BLOCOS DE TAMANHO 9 -----------------------------------
# Ler o banco de dados
fat333 <- readxl::read_xlsx("data/aula9.xlsx",
                           sheet = pla[1])
# vislumbre
glimpse(fat333)

# resumo rápido
skim(fat333)

# Criação do modelo
mod <- aov(RESP ~ BL + A*B*C,
   data = fat333 %>%
     mutate_at(vars(A,B,C,BL), as.factor)
)

# Análise de variância
anova(mod)

# "EXEMPLO BLOCO metade de 2^5" --------------------------------------------
# Ler o banco de dados
metade_25 <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = pla[2])
# Vislumbre dos dados
glimpse(metade_25)

# Criação do modelo
mod <- aov(RESP ~ BLOCO + A + B + C+ D+ E + A:B + A:C + A:D
           + A:E + B:C + B:D + B:E + C:D + C:E + D:E,
           data = metade_25 %>%
             mutate_at(vars(A,B,C,D,E,BLOCO), as.factor)
)

# Análise de variância
anova(mod)

# 'ATORIAL 3 FATORES - modelos mistos --------------------------------------
# lendo
tres_fat <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = pla[3])
glimpse(tres_fat)

## Análise preliminar com os tratamentos

mod <- aov(Resp ~ Trat,
           data = tres_fat %>% mutate_at(vars(Trat), as_factor))

anova(mod)

## Análise com 3 fatores

mod <- aov(terms(Resp ~ A+ B +A:B +C + A:C + B:C + A:B:C),
           data = tres_fat %>% mutate_at(vars(A,B,C), as_factor))

anova(mod)

car::Anova(mod, type=2)

mod <- lme4::lmer(Resp ~ A + (1 | B) + (1 | C)+
                    (1 | A:B) + (1 | A:C) + (1 | B:C)+
                    (1 | A:B:C),
           data = tres_fat %>%
             mutate_at(vars(A,B,C), as_factor)
)
summary(mod)
VCA::fitVCA(Resp ~ A*B*C, tres_fat %>% data.frame() )
as.data.frame(ranef(mod))
confint(mod)
VarCorr(mod) # Variância


data.frame(ranef(mod))


emm <- emmeans::emmeans(mod, specs = ~A)

pairs(emm,
      adjust="tukey")
