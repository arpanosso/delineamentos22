#pacotes
library(tidyverse)
library(skimr)
# nomes das planilhas
planilhas <- readxl::excel_sheets("data/aula9.xlsx")



# Fat 3x3x3 ---------------------------------------------------------------
# lendo
fat333 <- readxl::read_xlsx("data/aula9.xlsx",
                           sheet = "Fat333")
# vislumbre
glimpse(fat333)

# resumo rapito
skim(fat333)

## anova
mod <- aov(RESP ~ BL + A*B*C,
   data = fat333 %>%
     mutate_at(vars(A,B,C,BL), as.factor)
)
anova(mod)
(deviance(mod)/ df.residual(mod))^.5 / mean(fat333$RESP) *100


# Metade 2^5 --------------------------------------------------------------
# lendo
metade_25 <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = "Metades2_5")
glimpse(metade_25)
mod <- aov(RESP ~ BLOCO + A + B + C+ D+ E + A:B + A:C + A:D
           + A:E + B:C + B:D + B:E + C:D + C:E + D:E,
           data = metade_25 %>%
             mutate_at(vars(A,B,C,D,E,BLOCO), as.factor)
)
anova(mod)

# Tres Fatores no SAS -----------------------------------------------------
# lendo
tres_fat <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = "TresFatresSas")
glimpse(tres_fat)
