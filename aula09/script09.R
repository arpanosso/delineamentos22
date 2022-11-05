#pacotes
library(tidyverse)

# nomes das planilhas
planilhas <- readxl::excel_sheets("data/aula9.xlsx")

# lendo
fat333 <- readxl::read_xlsx("data/aula9.xlsx",
                           sheet = "Fat333")
glimpse(fat333)

# lendo
tres_fat <- readxl::read_xlsx("data/aula9.xlsx",
                            sheet = "TresFatresSas")
glimpse(tres_fat)

# lendo
metade_25 <- readxl::read_xlsx("data/aula9.xlsx",
                              sheet = "Metades2_5")
glimpse(metade_25)
