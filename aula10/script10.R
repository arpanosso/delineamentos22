library(tidyverse)
library(readxl)
library(janitor)
library(emmeans)
planilhas <- excel_sheets("data/aula10.xlsx")

dados <- read_xlsx("data/aula10.xlsx",
                   sheet = planilhas[1]) %>%
  clean_names() %>%
  mutate(across(is.character,as_factor))
glimpse(dados)

mod <- aov(resp ~ bloco +fam,
           data = dados %>%
             mutate_at(vars(fam, bloco), as_factor))
anova(mod)

mod <- aov(resp ~ bloco + fam + Error(fam/bloco) + micro + fam:micro,
           data = dados %>%
             mutate_at(vars(fam, bloco), as_factor)
           )
summary(mod)

fam <- dados$fam
micro <- dados$micro
bloco <- dados$bloco
resp <-dados$resp
ExpDes.pt::psub2.dbc(fam,micro,bloco,resp,mcomp="lsd")



# análise conjunta
dados <- read_xlsx("data/aula10.xlsx",
                   sheet = planilhas[2]) %>%
  clean_names() %>%
  mutate(across(is.character,as_factor))
glimpse(dados)


mod <- aov(resp ~local + cultivar + local:cultivar+bloco,
           data = dados %>%
             mutate_at(vars(local,bloco,cultivar),as_factor))
anova(mod)

# Médias marginais ajustadas por local.
emm <- emmeans(mod, specs = ~cultivar:local)
pairs(emm,
      adjust="tukey")
