## Carregando os pacotes necessários
library(tidyverse)
library(readxl)
library(janitor)
library(emmeans)
library(agricolae)
library(lme4)
library(nlme) ##<- carregar esse tmb
library(VCA)
library(ExpDes.pt)

# Análise Conjunta --------------------------------------------------------
## Buscar os nomes das planilhas no arquivo
## aula10.xlsx na pasta data
planilhas <- excel_sheets("data/aula10.xlsx")
planilhas

## Carregar o banco de dados de análise conjunta
conjunta <- read_xlsx("data/aula10.xlsx",
                      sheet = planilhas[1]) %>%
  clean_names() %>%
  mutate_at(vars(local, bloco, cultivar), as_factor)

## vislumbre do banco de dados
glimpse(conjunta)

## Realizar o DBC por local
# minha função
meu_dbc <- function(meu_local=1){
  mod <- aov(resp ~ bloco + cultivar,
             data = conjunta %>%
               filter(local == meu_local)
  )
  anova(mod) %>%  print()
  LSD.test(mod,"cultivar",group = TRUE, console = TRUE)
}
meu_dbc(1) # Local 1
meu_dbc(2) # Local 2
meu_dbc(3) # Local 3
meu_dbc(4) # Local 4

## Gráfico das respostas por local
conjunta %>%
  group_by(local, cultivar) %>%
  summarise(media = mean(resp)) %>%
  ggplot(aes(x=as.numeric(local), y=media,
             color=cultivar)) +
  geom_point() +
  geom_line()

## Análise conjunta
mod_conjunta <- aov(resp ~ local + cultivar + local:cultivar + local/bloco,
                    data = conjunta)
summary(mod_conjunta)

## Médias ajustadas por local.
medias_ajustadas <- emmeans(mod_conjunta, specs = ~local:cultivar)
medias_ajustadas %>% data.frame() %>%
  arrange(desc(emmean))
pairs(medias_ajustadas)

## Modelo utilizando tratamento
mod_trat <- aov(resp ~ bloco + trat,
                data = conjunta %>%
                  mutate(trat = interaction(cultivar,local))
                  )
anova(mod_trat)
LSD.test(mod_trat, "trat", group=TRUE, console = TRUE)

## Utilizar as predições de modelo misto
mod_misto <-lmer(resp ~ local + cultivar +
                   (1|local:cultivar) + (1|local_bloco),
                 data = conjunta %>%
                   mutate(local_bloco = interaction(local,bloco)))
summary(mod_misto)

## apresentar oe efeitos aleatórios
ranef(mod_misto) %>% data.frame()

## variâncias e correlações
VarCorr(mod_misto)

##
medias_ajustadas_misto <- emmeans(mod_misto, specs = ~local:cultivar)
medias_ajustadas_misto %>%  data.frame()
pairs(medias_ajustadas_misto,adjust="tukey"
        )

# SplitPlot ---------------------------------------------------------------
## Carregar o banco de dados de parcelas subdivididas
planilhas <- excel_sheets("data/aula10.xlsx")
splitplot <- read_xlsx("data/aula10.xlsx",
                      sheet = planilhas[2]) %>%
  clean_names() %>%
  mutate_at(vars(fam, bloco, micro), as_factor)

## vislumbre do banco de dados
glimpse(splitplot)

## Realizar a análise utilizando o pacote ExpDes.pt.
## função psub2.dic.
fam <- splitplot %>% pull(fam)
micro <- splitplot %>% pull(micro)
bloco <- splitplot %>% pull(bloco)
resp <- splitplot %>% pull(resp)
psub2.dbc(fam, micro, bloco, resp,
          fac.names = c("FAM","MICRO") )

