# Exibir os pacotes ativos no ambiente
(.packages())

# Listar todos os pacotes instalados no
# computador
(.packages(all.available = TRUE))

# Vamos instalar os pacotes
## agricolae
## ExpDes.pt

## carregar os pacotes instalados
library(agricolae)
library(ExpDes.pt)

## Ajuda sobre um pacote
help(agricolae)
??agricolae
? "agricolae"

## Exemplo de Análise de variância
## Lista de Exerícios 03
## Entrada de dados do Excel
library(readxl)
aula3 <- read_excel("data/aula3.xlsx")
aula3

modelo <- aov(Y ~ Trat, data=aula3)
anova(modelo)

# Comparação Múltiplas
# teste LSD
saida <- LSD.test(modelo,"Trat",group = TRUE,
         console = TRUE)

plot(saida)


# teste de Tukey
saida <- HSD.test(modelo,"Trat",group = TRUE,
                  console = TRUE)
plot(saida)

## Análise de variãncia e comparação
# de médias utilizando o ExpDes.pt

trat <- aula3$Trat
resp <- aula3$Y
dose <- aula3$Dose

dic(trat, resp, quali = TRUE,
    mcomp = "tukey")

## Ajustes
dic(dose, resp,
    quali = FALSE)

aula3[27,4] <- NA
View(aula3)








