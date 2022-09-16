# Vetor com uma posição
x <- 5

# Vetor com 3 posições
y <- c(5, 8, 20)
y[3]
y[1]

# vetor de nomes
nomes <- c("Ana","Paulo","Raul")
nomes[3]

# Extrair o tamanho do vetor
length(y)

## Carregar um banco de dados no R
aula2 <- read.table("data/aula2.txt",
                    h = TRUE,
                    sep = "\t",
                    dec = ",")

aula2

## vamos acessar a coluna tratamento
aula2$TRAT

### acessar qa coluna RESP
aula2$RESP

## Construir y2 que é a RESP ao quadrado
aula2$Y2 = aula2$RESP^2
aula2

## calcular as médias por tratamento
media <- tapply(aula2$RESP, aula2$TRAT, mean)
aula2$Modelo_media = rep(media,c(2,2,2,2))
aula2

## Calculando o Desvio
aula2$Desvio = aula2$RESP - aula2$Modelo_media
aula2$Desvio2 = aula2$Desvio^2
aula2

## calculo das somas:
sum(aula2$RESP)
sum(aula2$Y2)
sum(aula2$Desvio2)

# tranformando a Coluna TRAT para fator
aula2$TRAT = as.factor(aula2$TRAT)

# Realizar a análise de variância pelo modelo
# de média.
modelo <- aov(RESP ~ TRAT, data=aula2)
anova(modelo)

## Análise para o Modelo linear (reta)
reg_1 <- lm(RESP ~ DOSE, data=aula2)
summary.lm(reg_1)


plot(aula2$DOSE, aula2$RESP)
abline(reg_1)


## Modelo quadrático
aula2$DOSE2 = aula2$DOSE^2
reg_2 <- lm(RESP ~ DOSE + DOSE2, aula2)
summary.lm(reg_2)

plot(aula2$DOSE, aula2$RESP,
     ylab = "Y", xlab="Doses",
     col="blue",pch=16,cex=2)
curve(3.6750 + 4.9650*x -0.7750*x^2,
      add=TRUE,col="red")


plot(aula2$TRAT, aula2$RESP)

anova(modelo,reg_1,reg_2)
