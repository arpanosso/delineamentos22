# Aula 01 - Introdução Ao R
# Operações aritméticas
8*7
4/2
14/3
14 %/% 3 # inteiro da divisão
14 %% 3 # resto da divisão
sqrt(225) # raíz quadrada
5^2 # potenciação
5**2 # potenciação
log(10) # logarítmo natural
exp(1) # função exponencial
log(3,4) # log de 3 na base 4

# Tipos Primitivos
## Caracter
"aluno"
aluno # erro, não temos o objeto aluno
'1000' # é um caracter


## Inteiro
1
5
-12

## Real - Double
pi
2.7893

## Lógico
3 > 5 # operação relacional
25/5 >= 5 
0.5 == 1/2 # teste de igualdade
3 != 4

## Atribuição com <- ou =
x <- 3
x
x*4
X <- 5
x <- 7

## igualdade para passar argumentos
log(x = 3, base = 4)


# Vamos criar o vetor de alturas

altura <- c(160, 165, 158, 184, 159, 171)
idade <- c(26, 30, 27, 29, 25, 29)

# Média
mean(altura)
mean(idade)

# variância amostral
var(altura)
var(idade)

# desvio padrão amostral
sd(altura)
sd(idade)

## Coeficiente de variação
100*sd(altura) / mean(altura)
100*sd(idade) / mean(idade)

## Criar uma função que faz o meu CV
meu_cv <- function(x){
  100*sd(x)/mean(x)
}
meu_cv(altura)
meu_cv(idade)















