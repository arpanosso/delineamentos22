# Carregando os pacotes
library(tidyverse)
library(readxl)

#entrada de dados
aula4 <- read_xlsx("data/aula4.xlsx")

#manipulação dos dados
# utulizando o PIPE (control+shift+M)
# selecionando as três primeiras colunas
aula4 %>%
  select(trat, rep, resp)

# removendo as colunas de origem até dose
aula4 %>%
  select(-(origem:dose))

#renomear colunas
aula4 %>%
  rename(Y=resp, tratamento=trat) %>%
  select(tratamento, rep, Y)

#vamos selecionar apenas os tratamentos de 1 a 3
#filtrar linhas
aula4 %>%
  filter(trat <=3)

#vamos buscar os diferentes de 4
#(comando para diferente !=)
aula4 %>%
  filter(trat !=4)

#selecionar as três primeiras colunas e dose
# comando para igual (==)
aula4 %>%
  filter(dose==3) %>%
  select(trat, rep, resp, dose)

#algumas estatisticas descritivas
# média / var / dp / epm / cv
aula4 %>%
  summarise(
    n=n(),
    media=mean(resp),
    variancia= var(resp),
    desvpad= sd(resp),
    epm= desvpad/sqrt(n),
    #epm2= sd(resp)/sqrt(length(resp)),
    cv= 100*desvpad/media
  )
# estatistica descritiva por tratamento
aula4 %>%
  group_by(trat) %>%
  summarise(
    n=n(),
    media=mean(resp),
    variancia= var(resp),
    desvpad= sd(resp),
    epm= desvpad/sqrt(n),
    #epm2= sd(resp)/sqrt(length(resp)),
    cv= 100*desvpad/media
  )

# selecionar apenas as colunas trat, rep, resp

aula4 %>%
  select(trat, rep, resp) %>%
  mutate(
    dose= case_when(
      trat==1~-3,
      trat==2~-1,
      trat==3~1,
      trat==4~3
    ),
    t_novo=ifelse(trat==1,"test", "novo"),
    mae= ifelse(trat<=2, "A1", "A2"),
    pai= ifelse(trat==1|trat==3, "B1", "B2")
  )
#| - simbolo para um ou outro

#alguns graficos o PIPE e o GGPLOT2
#grafico de dispersão por tratamento
aula4 %>%
  ggplot(aes(x=trat, y=resp))+
  geom_point(color="red", size=4, pch=5)+
  theme_bw()+
  labs(x="Tratamentos",y= "Variavel Y" )+
  geom_smooth(method = "lm", se=FALSE )

#grafico de colunas
aula4 %>%
  group_by(trat) %>%
  summarise(media=mean(resp)) %>%
  ggplot(aes(x=trat, y=media,
             fill=as.factor(trat)))+
  geom_col()

#grafico de boxplot
aula4 %>%
  group_by(trat) %>%
  ggplot(aes(x=trat, y=resp,
             fill=as.factor(trat)))+
  geom_boxplot()

#grafico histograma
aula4 %>%
  ggplot(aes(x=resp, y=..density..))+
  geom_histogram(bins=4, color="black",
                 fill= "gray")+
  geom_density(fill="red", alpha=0.2)+
  theme_classic()












































































