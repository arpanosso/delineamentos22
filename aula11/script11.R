library(ExpDes.pt)
library(patchwork)
library(tidyverse)
library(agricolae)
library(readxl)
library(nlme)
library(lme4)
library(janitor)

# entrada de dados
dados <- read_xlsx("data/aula11.xlsx",
                   sheet = "Cigarrinha") %>%
  clean_names() %>%
  rename(cultivar = fa, cigarrinha = fb) %>%
  mutate_at(vars(tr, cultivar, cigarrinha), as_factor)
glimpse(dados)




# Análise Diagnóstico Clorofila -------------------------------------------
dados %>%
  group_by(tr) %>%
  ggplot(aes(y=z, x=tr)) +
  geom_boxplot()


# Teste de Homocedasticidade
library(lawstat)
levene.test(dados$z,dados$tr)
levene.test(dados$z,dados$tr,
            location = "mean")

# testar as transformações potência
dados %>%
  group_by(tr) %>%
  summarise(
    media = mean(z, na.rm=TRUE),
    dp = sd(z, na.rm=TRUE),
    log_media = log10(media),
    log_dp = log10(dp)
  ) %>%
  ggplot(aes(x=log_media, y=log_dp)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)

# Análise de ajuste linear entre log_dp vrsus log_media
lm(log_dp ~ log_media,
   data=dados %>%
     group_by(tr) %>%
     summarise(
       media = mean(z, na.rm=TRUE),
       dp = sd(z, na.rm=TRUE),
       log_media = log10(media),
       log_dp = log10(dp)
     )) %>%  summary

### Aplicação da transformação de Box-Cox
mod <- aov(z ~ bl + tr,
           data=dados)
library(MASS)
boxcox(mod)

## teste de normalidade dos resíduos
dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(z ~ tr + bl))
  ) %>%
  ggplot(aes(x=rs, y=..density..)) +
  geom_histogram(bins=10, color="black",fill="gray") +
  geom_density(fill="red", alpha=.12)

# Extraindo os resíduos
rs <- dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(z ~ bl + tr))
  ) %>% pull(rs)
shapiro.test(rs)

library(nortest)
ad.test(rs)
cvm.test(rs)
lillie.test(rs)

## construir o qqplot
dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(z ~ bl + tr))
  ) %>%
  ggplot(aes(sample = rs)) +
  stat_qq() +
  stat_qq_line()

# Estudo dos outliers

dados %>%
  drop_na() %>%
  mutate(
    rs = aov(z ~ tr + bl) %>% rstudent,
    yp = aov(z ~ tr + bl) %>% predict()
  ) %>%
  ggplot(aes(x=yp,y=rs)) +
  geom_point() +
  coord_cartesian(ylim=c(-4,4)) +
  geom_hline(yintercept = c(-3,3), color="red")

## identificando os outliers

dados <- dados %>%
  drop_na() %>%
  mutate(
    rs = aov(z ~ tr + bl) %>% rstudent,
    yp = aov(z ~ tr + bl) %>% predict()
  ) %>%
  mutate(
    z2 = ifelse(rs < -3 | rs > 3, yp, z)
  )

### refazendo a análise de normalidade dos
### com Z2
## teste de normalidade dos resíduos
dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(z2 ~ tr + bl))
  ) %>%
  ggplot(aes(x=rs, y=..density..)) +
  geom_histogram(bins=10, color="black",fill="gray") +
  geom_density(fill="red", alpha=.12)

# Extraindo os resíduos
rs <- dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(z2 ~ bl + tr))
  ) %>% pull(rs)
shapiro.test(rs)

library(nortest)
ad.test(rs)
cvm.test(rs)
lillie.test(rs)

## construir o qqplot
dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(z ~ bl + tr))
  ) %>%
  ggplot(aes(sample = rs)) +
  stat_qq() +
  stat_qq_line()

# Estudo dos outliers

dados %>%
  drop_na() %>%
  mutate(
    rs = aov(z ~ tr + bl) %>% rstudent,
    yp = aov(z ~ tr + bl) %>% predict()
  ) %>%
  ggplot(aes(x=yp,y=rs)) +
  geom_point() +
  coord_cartesian(ylim=c(-4,4)) +
  geom_hline(yintercept = c(-3,3), color="red")

## Análise de variância
mod <- aov(z2 ~ bl + cultivar*cigarrinha,
           data = dados)
anova(mod)


# Análise Diagnóstico Peso Planta -------------------------------------------
glimpse(dados)
dados %>%
  group_by(tr) %>%
  ggplot(aes(y=y, x=tr)) +
  geom_boxplot()


# Teste de Homocedasticidade
library(lawstat)
levene.test(dados$y,dados$tr)
levene.test(dados$y,dados$tr,
            location = "mean")

# testar as transformações potência
dados %>%
  group_by(tr) %>%
  summarise(
    media = mean(y, na.rm=TRUE),
    dp = sd(y, na.rm=TRUE),
    log_media = log10(media),
    log_dp = log10(dp)
  ) %>%
  ggplot(aes(x=log_media, y=log_dp)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)

# Análise de ajuste linear entre log_dp vrsus log_media
lm(log_dp ~ log_media,
   data=dados %>%
     group_by(tr) %>%
     summarise(
       media = mean(y, na.rm=TRUE),
       dp = sd(y, na.rm=TRUE),
       log_media = log10(media),
       log_dp = log10(dp)
     )) %>%  summary

### Aplicação da transformação de Box-Cox
mod <- aov(y ~ bl + tr,
           data=dados)
library(MASS)
boxcox(mod)

## teste de normalidade dos resíduos
dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(y ~ tr + bl))
  ) %>%
  ggplot(aes(x=rs, y=..density..)) +
  geom_histogram(bins=10, color="black",fill="gray") +
  geom_density(fill="red", alpha=.12)

# Extraindo os resíduos
rs <- dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(y ~ bl + tr))
  ) %>% pull(rs)
shapiro.test(rs)

library(nortest)
ad.test(rs)
cvm.test(rs)
lillie.test(rs)

## construir o qqplot
dados %>%
  drop_na() %>%
  mutate(
    rs = rstudent(aov(y ~ bl + tr))
  ) %>%
  ggplot(aes(sample = rs)) +
  stat_qq() +
  stat_qq_line()

# Estudo dos outliers
dados %>%
  drop_na() %>%
  mutate(
    rs = aov(y ~ tr + bl) %>% rstudent,
    yp = aov(y ~ tr + bl) %>% predict()
  ) %>%
  ggplot(aes(x=yp,y=rs)) +
  geom_point() +
  coord_cartesian(ylim=c(-4,4)) +
  geom_hline(yintercept = c(-3,3), color="red")

## identificando os outliers

dados <- dados %>%
  drop_na() %>%
  mutate(
    rs = aov(z ~ tr + bl) %>% rstudent,
    yp = aov(z ~ tr + bl) %>% predict()
  ) %>%
  mutate(
    y2 = ifelse(rs < -3 | rs > 3, yp, z)
  )

## Análise de variância
mod <- aov(y ~ bl + cultivar*cigarrinha,
           data = dados)
anova(mod)
