# Data:12/12/2023
# Programado por: Alan R Panosso

# Carregar os pacotes necessários
library(tidyverse)
library(agricolae)

# Ler o banco de dados geomorfologia.
milho <- read_rds("data/dados_dbc.rds")
glimpse(milho)

# Construir o boxplot para visualizar a variabilidade
# da produção de milho para as diferentes
# variedades
milho %>%
  ggplot(aes(x=trat, y=y)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Realizar a análise de variância para um modelo
# em blocos casualizados para estudar o efeito
# das variedades na produção de milho.
modelo <- aov(y ~ trat + bloco ,
              data = milho %>%
                mutate(trat = as_factor(trat),
                       bloco = as_factor(bloco))
              )
modelo
anova(modelo) # sem efeito.

# Fazer o diagnóstico da análise de variância
## Normalidade dos erros
diag <- milho %>%
  mutate(
    rs_y = rstudent(modelo), # extrair residuos
    pred_y = predict(modelo) # calcular preditos
  )

## Construa o QQ plot
diag %>%
  ggplot(aes(sample=rs_y)) +
  stat_qq() +
  stat_qq_line(color="blue") +
  theme_bw()

## Construa o violin-plot
# FAZER DEPOIS EM CASA

## Estudo de outliers
diag %>%
  ggplot(aes(x = pred_y, y= rs_y)) +
  geom_point() +
  #ylim(-4,4) +
  geom_hline(yintercept = c(-3,3),
             color="red",
             linetype = 2) +
  theme_bw()

## Histograma dos resíduos
diag %>%
  ggplot(aes(x=rs_y, y=..density..)) +
  geom_histogram(bins = 7,color="black",fill="gray") +
  theme_bw()
diag %>% pull(rs_y) %>% shapiro.test()
diag %>% pull(rs_y) %>% nortest::ad.test()
diag %>% pull(rs_y) %>% nortest::cvm.test()
diag %>% pull(rs_y) %>% nortest::lillie.test()

## Homocedasticidade
y <- diag %>% pull(y)
trat <- diag %>% pull(trat) %>% as_factor()

lawstat::levene.test(y, trat)
lawstat::levene.test(y, trat,location = "mean")
bartlett.test(y, trat)
# Conclusão: não rejeitamos H0 ao nível de 5% de probabi-
# lidade e concluímos que os dados de produção de
# milho tem variância comum, ou seja, são
# homocedásticos.

bloco <- milho %>% pull(bloco) %>% as_factor()
ExpDes.pt::dbc(trat,bloco,y,
               mcomp = "tukey")







