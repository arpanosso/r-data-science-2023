# Data:07/12/2023
# Programado por:Alan R Panosso
library(readxl)
library(tidyverse)

# Leia o arquivo geomorfologia.rds
geomorfologia <- read_rds("data/geomorfologia.rds")
geomorfologia %>% glimpse()

# Faça um gráfico de linhas do teor de argila em
# função de x
# Gráfico de Colunas
geomorfologia %>%
  ggplot(aes(x=x, y=argila)) +
  geom_col(color="white",fill="darkgreen")

# Gráfico de Barras
geomorfologia %>%
  ggplot(aes(x=x, y=argila)) +
  geom_col(color="white",fill="blue") +
  coord_flip()

# Gráfico de linhas
geomorfologia %>%
  ggplot(aes(x=x, y=argila)) +
  geom_line(color = "red", lwd = 1,
            linetype = 3)+
  geom_point(color="blue") +
  theme_bw()

# Mapeie o tipo de solo por cor, e insira a
# superfície como texto (label)
## Adicione linhas verticais para demarcar as
## diferentes superfícies
## adicione anotação definindo as superficies
sups <- geomorfologia %>%
  pull(sup) %>%
  unique()
sups <- paste0("Sup: ",sups)

geomorfologia %>%
  ggplot(aes(x=x, y=argila, color=solo, label=sup)) +
  geom_text() +
  geom_vline(xintercept = c(415,1960),
             linetype = 2, color = "darkgray") +
  theme_bw() +
  annotate(geom = "text",
           x=c(120, 1250, 2250),
           y=c(30,30,30),
           label=sups,
           size=5
           )+
  scale_color_viridis_d()

# Construir a matriz de correlação
# entre as variáveis numéricas
geomorfologia %>%
  select(where(is.numeric),
         -c(amostra,x)) %>%
  cor() %>%
  corrplot::corrplot()

# Escolha duas variáveis, construa o gráfico de
# dispersão entre elas e execute um teste no
# coeficiente de correlação e posterior análise
# de regresão linear
geomorfologia %>%
  ggplot(aes(x=argila, y=ph, color=sup)) +
  geom_point() +
  geom_smooth(method = "lm",
              se=FALSE) +
  facet_wrap(~sup,scales = "free",nc=2)

# Separando por superfície
## teste no coeficiente de correlação
geomorfologia %>%
  select(sup, argila, ph) %>%
  filter(sup == "III") %>%
  cor.test(~ argila + ph,
           data = .)

## Análise de Regressão
geomorfologia %>%
  select(sup, argila, ph) %>%
  filter(sup == "III") %>%
  lm(ph ~ argila, data = .) %>%
  summary.lm()

# construa os histograma para os teores de argila
# em cada superficie e realize os teste de
# normalidade.
vsup <- "III"
geomorfologia %>%
  filter(sup == vsup) %>%
  pull(argila) %>%
  shapiro.test()

geomorfologia %>%
  filter(sup == vsup) %>%
  pull(argila) %>%
  nortest::lillie.test()

geomorfologia %>%
  filter(sup == vsup) %>%
  pull(argila) %>%
  nortest::cvm.test()

geomorfologia %>%
  filter(sup == vsup) %>%
  pull(argila) %>%
  nortest::ad.test()

## Estruturas de repetição
## gerar sequências
# criar uma sequência de 1 a 10
1:10

# criar uma sequência de 0 a 20 de 2 em 2
seq(0,20,2)

# criar uma sequência de 100 a 50 de 5 em 5
seq(100,50,-5)

# Tabuada do 7 usando o for
for(i in 1:10){
  print(paste0("7 x ",i," = ", i*7))
}

# Tabuada do 7 usando a função map, pacote purrr
purrr::map_vec(1:10,
           ~paste0("7x", .x, "=",.x*7))

## Construir gráfico de dispersão para cada
## superfície a partir das variáveis selecionadas
## anteriormente usando o for
sups <- geomorfologia %>%
  pull(sup) %>%
  unique()

for(i in 1:3){
  filtro_sup <- sups[i]
  my_plot <- geomorfologia %>%
    filter(sup == filtro_sup) %>%
    ggplot(aes(x=argila, y=ph))+
    geom_point()+
    labs(title = filtro_sup)
  print(my_plot)
}

# usando a map do pacote purrr.
purrr::map(sups,
           ~{
             filtro_sup <- .x
             geomorfologia %>%
               filter(sup == filtro_sup) %>%
               ggplot(aes(x=argila, y=ph))+
               geom_point()+
               labs(title = filtro_sup)
           })




