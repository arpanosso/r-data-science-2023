# Data:06/12/2023
# Programado por: Alan R Panosso

# Pacotes necessários:
library(tidyverse)
library(readxl)
source("R/minhas-funcoes.R")

# Valores perdidos - NAs --------------------------------------------------
# Valores faltantes NA
## Cuidado pois são fontes de propagação
## de ERRO no código.
NA ## Valor perdido, não disponível

# Criando um vetor, aplicando a função média
## NA não funciona com operadores relacionais
## Função is.na()
## Contar o número de NAs no vetor y
## Contar o número de não NAs no vetor y
x <- 360
is.vector(x)
is.numeric(x)
is.data.frame(x)
is.character(x)

y <- c(24, 28, 32, 14, 16)
mean(y)
y[3] <- NA
mean(y)
4*4
NA*4
y >=10
mean(y, na.rm = TRUE)
# y == NA
sum(is.na(y)) #contando NAs
sum(!is.na(y)) #conta os não NAs

## Função para contar NA
contar_nas(y)

# Faxina dos dados geomorfologia ------------------------------------------
# Realizar a faxina nos dados de geomorfologia
library(readxl)
geomorfologia <- read_excel(
  "data-raw/geomorfologia.xlsx") %>%
  janitor::clean_names()

# Quantas superfícies e tipos de solo estão presentes na base?
geomorfologia %>%
  pull(sup) %>%
  unique()

geomorfologia %>%
  pull(solo) %>%
  unique()

## Observar a estrutura dos dados
geomorfologia %>% str()
geomorfologia %>% glimpse()
# install.packages("skimr")
geomorfologia %>%  skimr::skim()

# alterar o nome da variável relacao_silte_argila para
# silte_argila
# alterar o nome da variável p para p_resina
# altear o nome da variável t para ctc
geomorfologia <- geomorfologia %>%
  rename(
    silte_argila = relacao_silte_argila,
    p_resina = p,
    ctc = t,
    ph = p_h
  )
geomorfologia %>% glimpse()

# Salvar uma versão em rds na pasta data
write_rds(geomorfologia,"data/geomorfologia.rds")

# leia novamente esse arquivo rds...
geomorfologia <- read_rds("data/geomorfologia.rds")

# Verbos do dyplr ---------------------------------------------------------
# VERBO select
# selecionar solo, sup, x e p_resina
geomorfologia %>%
  select(sup, solo, x, p_resina)

# selecionar de p_resima a v
geomorfologia %>%
  select(p_resina:v)

# selecionar todas menos sup, solo, x e amostra
geomorfologia %>%
  select(- (sup:x) )

# VERBOS summarise e group_by
# Calcule a média para cada variável numérica
geomorfologia %>%
  summarise(across(amg:v, ~mean(.)))

geomorfologia %>%
  summarise(across(where(is.numeric), mean))

# Calcule um resumo estatístico para cada variável numérica
geomorfologia %>%
  summarise(across(where(is.numeric),
                   resumo_estatistico))

# Calcule a média do pH para cada superfície.
geomorfologia %>%
  group_by(sup) %>%
  summarise(media_ph = mean(ph, na.rm = TRUE))

# Calcule a média e variância da argila para cada solo.
geomorfologia %>%
  group_by(solo) %>%
  summarise(
    media_argila = mean(argila, na.rm = TRUE),
    variancia_argila = var(argila, na.rm = TRUE)
  )

# Calcule o resumo estatistico da argila para cada solo.
geomorfologia %>%
  group_by(solo) %>%
  summarise(
    esta_desc_argila = resumo_estatistico(argila)
  )


# VERBO filter, vamos filtrar:
# Todas as variáveis para a SUP == "I"

# Vamos selecionar somente os solos do tipo LV

# Vamos selecionar somente os solos do tipo LV com teor de
# argila maiores ou iguais a 20%

#VERBO mutate
# Criar a variável ARGILA+SILTE

# Passar a CTC para a escala logarítmica

# Classificar a CTC média (>=15) e baixa (<15)

# Classificar a CTC média (>=15) e baixa (<15 e >=6) e muito baixa (<6)

# vamos eliminar as 4 últimas colunas

# criando um novo banco de dados

# Ordenar o banco de dados por teor de a argila do menor para o
# maior

# Ordenar o banco de dados por teor de argila do maior para o
# menor




