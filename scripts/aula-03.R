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
geomorfologia %>%
  filter(sup == "I") %>%
  View()

# Vamos selecionar somente os solos do tipo LV
geomorfologia %>%
  filter(solo == "LV") %>%
  View()

# Vamos selecionar todos os PVs
geomorfologia %>%
  filter( str_detect(solo,"PV|R") ) %>%
  View()

# Vamos selecionar somente os solos do tipo LV
# com teor de argila maiores ou iguais a 20%
geomorfologia %>%
  filter(solo == "LV", argila >= 20) %>%
  select(solo, argila) %>%
  View()

geomorfologia %>%
  filter(solo == "LV" & argila >= 20) %>%
  select(solo, argila) %>%
  View()

# Faça um filtro para todos os PV com argila
# >= a 20 ou os LVs com argila <= 10.
geomorfologia %>%
  filter(solo == "LV" & argila >= 20 |
           str_detect(solo,"PV") & argila <= 10
           ) %>%
  View()

#VERBO mutate
# Criar a variável ARGILA+SILTE
geomorfologia %>%
  mutate(
    argila_silte = argila + silte
  ) %>% View()

# Passar a CTC para a escala logarítmica
geomorfologia %>%
  mutate(
    ctc_log10 = log10(ctc)
  ) %>% View()

# Classificar a CTC média (>=15) e baixa (<15)
geomorfologia %>%
  mutate(
    ctc_class = ifelse(ctc >= 15,"média","baixa")
  ) %>% View()

# Classificar a CTC média (>=15) e baixa
# (<15 e >=6) e muito baixa (<6)
geomorfologia %>%
  mutate(
    ctc_class = case_when(
      ctc < 6 ~ "muito baixa",
      ctc < 15 ~ "baixa",
      ctc >= 15 ~ "média"
    )
  ) %>%
  View()

# vamos eliminar as 4 últimas colunas
geomorfologia %>% names()
geomorfologia %>%
  select( -(h_al:v)) %>%
  View()

# Ordenar o banco de dados por teor de a
# argila do menor para o maior
geomorfologia %>%
  arrange(argila) %>%
  select(amostra, argila) %>%
  View()

# Ordenar o banco de dados por teor de
# argila do maior para o menor
geomorfologia %>%
  arrange(desc(argila)) %>%
  select(amostra, argila) %>%
  View()
# Histograma para teor de argila
geomorfologia %>%
  filter(sup == "I") %>%
  ggplot(aes(x=argila, y = ..density..)) +
  geom_histogram(
    bins = 7, # número de colunas do hist
    color="black", # cor da borda
    fill="gray", #preenchimento da coluna
    ) +
  labs(title="Meu Gráfico",
       x = "Teor de Argila") +
  geom_density(color="red",
               fill="blue",
               alpha = .10) +
  theme_minimal()


# Histograma para teor de areia
geomorfologia %>%
  mutate(areia = amg+ag+am+amf+af) %>%
  filter(sup == "I") %>%
  ggplot(aes(x=areia, y = ..density..)) +
  geom_histogram(
    bins = 7, # número de colunas do hist
    color="black", # cor da borda
    fill="pink", #preenchimento da coluna
  ) +
  labs(title="Meu Gráfico",
       x = "Teor de Areia") +
  geom_density(color="red",
               fill="yellow",
               alpha = .10) +
  theme_minimal()


# Histograma para teor de silte
geomorfologia %>%
  filter(sup == "I") %>%
  ggplot(aes(x=silte, y = ..density..)) +
  geom_histogram(
    bins = 7, # número de colunas do hist
    color="black", # cor da borda
    fill="aquamarine4", #preenchimento da coluna
  ) +
  labs(title="Meu Gráfico",
       x = "Teor de Silte") +
  geom_density(color="red",
               fill="purple",
               alpha = .10) +
  theme_minimal()

# Boxplot
geomorfologia %>%
  ggplot(aes(y=argila, x=sup)) +
  geom_boxplot()

geomorfologia %>%
  ggplot(aes(y=argila, x=solo, fill=sup)) +
  geom_boxplot()

geomorfologia %>%
  ggplot(aes(y=argila, x=solo, fill=sup)) +
  geom_boxplot()+
  facet_wrap(~sup, nr=2, scale="free") +
  ylim(0,30)+
  scale_fill_viridis_d() +
  theme_bw() +
  labs(fill="Superfície") +
  theme(
    legend.position = "top"
  )
for(i in 1:10){
  print("a vida é bela!!")
  print(i)
}

# Imprimir o boxplot da argila
# para cada solo



solo_filtro <- geomorfologia %>%
  pull(solo) %>%
  unique()

for(i in 1:8){
  fs <- solo_filtro[i]
  meu_box <- geomorfologia %>%
    filter(solo == fs) %>%
    ggplot(aes(y=argila)) +
    geom_boxplot(fill="orange")+
    labs(title = fs)
  print(meu_box)
}














































