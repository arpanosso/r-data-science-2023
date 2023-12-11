# Data: 11/12/2023
# Programado por: Alan R Panosso
library(tidyverse)
library(ggpubr) # install.packages("ggpubr")
source("R/minhas-funcoes.R")

# Ler o banco de dados geomorfologia
geomorfologia <- read_rds("data/geomorfologia.rds")
glimpse(geomorfologia)

#  Criar uma tabela de contagem do número de
# observações em cada categoria de SUP.
# Utilize a função count()
geomorfologia %>%
  group_by(solo) %>%
  count() %>%
  ungroup() %>%
  mutate(
    perc = n/sum(n)
  )


# Tabela de contagem, sup e solo
geomorfologia %>%
  group_by(sup, solo) %>%
  count() %>%
  ungroup(solo) %>%
  mutate(
    perc = n / sum(n)
  )

# construir o gráfico de distribuição para
# teor de argila
geomorfologia %>%
  pull(argila) %>%
  range() %>%
  diff()

geomorfologia %>%
  mutate(
    class_argila = cut(argila,7)
  ) %>%
  arrange(argila) %>%
  count(class_argila) %>%
  ggplot(aes(x=class_argila, y=n)) +
  geom_col(color="black",fill="gray") +
  theme_bw()

# Construir um gráfico de colunas de superfície e
# tipos de solo y é a contagem...
geomorfologia %>%
  group_by(sup, solo) %>%
  summarise(
    n=n()
  ) %>%
  ggplot(aes(x=sup,y=n,fill=solo)) +
  geom_col(color="black",
           position = "dodge") +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(title = "Gráfico de Colunas") +
  meu_tema()



# Construir o gráfio de dispersão e
# adicionar a linha e respectiva equação,
# use o pacote ggpubr

# Gráfico com dois eixos y
# x = X
# y = ARGILA
# y.sec = H_Al

# Criar o gráfico de barras crescente da
# contagem do número de solo






