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
geomorfologia %>%
  ggplot(aes(x=x, y=argila)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  theme_bw() +
  ggpubr::stat_cor(label.x = 3, label.y = 10) +
  ggpubr::stat_regline_equation(label.x = 3,
                                label.y = 8)


# Gráfico com dois eixos y
# x = X
# y = ARGILA
# y.sec = H_Al
coef <- 3
geomorfologia %>%
  ggplot(aes(x=x)) +
  #geom_line(aes(y=argila), color="red") +
  geom_col(aes(y=argila), fill="gray",color="white") +
  geom_line(aes(y=ph*coef),color="blue") +
  scale_y_continuous(
    name= "Teor de Argila",
    sec.axis = sec_axis(name="pH do solo",
                        trans = ~./coef)
  ) +
  theme(
    axis.title.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue",
                                      angle=90)
  ) +
  theme_bw()

# Criar o gráfico de barras crescente da
# contagem do número de solo
geomorfologia %>%
  group_by(solo) %>%
  count() %>%
  ggplot(aes(x=solo,y=n)) +
  geom_col(color="black",fill="aquamarine4")

# adicionar a coluna perc, para ser udada como peso...
geomorfologia %>%
  group_by(solo) %>%
  count() %>%
  ungroup() %>%
  mutate(perc=n/sum(n),
         solo = solo %>%
           fct_lump(n=6,
                    w=perc,
                    other_level = "Outros") %>%
           fct_reorder(perc)
           ) %>%
  ggplot(aes(x=solo,y=n)) +
  geom_col(color="black",fill="aquamarine4") +
  theme_bw() +
  coord_flip() +
  theme(
    axis.title.x = element_text(size=rel(2)),
    axis.title.y = element_text(size=rel(2)),
    axis.text.y = element_text(size=rel(1.5))
  )
















