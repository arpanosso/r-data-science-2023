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

# Distribuição Binomial
n <- 3 # o número de realizações
p <- 1/2 # chance de sucesso
x <- 0:n
px <- dbinom(x,n,p)
sum(px)
tibble(x, px) %>%
  mutate(
    acumulado = cumsum(px)
  ) %>%
  ggplot(aes(x=x,y=px)) +
  geom_col(fill="lightblue",
           color="black") +
  theme_bw()

pbinom(2,n,p) # acumula probabilidade até o valor 2
qbinom(0.875,n,p) # retorna o x para uma acumulada de
                  # 0.875

rbinom(10000,n,p) %>%  hist() # gera a variáveis com
                              # a distribuição

# Distribuição amostral da Média
x <- 0:6
fx <- c(1000,50,400,250,320,700,100)

va <- rep(x, fx)
hist(va)
mean(va)
var(va)

n<-5 # tamanho da amostra
ams <- sample(va,n,replace = TRUE) # amostragem com reposição
mean(ams) # média dos valores amostrados

vetor_medias <- 0
for(i in 1:100000){
  ams <- sample(va,n,replace = TRUE)
  vetor_medias[i] <- mean(ams)
}
hist(vetor_medias)
mean(vetor_medias)
var(vetor_medias)

# teste de hipótese
# Ler o banco de dados geomorfologia
geomorfologia <- read_rds("data/geomorfologia.rds")
glimpse(geomorfologia)

arg_sup_1 <- geomorfologia %>%
  filter(sup == "I") %>%
  pull(argila)
mean(arg_sup_1)

arg_sup_2 <- geomorfologia %>%
  filter(sup == "II") %>%
  pull(argila)
mean(arg_sup_2)

# Teste entre I e II
t.test(arg_sup_1,arg_sup_2,
      alternative = "t",
      var.equal = TRUE)

# Teste entre I e III
arg_sup_3 <- geomorfologia %>%
  filter(sup == "III") %>%
  pull(argila)
mean(arg_sup_3)

t.test(arg_sup_1,arg_sup_3,
       alternative = "t",
       var.equal = TRUE)

# Teste entre II e III
t.test(arg_sup_2,arg_sup_3,
       alternative = "t",
       var.equal = TRUE)


# Será o valor médio do teor de argila
# na superfície I igual a 25 ?
t.test(arg_sup_1, mu=25,
       alternative = "l",
       conf.level = 0.99)

#intervalo de confiança.
t.test(arg_sup_1, mu=mean(arg_sup_1),
       alternative = "t",
       conf.level = 0.99)

t.test(arg_sup_1, mu=mean(arg_sup_1),
       alternative = "t",
       conf.level = 0.95)

