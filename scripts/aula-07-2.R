# Data: 13/12/2023
# Programado por: Alan R Panosso
# Carregar os pacotes
library(tidyverse)


# Análise 01 --------------------------------------------------------------
# Análise de variância de um experimento fatorial com 2
# fatores com interação não significativa

# Para a obtenção da análise de variância, vamos
# utlizar os dados adaptados do trabalho “Ensaios em
# condições de casa-de-vegetação para controle químico
# do ‘damping-off’ em Eucalyptus saligna Sm.”,
# realizado por KRUGNER; CARVALHO (1971) e publicado em
# IPEF, n 2/3 p. 97-113. O ensaio foi realizado no
# delineamento inteiramente casualizado, com 3
# repetições e foram estudados os efeitos sobre a altura
# média das mudas de Eucalytus saligna, dos fatores:
# Tratamento do solo (S), sendo:
# S1=Vapam
# S2=Brometo de metila
# S3=PCNB
# S4=Testemunha
#
# Pulverização com fungicida em pós emergência, sendo:
# F0=Sem fungicida
# F1=Com fungicida

# Entrar com os dados
altura_mudas <-read_rds("data/altura_mudas_dic.rds")

# Criar tabela de médias
glimpse(altura_mudas)
tab_media <- altura_mudas %>%
  group_by(trat_solo, fungicida) %>%
  summarise(
    media = mean(y, na.rm = TRUE)
  )

# criar gráfico da interação
tab_media %>%
  ggplot(aes(x=trat_solo,y=media,
             color=as_factor(fungicida))) +
  geom_point()+
  geom_line()

tab_media %>%
  ggplot(aes(x=fungicida,y=media,
             color=as_factor(trat_solo))) +
  geom_point()+
  geom_line()


# Realizar a Análise de Variância
trat_solo <- altura_mudas %>%
  pull(trat_solo) %>%
  as_factor()

fungicida <- altura_mudas %>%
  pull(fungicida) %>%
  as_factor()

y <- altura_mudas %>%
  pull(y)
ExpDes.pt::fat2.dic(trat_solo,fungicida,y,
                    fac.names = c("TS","Fung"))

# Realizar o Diagnostico da ANOVa utilizando o delineamento
# de tratamentos (DIC no caso)
# Construir o boxplot para visualizar a variabilidade
# d Y para as diferentes trats
altura_mudas <- altura_mudas %>%
  mutate(
    trat = interaction(trat_solo,fungicida)
  )

altura_mudas %>%
  ggplot(aes(x=trat, y=y)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# de trat nos valores de y.
modelo <- aov(y ~ trat,
              data = altura_mudas %>%
                mutate(
                  trat = as_factor(trat),
                  # y_t = y^(1-1.5423/2),
                  # y_bc = log(y)
                )
)
modelo
anova(modelo) # sem efeito.

## Normalidade dos erros
diag <- altura_mudas %>%
  mutate(
    rs_y = rstudent(modelo), # extrair residuos
    pred_y = predict(modelo) # calcular preditos
  ) #%>%
#select(sup, argila, rs_argila, pred_argila)

## Construa o QQ plot
diag %>%
  ggplot(aes(sample=rs_y)) +
  stat_qq() +
  stat_qq_line(color="blue") +
  theme_bw()

## Estudo de outliers
diag %>%
  ggplot(aes(x = pred_y, y= rs_y)) +
  geom_point() +
  # ylim(-4,4) +
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
y_bc <- log(y)
lawstat::levene.test(y, trat)
lawstat::levene.test(y, trat,location = "mean")
bartlett.test(y, trat)
# Conclusão:
ExpDes.pt::dic(trat,y_t,mcomp = "tukey")

## Realizar o teste de Bartlett para verificar
## a relação da média e da variância.
## transformação se coef beta for significativo
## heterocedasticidade regular
## y_t = y^(1-beta/2)

### Construir a tabela de médias e variâncias
df_aux <- diag %>%
  group_by(trat) %>%
  summarise(
    media = mean(y, na.rm = TRUE),
    variancia = var(y, na.rm = TRUE),
    log_media = log(media),
    log_var = log(variancia)
  )

df_aux %>%
  ggplot(aes(x=log_media,y=log_var))+
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se=FALSE)

modelo_reg <- lm(log_var ~ log_media,
                 data = df_aux)
summary.lm(modelo_reg)
## Conclusão:

## Trasnformação de Box and Cox
MASS::boxcox(modelo)
obj <- MASS::boxcox(modelo)
str(obj)

as.tibble(obj) %>%
  arrange(desc(y))

## Estudar a melhor transformação pelo Box-Cox
## método de Box-Cox
### se lambda não difere de 1 ~ homocedásticos
### se lambda difere de 1:
### se lambda não difere de 0 y_t=log(y)
### se lambda difere de 0 y_t=y^LAMBDA
# Conclusão:






# Análise 2 ---------------------------------------------------------------
# Análise de variância de um experimento fatorial com 2
# fatores com interação significativa

# Para obtenção da análise de variância, vamos supor o
# seguinte ensaio em que foram estudados os efeitos de
# 4 inseticidas em 2 doses diferentes sobre a produção da
# cultura do milho em kg/parcela.
# Entrar com os dados
inset_dose <-read_rds("data/milho_inset_dose.rds") %>%
  mutate(
    trat = interaction(inseticida,dose)
  )

# Criar tabela de médias
tab_media <- inset_dose %>%
  group_by(inseticida,dose) %>%
  summarise(
    media = mean(y,na.rm = TRUE)
  )

# criar gráfico da interação
tab_media %>%
  ggplot(aes(x=inseticida, y =media,
             color=as_factor(dose))) +
  geom_point()+
  geom_line()+
  theme_bw()


tab_media %>%
  ggplot(aes(x=dose, y =media,
             color=as_factor(inseticida))) +
  geom_point()+
  geom_line()+
  theme_bw()

# Realizar o Diagnostico da ANOVa utilizando o delineamento
# de tratamentos (DIC no caso)
# Realizar a Análise de Variância
inseticida <- inset_dose %>% pull(inseticida) %>%
  as_factor()

dose <- inset_dose %>% pull(dose) %>%
  as_factor()

y <- inset_dose %>% pull(y)

ExpDes.pt::fat2.dic(inseticida,dose,y,
                    fac.names = c("Ins","Dose"))
