# Data:12/12/2023
# Programado por: Alan R Panosso

# Carregar os pacotes necessários
library(tidyverse)
library(agricolae)

# Ler o banco de dados geomorfologia.
geomorfologia <- read_rds("data/geomorfologia.rds")
glimpse(geomorfologia)

# Construir o boxplot para visualizar a variabilidade
# do teor de argila para as diferentes superfícies
geomorfologia %>%
  ggplot(aes(x=sup, y=argila)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Comparar a variância do teor de argila do solo
# para as superfícies, duas a duas.
arg_sup_1 <- geomorfologia %>%
  filter(sup == "I") %>%
  pull(argila)

arg_sup_2 <- geomorfologia %>%
  filter(sup == "II") %>%
  pull(argila)

arg_sup_3 <- geomorfologia %>%
  filter(sup == "III") %>%
  pull(argila)


# Fazer o test F para sup 1 vs 2
var.test(arg_sup_2,arg_sup_1,
         alternative = "g",
         conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que a variância do teor de argi-
# la é maior para a superfície 2. Ou seja, são
# heterocedásticas.

# Fazer o test F para sup 1 vs 3
var.test(arg_sup_3,arg_sup_1,
         alternative = "g",
         conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que a variância do teor de argi-
# la é maior para a superfície 3. Ou seja, são
# heterocedásticas.

# Fazer o test F para sup 2 vs 3
var.test(arg_sup_2,arg_sup_3,
         alternative = "g",
         conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que a variância do teor de argi-
# la é maior para a superfície 2. Ou seja, são
# heterocedásticas.

# Teste de comparação de médias.
t.test(arg_sup_1, arg_sup_2,
       alternative = "t",
       var.equal = FALSE,
       conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que as médias do teor de argila
# para as superfícies I e II são diferentes.

# Construa o histograma para argila em cada superfície
# realize o teste de normalidade:
# alpha = 5%, ou seja, se p<0.05 rejeitamos H0
# H0: Dados tem distribuição normal
# H1: Dados não tem ditribuição normal
## Superfície I
geomorfologia %>%
  filter(sup == "I") %>%
  ggplot(aes(x=argila, y=..density..)) +
  geom_histogram(bins=7,color="black",fill="linen")
arg_sup_1 %>%  shapiro.test()
arg_sup_1 %>%  nortest::ad.test()
arg_sup_1 %>%  nortest::cvm.test()
arg_sup_1 %>%  nortest::lillie.test()
# Conclusão: Não rejeitamos H0 ao nível de 1% de sig-
# nificância e concluímos que os dados de teor de
# argila para superfície I seguem distribuição
# normal.

## Superfície II
geomorfologia %>%
  filter(sup == "II") %>%
  ggplot(aes(x=argila, y=..density..)) +
  geom_histogram(bins=15,color="black",fill="orange")
arg_sup_2 %>%  shapiro.test()
arg_sup_2 %>%  nortest::ad.test()
arg_sup_2 %>%  nortest::cvm.test()
arg_sup_2 %>%  nortest::lillie.test()
# Conclusão: Rejeitamos H0 ao nível de 1% de sig-
# nificância e concluímos que os dados de teor de
# argila para superfície II não seguem distribuição
# normal.

## Superfície III
geomorfologia %>%
  filter(sup == "III") %>%
  ggplot(aes(x=argila, y=..density..)) +
  geom_histogram(bins=15,color="black",fill="salmon")
arg_sup_3 %>%  shapiro.test()
arg_sup_3 %>%  nortest::ad.test()
arg_sup_3 %>%  nortest::cvm.test()
arg_sup_3 %>%  nortest::lillie.test()
# Conclusão: Não rejeitamos H0 ao nível de 1% de sig-
# nificância e concluímos que os dados de teor de
# argila para superfície III seguem distribuição
# normal.

# Calcule a interprete os coeficientes de assimetria
# e curtose para cada superfície.
arg_sup_1 %>% agricolae::skewness()
arg_sup_1 %>% agricolae::kurtosis()

arg_sup_2 %>% agricolae::skewness()
arg_sup_2 %>% agricolae::kurtosis()

arg_sup_3 %>% agricolae::skewness()
arg_sup_3 %>% agricolae::kurtosis()

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# das superfícies nos teores de argila do solo.
modelo <- aov(argila_t ~ sup,
              data = geomorfologia %>%
                mutate(sup = as_factor(sup),
                       argila_t = argila^(0.626))
              )
modelo
anova(modelo) # sem efeito.

# Fazer o diagnóstico da análise de variância
## Normalidade dos erros
diag <- geomorfologia %>%
  mutate(
    rs_argila = rstudent(modelo), # extrair residuos
    pred_argila = predict(modelo) # calcular preditos
  ) %>%
  select(sup, argila, rs_argila, pred_argila)

## Construa o QQ plot
diag %>%
  ggplot(aes(sample=rs_argila)) +
  stat_qq() +
  stat_qq_line(color="blue") +
  theme_bw()

## Construa o violin-plot
# FAZER DEPOIS EM CASA

## Estudo de outliers
diag %>%
  ggplot(aes(x = pred_argila, y= rs_argila)) +
  geom_point() +
  ylim(-4,4) +
  geom_hline(yintercept = c(-3,3),
             color="red",
             linetype = 2) +
  theme_bw()

## Histograma dos resíduos
diag %>%
  ggplot(aes(x=rs_argila, y=..density..)) +
  geom_histogram(bins = 7,color="black",fill="gray") +
  theme_bw()
diag %>% pull(rs_argila) %>% shapiro.test()
diag %>% pull(rs_argila) %>% nortest::ad.test()
diag %>% pull(rs_argila) %>% nortest::cvm.test()
diag %>% pull(rs_argila) %>% nortest::lillie.test()

## Homocedasticidade
argila <- diag %>% pull(argila)
sup <- diag %>% pull(sup) %>% as_factor()
argila_t <- argila^(0.626)
lawstat::levene.test(argila_t, sup)
lawstat::levene.test(argila_t, sup,location = "mean")
bartlett.test(argila_t,sup)
# Conclusão: rejeitamos H0 ao nível de 5% de probabi-
# lidade e concluímos que os dados de teor de argila
# não tem variância comum, ou seja, não são
# homocedásticos.

## Realizar o teste de Bartlett para verificar
## a relação da média e da variância.
## transformação se coef beta for significativo
## heterocedasticidade regular
## y_t = y^(1-beta/2)

### Construir a tabela de médias e variâncias
df_aux <- diag %>%
  group_by(sup) %>%
  summarise(
    arg_media = mean(argila, na.rm = TRUE),
    arg_var = var(argila, na.rm = TRUE),
    log_arg_media = log(arg_media),
    log_arg_var = log(arg_var)
  )

df_aux %>%
  ggplot(aes(x=log_arg_media,y=log_arg_var))+
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se=FALSE)

modelo_reg <- lm(log_arg_var ~ log_arg_media,
                 data = df_aux)
summary.lm(modelo_reg)
## Conclusão: Não rejeitamos H0 para o teste
## t ao nível de 5% de probabilidade, indicando
## que não existe uma relação linear entre
## o log da média de argila e o logo da variância
## de argila. Portanto, concluímos que a
## heterocedasticidade é irregular.
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
# Conclusão: O valor de lambda estimado, não diferiu
# da unidade (de um) indicando a não tranformação
# dos dados. Portanto, a análise de variância, não
# pode ser suportada pelos seus pressupostos, reco-
# menda-se a utilização de outras técnicas, como
# análise não-paramétria e/ou analise exploratória
# multivariada.

