# Data:12/12/2023
# Programado por: Alan R Panosso

# Carregar os pacotes necessários
library(tidyverse)
library(agricolae)

# Ler o banco de dados geomorfologia.
milho <- read_rds("data/dados_dbc.rds")
glimpse(milho)

# Construir o boxplot para visualizar a variabilidade
# do teor de ctc para as diferentes superfícies
geomorfologia %>%
  ggplot(aes(x=sup, y=ctc)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Comparar a variância do teor de ctc do solo
# para as superfícies, duas a duas.
ctc_sup_1 <- geomorfologia %>%
  filter(sup == "I") %>%
  pull(ctc)

ctc_sup_2 <- geomorfologia %>%
  filter(sup == "II") %>%
  pull(ctc)

ctc_sup_3 <- geomorfologia %>%
  filter(sup == "III") %>%
  pull(ctc)


# Fazer o test F para sup 1 vs 2
var.test(ctc_sup_2,ctc_sup_1,
         alternative = "g",
         conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que a variância do teor de ctc-
# la é maior para a superfície 2. Ou seja, são
# heterocedásticas.

# Fazer o test F para sup 1 vs 3
var.test(ctc_sup_3,ctc_sup_1,
         alternative = "g",
         conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que a variância do teor de ctc-
# la é maior para a superfície 3. Ou seja, são
# heterocedásticas.

# Fazer o test F para sup 2 vs 3
var.test(ctc_sup_2,ctc_sup_3,
         alternative = "g",
         conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que a variância do teor de ctc-
# la é maior para a superfície 2. Ou seja, são
# heterocedásticas.

# Teste de comparação de médias.
t.test(ctc_sup_1, ctc_sup_2,
       alternative = "t",
       var.equal = FALSE,
       conf.level = 0.95)
# Conclusão: rejeitamos H0 ao nível de 5% de signifi-
# cância e concluímos que as médias do teor de ctc
# para as superfícies I e II são diferentes.

# Construa o histograma para ctc em cada superfície
# realize o teste de normalidade:
# alctca = 5%, ou seja, se p<0.05 rejeitamos H0
# H0: Dados tem distribuição normal
# H1: Dados não tem ditribuição normal
## Superfície I
geomorfologia %>%
  filter(sup == "I") %>%
  ggplot(aes(x=ctc, y=..density..)) +
  geom_histogram(bins=7,color="black",fill="linen")
ctc_sup_1 %>%  shapiro.test()
ctc_sup_1 %>%  nortest::ad.test()
ctc_sup_1 %>%  nortest::cvm.test()
ctc_sup_1 %>%  nortest::lillie.test()
# Conclusão: Não rejeitamos H0 ao nível de 1% de sig-
# nificância e concluímos que os dados de teor de
# ctc para superfície I seguem distribuição
# normal.

## Superfície II
geomorfologia %>%
  filter(sup == "II") %>%
  ggplot(aes(x=ctc, y=..density..)) +
  geom_histogram(bins=15,color="black",fill="orange")
ctc_sup_2 %>%  shapiro.test()
ctc_sup_2 %>%  nortest::ad.test()
ctc_sup_2 %>%  nortest::cvm.test()
ctc_sup_2 %>%  nortest::lillie.test()
# Conclusão: Rejeitamos H0 ao nível de 1% de sig-
# nificância e concluímos que os dados de teor de
# ctc para superfície II não seguem distribuição
# normal.

## Superfície III
geomorfologia %>%
  filter(sup == "III") %>%
  ggplot(aes(x=ctc, y=..density..)) +
  geom_histogram(bins=15,color="black",fill="salmon")
ctc_sup_3 %>%  shapiro.test()
ctc_sup_3 %>%  nortest::ad.test()
ctc_sup_3 %>%  nortest::cvm.test()
ctc_sup_3 %>%  nortest::lillie.test()
# Conclusão: Não rejeitamos H0 ao nível de 1% de sig-
# nificância e concluímos que os dados de teor de
# ctc para superfície III seguem distribuição
# normal.

# Calcule a interprete os coeficientes de assimetria
# e curtose para cada superfície.
ctc_sup_1 %>% agricolae::skewness()
ctc_sup_1 %>% agricolae::kurtosis()

ctc_sup_2 %>% agricolae::skewness()
ctc_sup_2 %>% agricolae::kurtosis()

ctc_sup_3 %>% agricolae::skewness()
ctc_sup_3 %>% agricolae::kurtosis()

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# das superfícies nos teores de ctc do solo.
modelo <- aov(ctc_t ~ sup,
              data = geomorfologia %>%
                mutate(sup = as_factor(sup),
                       ctc_t = ctc^(-1.23))
              )
modelo
anova(modelo) # sem efeito.

# Fazer o diagnóstico da análise de variância
## Normalidade dos erros
diag <- geomorfologia %>%
  mutate(
    rs_ctc = rstudent(modelo), # extrair residuos
    pred_ctc = predict(modelo) # calcular preditos
  ) %>%
  select(sup, ctc, rs_ctc, pred_ctc)

## Construa o QQ plot
diag %>%
  ggplot(aes(sample=rs_ctc)) +
  stat_qq() +
  stat_qq_line(color="blue") +
  theme_bw()

## Construa o violin-plot
# FAZER DEPOIS EM CASA

## Estudo de outliers
diag %>%
  ggplot(aes(x = pred_ctc, y= rs_ctc)) +
  geom_point() +
  ylim(-4,4) +
  geom_hline(yintercept = c(-3,3),
             color="red",
             linetype = 2) +
  theme_bw()

## Histograma dos resíduos
diag %>%
  ggplot(aes(x=rs_ctc, y=..density..)) +
  geom_histogram(bins = 7,color="black",fill="gray") +
  theme_bw()
diag %>% pull(rs_ctc) %>% shapiro.test()
diag %>% pull(rs_ctc) %>% nortest::ad.test()
diag %>% pull(rs_ctc) %>% nortest::cvm.test()
diag %>% pull(rs_ctc) %>% nortest::lillie.test()

## Homocedasticidade
ctc <- diag %>% pull(ctc)
sup <- diag %>% pull(sup) %>% as_factor()
ctc_t <- ctc^(-1.23)
lawstat::levene.test(ctc_t, sup)
lawstat::levene.test(ctc_t, sup,location = "mean")
bartlett.test(ctc_t,sup)
# Conclusão: rejeitamos H0 ao nível de 5% de probabi-
# lidade e concluímos que os dados de teor de ctc
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
    ctc_media = mean(ctc, na.rm = TRUE),
    ctc_var = var(ctc, na.rm = TRUE),
    log_ctc_media = log(ctc_media),
    log_ctc_var = log(ctc_var)
  )

df_aux %>%
  ggplot(aes(x=log_ctc_media,y=log_ctc_var))+
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se=FALSE)

modelo_reg <- lm(log_ctc_var ~ log_ctc_media,
                 data = df_aux)
summary.lm(modelo_reg)
## Conclusão: Não rejeitamos H0 para o teste
## t ao nível de 5% de probabilidade, indicando
## que não existe uma relação linear entre
## o log da média de ctc e o logo da variância
## de ctc. Portanto, concluímos que a
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

