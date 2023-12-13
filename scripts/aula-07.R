# Data:
# Programado por:
# Carregar os pacotes
library(tidyverse)

# Os dados abaixo referen-se a um  experimento em dic
# com 5 tratamentos (variedades) e 6 repetições onde a
# variável aleatória estudada foi a produção da
# cultura. Realize a análise de variância para testar
# a hipótese do efeitos das variedades por meio do
# teste F a 1% de probabilidade. Caso a hipótese
# da nulidade seja rejeitada, realize um teste de
# comparação múltiplo para concluir qual a melhor
# variedade quanto a produção (maior produção).

# Entrada de dados
dados <- read_rds("data/dados_dic.rds")
glimpse(dados)

# Realizar o diagnóstico da análise de variância
# transforme os dados se necessário.

# Construir o boxplot para visualizar a variabilidade
# d Y para as diferentes trats
geomorfologia %>%
  ggplot(aes(x=sup, y=argila)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# de trat nos valores de y.
modelo <- aov(argila_t ~ sup,
              data = geomorfologia %>%
                mutate(sup = as_factor(sup),
                       argila_t = argila^(0.626))
)
modelo
anova(modelo) # sem efeito.

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
# Conclusão:

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
## Conclusão:
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

