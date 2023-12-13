# Data: 13/12/2023
# Programado por: Alan R Panosso
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

# Realizar a Anova com o Pacote ExpDes.pt
# Extrair os vetor numérico da variável alvo
y <- dados %>% pull(y)

# Extrair o fator tratamento
trat <- dados %>%  pull(trat) %>% as_factor()

# Análise de Variância
ExpDes.pt::dic(trat, y, mcomp="tukey")

# SQ total
C <- sum(y)^2/(5*6)
sum(y^2) - C

# SQ Tratamento
total <- dados %>%
  group_by(trat) %>%
  summarise(
    total = sum(y)
  ) %>% pull(total)
sum(total^2)/6 - C

# SQ residuo
(sum(y^2) - C) - (sum(total^2)/6 - C)

# Teste de Tukey
agricolae::HSD.test(y = y,trt = trat,
                    DFerror = 25,MSerror = 70746,
                    console = TRUE)

# Calcular as Médias
dados %>%
  group_by(trat) %>%
  summarise(
    media = mean(y)
  )

# Realizar o diagnóstico da análise de variância
# transforme os dados se necessário.

# Construir o boxplot para visualizar a variabilidade
# d Y para as diferentes trats
dados %>%
  ggplot(aes(x=trat, y=y)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# de trat nos valores de y.
modelo <- aov(y_bc ~ trat,
              data = dados %>%
                mutate(
                  trat = as_factor(trat),
                  y_t = y^(1-1.5423/2),
                  y_bc = log(y)
                  )
)
modelo
anova(modelo) # sem efeito.

## Normalidade dos erros
diag <- dados %>%
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
lawstat::levene.test(y_bc, trat)
lawstat::levene.test(y_bc, trat,location = "mean")
bartlett.test(y_bc, trat)
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

ExpDes.pt::dic(trat,y_bc,mcomp = "tukey")
