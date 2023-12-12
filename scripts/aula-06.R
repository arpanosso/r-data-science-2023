# Data:
# Programado por:

# Carregar os pacotes necessários
library(tidyverse)
library(agricolae)

# Ler o banco de dados geomorfologia.

# Construir o boxplot para visualizar a variabilidade
# do teor de argila para as diferentes superfícies

# Comparar a variância do teor de argila do solo
# para as superfícies, duas a duas.

# Construa o histograma para argila em cada superfície
# realize o teste de normalidade:
# alpha = 5%, ou seja, se p<0.05 rejeitamos H0
# H0: Dados tem distribuição normal
# H1: Dados não tem ditribuição normal

# Calcule a interprete os coeficientes de assimetria
# e curtose para cada superfície.

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# das superfícies nos teores de argila do solo.

# Fazer o diagnóstico da análise de variância
## Normalidade dos erros

## Construa o QQ plot

## Contrua o violin-plot

## Estudo de outliers

## Homocedasticidade

## Realizar o teste de Bartlett para verificar
## a relação da média e da variância.
## transformação se coef beta for significativo
## heterocedasticidade regular
## y_t = y^(1-beta/2)

## Estudar a melhor transformação pelo Box-Cox
## método de Box-Cox
### se lambda não difere de 1 ~ homocedásticos
### se lambda difere de 1:
### se lambda não difere de 0 y_t=log(y)
### se lambda difere de 0 y_t=y^LAMBDA
