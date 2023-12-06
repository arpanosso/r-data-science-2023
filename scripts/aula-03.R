# Data:
# Programado por:

# Pacotes necessários:
library(tidyverse)
library(readxl)
source("R/minhas-funcoes.R")



# Valores perdidos - NAs --------------------------------------------------
# Valores faltantes NA
## Cuidado pois são fontes de propagação de ERRO no código.

# Criando um vetor, aplicando a função média

## NA não funciona com operadores relacionais

## Função is.na()

## Contar o número de NAs no vetor y

## Contar o número de não NAs no vetor y

## Função para contar NA

# Faxina dos dados geomorfologia ------------------------------------------
# Realizar a faxina nos dados de geomorfologia

# Quantas suerfícies e tipos de solo estão presentes na base?

## Observar a estrutura dos dados

# alterar o nome da variável relacao_silte_argila para
# silte_argila
# alterar o nome da variável p para p_resina
# altear o nome da variável t para ctc

# Salvar uma versão em rds na pasta data

# leia novamente esse arquivo rds...


# Verbos do dyplr ---------------------------------------------------------
# VERBOS summarise e group_by
# Calcule a média para cada variável numérica

# Calcule um resumo estatístico para cada variável numérica

# Calcule a média do pH para cada superfície.

# Calcule a média e variância da argila para cada solo.

# VERBO filter, vamos filtrar:
# Todas as variáveis para a SUP == "I"

# Vamos selecionar somente os solos do tipo LV

# Vamos selecionar somente os solos do tipo LV com teor de
# argila maiores ou iguais a 20%

#VERBO mutate
# Criar a variável ARGILA+SILTE

# Passar a CTC para a escala logarítmica

# Classificar a CTC média (>=15) e baixa (<15)

# Classificar a CTC média (>=15) e baixa (<15 e >=6) e muito baixa (<6)

# vamos eliminar as 4 últimas colunas

# criando um novo banco de dados

# Ordenar o banco de dados por teor de a argila do menor para o
# maior

# Ordenar o banco de dados por teor de argila do maior para o
# menor




