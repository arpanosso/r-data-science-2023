# Data: 13/12/2023
# Programado por: Alan R Panosso
# Carregar os pacotes
library(tidyverse)

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
altura_modas <-read_rds("data/altura_mudas_dic.rds")

# Criar tabela de médias
# criar gráfico da interação
# Realizar o Diagnostico da ANOVa utilizando o delineamento
# de tratamentos (DIC no caso)
# Realizar a Análise de Variância




# Análise de variância de um experimento fatorial com 2
# fatores com interação significativa

# Para obtenção da análise de variância, vamos supor o
# seguinte ensaio em que foram estudados os efeitos de
# 4 inseticidas em 2 doses diferentes sobre a produção da
# cultura do milho em kg/parcela.
# Entrar com os dados
inset_dose <-read_rds("data/milho_inset_dose.rds")

# Criar tabela de médias

# criar gráfico da interação
# Realizar o Diagnostico da ANOVa utilizando o delineamento
# de tratamentos (DIC no caso)
# Realizar a Análise de Variância
