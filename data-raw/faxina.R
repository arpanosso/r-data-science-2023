library(tidyverse)

# Script Para Faxina de Dados
df <- read.table("data-raw/alunos.txt",
           h = TRUE)
df <- janitor::clean_names(df)

# Criando o vetor de nomes
vetor_nomes <- c("Carlos", "Edvan", "Pedro",
                 "Gabriel", "Daniel")

vetor_nomes[1] <- "Carlos Daniel"
df[4,2] # linha 4 coluna 2
df[4,] # linha 4
df[,1] # coluna 1
df$idade
df$nome <- vetor_nomes # adicionando uma coluna
df <- df[c(3,1,2)]

length(vetor_nomes) # número de elementos do vetor
length(df) # número de colunas do data frame
nrow(df) # número de linhas do data frame
ncol(df) # número de colunas

df$prova_1 <- c(2, 4, 5, 2, 3)
df$prova_2 <- c(3, 5, 8, 9, 5)
df$nota_final <- 0.4*df$prova_1+0.6*df$prova_2

# Alguns testes relacionais (lógicos)
4 < 8 # menor que
4 >= 20/5 # maior ou igual
4 == 8 # igual
4 == 8/2
4 != 5 # não igual (diferente)

df$situacao <- ifelse(df$nota_final>=5,
                      "APROVADO",
                      "REPROVADO")

# Para critério com 3 saídas
df$situacao <- ifelse(df$nota_final >= 3,
       ifelse(df$nota_final >= 5, "Apr", "Rec"),
       "Rep")

# Ordenar o Banco de Dados
sort(df$nome)
filtro_ordem <- order(df$nome)
df[filtro_ordem,] #odenação por nome

filtro_ordem <- order(df$nota_final,
                      decreasing = TRUE)
df[filtro_ordem,] #odenação por nota, maior para menor
df[-(4:5)]


# instalando um pacote
# install.packages("readr")

# salvar o arquivo df como uma base de dados
# do R
# carregando o pacote
library(readr)
write_rds(df,"data/dados_alunos.rds")

###
dados <- readxl::read_xlsx("data-raw/Pasta1.xlsx") %>%
  janitor::clean_names() %>%
  mutate(
    trat = str_to_lower(trat),
    trat = str_replace(trat,"\\.","_")
  )
write_rds(dados,"data/dados_dbc.rds")

##
dados <- readxl::read_xlsx("data-raw/dados_prod_cana.xlsx") %>%
  janitor::clean_names() %>%
  mutate(
    trat = str_to_lower(trat),
    trat = str_replace(trat,"\\.","_")
  )
write_rds(dados,"data/dados_dic.rds")


##
dados <- readxl::read_xlsx("data-raw/dados_altura_mudas.xlsx") %>%
  janitor::clean_names()
write_rds(dados,"data/altura_mudas_dic.rds")


dados <- readxl::read_xlsx("data-raw/dados_prod_milho_inset_dose.xlsx") %>%
  janitor::clean_names()
write_rds(dados,"data/milho_inset_dose.rds")


dados <- readxl::read_xlsx("data-raw/dados_prod_soja.xlsx") %>%
  janitor::clean_names() %>%
  mutate(
    epoca = paste0("e",epoca),
    variedade = paste0("v",variedade)
  )

write_rds(dados,"data/soja_dbc.rds")
