# Instalar o pacote {writexl}
# install.packages("writexl")

# lendo o arquivo em rds
library(readr)
dados_alunos <- read_rds("data/dados_alunos.rds")

# salvando o objeto em xlsx
writexl::write_xlsx(dados_alunos,
                    "data/dados_alunos.xlsx")

# LENDO UM ARQUIVO XLSX
library(readxl)
dados_alunos <- read_excel("data/dados_alunos.xlsx")
View(dados_alunos)
