# Minhas funções
# Função para o cálculo do erro padrão da média
erro_padrao_media <- function(vetor){
  s <- sd(vetor)
  n <- length(vetor)
  epm <- s/sqrt(n)
  return(epm)
}

# Função para calcular o CV
coef_varia <- function(vetor){
  s <- sd(vetor)
  media <- mean(vetor)
  cv <- s/media*100
  return(cv)
}

# Função de Resumo Estatístico
resumo_estatistico <- function(vetor){
  media <- mean(vetor)
  variancia <- var(vetor)
  cv <- coef_varia(vetor)

  saida <- c(Media=media,
             Variancia=variancia,
             CV=cv)

  return(saida)
}



















