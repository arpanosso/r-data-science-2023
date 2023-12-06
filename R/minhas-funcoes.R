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
  p_normalidade <- shapiro.test(vetor)

  saida <- c(
    N = length(vetor),
    Soma = sum(vetor),
    Media=media,
    Minimo = min(vetor),
    Q1 = quantile(vetor,0.25),
    Mediana = median(vetor), # direto no vetor saída
    Q3 = quantile(vetor, 0.75),
    Maximo = max(vetor),
    Variancia=variancia,
    DesvPad = sd(vetor),
    ErroPadMedia=erro_padrao_media(vetor),
    CV=cv,
    Assimetria =agricolae::skewness(vetor),
    Curtose = agricolae::kurtosis(vetor),
    W_Shapiro_Wilk = as.vector(p_normalidade$statistic),
    p_value = p_normalidade$p.value
    )

  return(saida)
}














