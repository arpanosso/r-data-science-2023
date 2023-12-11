# Minhas funções
# Função para o cálculo do erro padrão da média
erro_padrao_media <- function(vetor){
  s <- sd(vetor,na.rm = TRUE)
  n <- length(vetor)
  epm <- s/sqrt(n)
  return(epm)
}

# Função para calcular o CV
coef_varia <- function(vetor){
  s <- sd(vetor,na.rm = TRUE)
  media <- mean(vetor,na.rm = TRUE)
  cv <- s/media*100
  return(cv)
}

# Função de Resumo Estatístico
resumo_estatistico <- function(vetor){
  media <- mean(vetor,na.rm = TRUE)
  variancia <- var(vetor,na.rm = TRUE)
  cv <- coef_varia(vetor)
  p_normalidade <- shapiro.test(vetor)

  saida <- c(
    N = length(vetor),
    Soma = sum(vetor,na.rm = TRUE),
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

# função para contar NAs
contar_nas <- function(vetor) sum(is.na(vetor))


# função para definição do tema no ggplot
meu_tema <- function(){
  list(
    theme(
      plot.title = element_text(hjust = 1/2,
                                color="red",
                                size=rel(2)),
      axis.title = element_text(color="blue",
                                size=rel(1.5)),
      panel.background = element_rect(fill="pink",
                                      color="red"),
      axis.text.x = element_text(color="darkgreen",
                                 angle = 90,
                                 size = rel(3))
    )
  )
}









