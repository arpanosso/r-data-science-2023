# Data: 05/12/2023
# Programado por: Alan R Panosso
#
# 1) Listar todos os pacotes presentes no ambiente de
# trabalho.
(.packages())

# carregar um pacote no ambiente {MASS}
library(MASS)
(.packages())

# 2) Listar todos os pacotes disponíveis
# (pré-instalados) no R.
(.packages(all.available = TRUE))

# 3) Instalar os seguintes pacotes:
# install.packages("agricolae")
# install.packages("nortest")
# install.packages("lawstat")
# install.packages("tidyverse")

# 4) Exemplo de operações
## MOD - Resto da Divisão
23 %% 4

## DIV - Divisão Inteira
23 %/% 4

# DADOS X = 1531, construir:
# Y = 15
# Z = 31
# W = 3115
X <- 1531
Y <- X %/% 100
Z <- X %% 100
W <- 100*Z + Y

## Raiz Quadrada
sqrt(225)

## raiz cúbica de 16
16^(1/3)

## Potenciação
2^10
2**10

## Logaritmos
log10(1000)
log(9,3)
log(7.389056)

## Exponenciação
exp(1)
exp(2)

## FUnções Trigonométricas
sin(30)
pi
sin(30*pi/180)

## Operadores Relacionais
5 > 3
4 < 8
5 >= 10
5 <= 10
5 == 10
5 != 10
nome <- "Vanellope"
nome == "Pumba"
nome == "Vanellope"

## Operadores Lógicos (Booleanos)
# CONJUNÇÃO (e)
T && T
T && F
F && T
F && F

# DISJUNÇÃO (ou)
T || T
T || F
F || T
F || F

# NEGAÇÃO (não)
!T
!F

# 4) A partir de um vetor, criar uma matriz no R
vetor_a <- c(1, 2, 3, 4, 5, 6, 7, 9, 10)
A <- matrix(vetor_a, ncol=3) # preenchimento linha
                             # byrow = TRUE
vetor_b <- c(2, 4, 6, 8, 0, 10)
B <- matrix(vetor_b, ncol=2)

# Calcular o determinante de A (matriz quadrada)
det(A)

# Multiplicação de Matrizes
A %*% B

# Inversa de A
inv_A <- solve(A)

# Matriz indentidade
round(inv_A %*% A)

# resolvendo um sistema de equação linear
A <- matrix(
  c(4,3,3,1, 2,-3,5,-1, 1,-1,1,-1, -2,-1,1,4),
  ncol = 4
)

B <- matrix(
  c(3, 2, 0 , -2),
  ncol=1
)

solve(A) %*% B

# 5) Observe o retorno das funções nrow, ncol e
# length.
length(A) #númerio total de elementos da matriz
nrow(A)
ncol(A)

# 6) Calcule o determinante da matriz
det(A)

# 7) Crie a matriz transposta
t(A)

# 8) Dados os vetores
# x = {1, 2, 3, 4, 5}
# y = {2.1, 8.9, 18.7, 32.3, 50.2}
# faça o gráfico de y ~ x
x <- 1:5
y <- c(2.1, 8.9, 18.7, 32.3, 50.2)
plot(x,y,
     main = "Gráfico de Dispersão",
     cex = 2,
     pch = 16,
     col = "red",
     xlab = "Eixo X",
     ylab = "Eixo Y",
     cex.lab = 1.4,
     las = 1,
     ylim = c(0, 55)
     )
abline(h = mean(y), lty =2, col="blue")
text(x,y+2.5,label=y)

#











# Realize um ajuste polinomial de segunda ordem
# onde a matriz de coeficiente BETA
# podrá se estimada por:
# BETA = (X^T.X)^(-1) X^T Y

