## Código para limpar o console do R
rm(list = ls())
##==================================
library(dplyr)

# varianca_amostral <- function(x) {
#   media_x <- mean(x)
#   divisor <- length(x) - 1
#   s = 0
#   
#   for (i in x) {
#     s <- ((i - media_x)^2)
#   }
#   s <- s / divisor
#   s <- sum(s)
#   return(s)
# }

variancia_amostral <- function(x) {
  n <- length(x)  # Número de observações
  media <- mean(x)  # Média amostral
  soma_quadrados <- sum((x - media)^2)  # Soma dos quadrados das diferenças
  variancia <- soma_quadrados / (n - 1)  # Divisão pelo grau de liberdade
  return(variancia)
}

desvio_abs_medio <- function(x) {
  n <- length(x)  # Número de observações
  media <- mean(x)  # Média amostral
  modulo_soma_quadrados <- abs(x - media)  # Modulo da soma dos quadrados das diferenças
  dam <- mean(modulo_soma_quadrados / (n - 1))  # Divisão pelo grau de liberdade
  return(dam)
}

coef_assim_pearson <- function(x) {
  media <- mean(x)       # Média
  mediana <- median(x)   # Mediana
  desvio_padrao <- sd(x) # Desvio padrão
  
  if (desvio_padrao == 0) {
    return(NA)  # Evita divisão por zero
  }
  
  skewness <- 3 * (media - mediana) / desvio_padrao
  return(skewness)
}

varianca_amostral(ChickWeight$Time)
desvio_abs_medio(ChickWeight$Time)
coef_assim_pearson(ChickWeight$weight[1:28])

resumo <- tibble(
  variancia_time = varianca_amostral(ChickWeight$Time),
  dma_time = desvio_abs_medio(ChickWeight$Time),
  
)

# var(ChickWeight$Time)

# =====
a_mat <- matrix( c(-5, 7, 8, 9, -2, -1, 5, 3, -8, 6, -7, 4, -10, -6, 0, 2) , nrow = 4, byrow = FALSE)
b_mat <- matrix( c(-11, 17, -2, -12, -8, 0, 4, 10, -6, -20, -19, 20, -3, -17, -9, -10, -18, 15, -15, 1) , nrow = 4, byrow = FALSE)
c_mat <- matrix( c(6, 5, -19, 15, 25, 30, -3, 16, -24, -2, -5, 13, -6, -18, 1, -28) , nrow = 4, byrow = FALSE)

eh_primo <- function(m) {
  if (m == 2) {
    return(TRUE)
  }
  if (m <= 1) {
    return(FALSE)
  }
  for (i in 2:(m-1)) {
    if (m %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

## print(is_prime(-5))

eh_quadrado_perfeito <- function(m) {
  if (m < 0) return(FALSE)
  raiz <- sqrt(m)
  return(raiz == floor(raiz))
}

varrer_matriz <- function(mat) {
  m <- nrow(mat)
  mat_transformada <- mat  # Copia a matriz original
  
  for (i in 1:m) {
    for (j in 1:m) {
      valor <- mat[i, j]
      
      if (eh_primo(valor)) {
        mat_transformada[i, j] <- valor * 3
      } else if (eh_quadrado_perfeito(valor)) {
        mat_transformada[i, j] <- valor - 14
        if (mat_transformada[i, j] < 0) {
          mat_transformada[i, j] <- abs(mat_transformada[i, j])^5
        }
      } else if (valor < 0) {
        mat_transformada[i, j] <- sqrt(abs(valor))
      }
    }
  }
  return(mat_transformada)
}

# print(varrer_matriz(a_mat))

# Aplicar a função nas matrizes
mat_A_trans <- varrer_matriz(a_mat)
mat_B_trans <- varrer_matriz(b_mat)
mat_C_trans <- varrer_matriz(c_mat)

# 1. Soma dos elementos da diagonal principal da matriz A transformada
soma_diag_A <- sum(diag(mat_A_trans))
print(paste("Soma da diagonal principal de A:", soma_diag_A))

# 2. Maior elemento em módulo da matriz C transformada
maior_mod_C <- max(abs(mat_C_trans))
print(paste("Maior elemento em módulo de C:", maior_mod_C))

# 3. Soma dos elementos da coluna 1 da matriz C transformada
soma_col1_C <- sum(mat_C_trans[, 1])
print(paste("Soma da coluna 1 de C:", soma_col1_C))

# 4. Quantidade de números primos na matriz A original
primos_A <- sum(sapply(a_mat, eh_primo))
print(paste("Quantidade de números primos em A:", primos_A))

# 5. Quantidade de números primos nas matrizes A, B e C juntas
todas_as_matrizes <- c(a_mat, b_mat, c_mat)
primos_total <- sum(sapply(todas_as_matrizes, eh_primo))
print(paste("Quantidade total de números primos em A, B e C:", primos_total))

# =====

# Função principal para simular a população de peixes no lago
simular_lago <- function(dias, peixes_iniciais, estacao, ph) {
  # Parâmetros fixos
  capacidade_maxima <- (5*10^5)   # Capacidade máxima de peixes
  taxa_basica <- 0.06        # Taxa de reprodução basal
  
  # Ajustes com base na Tabela 1 (Taxa de Reprodução)
  fator_reproducao <- switch(estacao,
                             "Primavera" = 0.00,
                             "Verão" = 0.03,
                             "Outono" = -0.03,
                             "Inverno" = -0.02)
  
  taxa_reproducao_ajustada <- taxa_basica + fator_reproducao
  
  # Ajustes com base na Tabela 2 (Percentual Pescado)
  percentual_pescado <- ifelse(ph >= 6.0 & ph <= 6.5, 0.12,
                               ifelse(ph > 6.5 & ph <= 7.0, 0.20,
                                      ifelse(ph > 7.0 & ph <= 7.5, 0.05,
                                             ifelse(ph > 7.5 & ph <= 8.0, 0.16, 0.00))))
  
  # Inicialização
  peixes <- peixes_iniciais
  resultado <- data.frame(dias = 1, peixes = peixes)
  
  # Loop para calcular o número de peixes dia a dia
  for (dia in 2:dias) {
    peixes_reproducao <- peixes * (1 + taxa_reproducao_ajustada)
    peixes_pescados <- peixes * percentual_pescado
    peixes <- ceiling(peixes_reproducao - peixes_pescados)
    
    # Limita ao máximo de capacidade
    if (peixes > capacidade_maxima) peixes <- capacidade_maxima
    
    # Adiciona o resultado ao data frame
    resultado <- rbind(resultado, data.frame(dias = dia, peixes = peixes))
  }
  
  return(resultado)
}

# Exemplo de Uso
resultado <- simular_lago(
  dias = 30,
  peixes_iniciais = 50000,
  estacao = "Verão",
  ph = 7.0
)

print(resultado)

# 1 Durante o Verão e com pH entre 6.6–7.0 após 16 dias:
resultado <- simular_lago(16, 80, "Verão", 6.6)
print(resultado)

# 2 Quantos dias são necessários para atingir a capacidade máxima:
resultado <- simular_lago(1000, 80, "Verão", 6.6)
print(min(resultado$peixes == 5e5))

# 3 Outono com pH entre 6.0–6.5 após 87 dias:
resultado <- simular_lago(87, 80, "Outono", 6.0)
print(resultado)

# 4 Quantos dias são necessários para atingir a capacidade máxima no Outono:
resultado <- simular_lago(1000, 80, "Outono", 6.0)
print(min(resultado$peixes == 5e5))

