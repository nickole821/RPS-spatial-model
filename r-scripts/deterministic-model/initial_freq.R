### Spatial model ###
### 2 dimensions ###
### simetric matrix ###
### diferent initial frequencies ###

library(tidyverse)

### Functions ------------------------------------------------------------------
evolucao_matriz <- function(position_matrix_0, num_geracoes) {
  
  freq_resultado <- data.frame(geracao = integer(), O = numeric(), Y = numeric(),
                               B = numeric())
  matrizes_geradas <- list(position_matrix_0)
  
  get_neighbors <- function(i, j, matriz) {
    vizinhos <- c()
    if (i > 1 & i < size & j > 1 & j < size) {
      vizinhos <- c(matriz[i-1, j-1], matriz[i-1, j], matriz[i-1, j+1],
                    matriz[i, j-1], matriz[i, j+1], 
                    matriz[i+1, j-1], matriz[i+1, j], matriz[i+1, j+1]) 
    } else if (i == 1 & j > 1 & j < size) {
      vizinhos <- c(matriz[size, j-1], matriz[size, j], matriz[size, j+1],
                    matriz[i, j-1], matriz[i, j+1], 
                    matriz[i+1, j-1], matriz[i+1, j], matriz[i+1, j+1]) 
    } else if (i == size & j > 1 & j < size) {
      vizinhos <- c(matriz[i-1, j-1], matriz[i-1, j], matriz[i-1, j+1],
                    matriz[i, j-1], matriz[i, j+1], 
                    matriz[1, j-1], matriz[1, j], matriz[1, j+1]) 
    } else if (i > 1 & i < size & j == 1) {
      vizinhos <- c(matriz[i-1, size], matriz[i-1, j], matriz[i-1, j+1],
                    matriz[i, size], matriz[i, j+1], 
                    matriz[i+1, size], matriz[i+1, j], matriz[i+1, j+1]) 
    } else if (i > 1 & i < size & j == size) {
      vizinhos <- c(matriz[i-1, j-1], matriz[i-1, j], matriz[i-1, 1],
                    matriz[i, j-1], matriz[i, 1], 
                    matriz[i+1, j-1], matriz[i+1, j], matriz[i+1, 1]) 
    } else if (i == 1 & j == 1) {
      vizinhos <- c(matriz[size, size], matriz[size, j], matriz[size, j+1],
                    matriz[i, size], matriz[i, j+1], 
                    matriz[i+1, size], matriz[i+1, j], matriz[i+1, j+1]) 
    } else if (i == size & j == size) {
      vizinhos <- c(matriz[i-1, j-1], matriz[i-1, j], matriz[i-1, 1],
                    matriz[i, j-1], matriz[i, 1], 
                    matriz[1, j-1], matriz[1, j], matriz[1, 1])
    } else if (i == size & j == 1) {
      vizinhos <- c(matriz[i-1, size], matriz[i-1, j], matriz[i-1, j+1],
                    matriz[i, size], matriz[i, j+1], 
                    matriz[1, size], matriz[1, j], matriz[1, j+1]) 
    } else if (i == 1 & j == size) {
      vizinhos <- c(matriz[size, j-1], matriz[size, j], matriz[size, 1],
                    matriz[i, j-1], matriz[i, 1], 
                    matriz[i+1, j-1], matriz[i+1, j], matriz[i+1, 1]) 
    }
    return(vizinhos)
  }
  
  get_payoff <- function(acao1, acao2) {
    payoff <- c()
    if (acao1 == "O" && acao2 == "O") {
      payoff <- c(payoff, payoff_matrix[1, 1])
    } else if (acao1 == "O" && acao2 == "Y") {
      payoff <- c(payoff, payoff_matrix[1, 2])
    } else if (acao1 == "O" && acao2 == "B") {
      payoff <- c(payoff, payoff_matrix[1, 3])
    } else if (acao1 == "Y" && acao2 == "O") {
      payoff <- c(payoff, payoff_matrix[2, 1])
    } else if (acao1 == "Y" && acao2 == "Y") {
      payoff <- c(payoff, payoff_matrix[2, 2])
    } else if (acao1 == "Y" && acao2 == "B") {
      payoff <- c(payoff, payoff_matrix[2, 3])
    } else if (acao1 == "B" && acao2 == "O") {
      payoff <- c(payoff, payoff_matrix[3, 1])
    } else if (acao1 == "B" && acao2 == "Y") {
      payoff <- c(payoff, payoff_matrix[3, 2])
    } else if (acao1 == "B" && acao2 == "B") {
      payoff <- c(payoff, payoff_matrix[3, 3])
    }
    return(payoff)
  }
  
  get_fitness_matrix <- function(position_matrix) {
    fitness_matrix <- matrix(NA, nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        fitness_ID <- 0  # Iniciar com 0
        a <- get_neighbors(i, j, position_matrix)
        for (z in 1:length(a)) {
          fitness_ID <- fitness_ID + get_payoff(position_matrix[i, j], a[z])
        }
        fitness_matrix[i, j] <- fitness_ID
      }
    }
    return(fitness_matrix)
  }
  
  get_new_matrix <- function(position_matrix, fitness_matrix) {
    nova_matriz <- matrix(NA, nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        vizinhos_mais_focal <- c(get_neighbors(i, j, position_matrix),
                                 position_matrix[i,j])
        fitness_mais_focal <- c(get_neighbors(i, j, fitness_matrix),
                                fitness_matrix[i,j])
        x <- which.max(fitness_mais_focal)
        nova_matriz[i,j] <- vizinhos_mais_focal[x]
      }
    }
    return(nova_matriz)
  }
  
  # Frequência inicial
  freq_0 <- table(position_matrix_0)/size^2
  freq_resultado <- rbind(freq_resultado, data.frame(
    geracao = 0,
    O = ifelse("O" %in% names(freq_0), freq_0["O"], 0), 
    B = ifelse("B" %in% names(freq_0), freq_0["B"], 0),
    Y = ifelse("Y" %in% names(freq_0), freq_0["Y"], 0)
  ))
  
  for (geracao in 1:num_geracoes) {
    fitness_matrix <- get_fitness_matrix(position_matrix_0)
    nova_matriz <- get_new_matrix(position_matrix_0, fitness_matrix)
    
    matrizes_geradas[[geracao + 1]] <- nova_matriz
    
    freq <- table(nova_matriz) / size^2
    freq_resultado <- rbind(freq_resultado, data.frame(
      geracao = geracao,
      O = ifelse("O" %in% names(freq), freq["O"], 0), 
      B = ifelse("B" %in% names(freq), freq["B"], 0),
      Y = ifelse("Y" %in% names(freq), freq["Y"], 0)))
    
    position_matrix_0 <- nova_matriz  # Atualize a posição da matriz
  }
  return(list(mat = matrizes_geradas, freq = freq_resultado))
}

### Setting players and the payoff matrix --------------------------------------

strategies <- c("O", "Y", "B")
f_O <- 0.25
f_Y <- 0.25
f_B <- 1 - (f_O + f_Y)
i_freq <- c(f_O, f_Y, f_B)

a <- 2
b <- 1/a

payoff_matrix <- matrix(c(1, b, a,
                          a, 1, b,
                          b, a, 1), byrow = T, nrow = 3)

colnames(payoff_matrix) <- strategies
rownames(payoff_matrix) <- strategies

payoff_matrix

### Creating the initial position matrix ---------------------------------------

path <- "./output/data/initial_freq/O025-Y025-B05/"
size <- 100
n_sim <- 100  # Número de simulações
t <- 100  # Número de gerações por simulação
final_freq <- data.frame(
  geracao = integer(),
  O = numeric(),
  Y = numeric(),
  B = numeric(),
  sim = numeric(),
  stringsAsFactors = FALSE  # Para evitar fatores automáticos
)

for (i in 1:n_sim) {
  cat("Rodando simulação", i, "\n")  
  
  position_matrix_0 <- matrix(sample(strategies, size^2, replace = TRUE,
                                     prob = i_freq),
                              nrow = size, ncol = size)
  
  # Executando a simulação
  result <- evolucao_matriz(position_matrix_0, t)
  
  # Salvar frequências
  write.csv(result$freq, 
            file = paste0(path, "freq/freq_", i, ".csv"), 
            row.names = FALSE)
  
  a <- result$freq %>%
    filter(geracao == max(geracao)) %>%
    mutate(sim = i)
  
  final_freq <- rbind(final_freq, a)
  
  # Salvar todas as matrizes geradas durante a simulação
  for (j in 1:(t + 1)) {
    write.csv(as.data.frame(result$mat[[j]]), 
              file = paste0(path, "matrix/matrix_", i,
                            "_generation_", j, ".csv"), 
              row.names = FALSE)
  }
}

cat("Acabou! Ufa! :)\n")

write.csv(final_freq, 
          file = paste0(path, "freq/final_freq.csv"), 
          row.names = FALSE)



