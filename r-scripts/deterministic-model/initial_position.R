### Spatial model ###
### 2 dimensions ###
### simetric matrix ###
### diferent initial positions ###

### Packages required ----------------------------------------------------------
library(magick)
library(tidyverse)
library(ribiosUtils)

### Creating the function ------------------------------------------------------
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
  return(list(matrizes = matrizes_geradas, frequencias = freq_resultado))
}

### Setting players and the payoff matrix --------------------------------------

strategies <- c("O", "Y", "B")

a <- 2
b <- 1/a

payoff_matrix <- matrix(c(1, b, a,
                          a, 1, b,
                          b, a, 1), byrow = T, nrow = 3)

colnames(payoff_matrix) <- strategies
rownames(payoff_matrix) <- strategies

payoff_matrix

### Creating the initial position matrix ---------------------------------------
size <- 100

# alternado em linha -----------------------------------------------------------
position_matrix_0 <- matrix(c(rep("Y", 10), rep("B", 10), rep("O", 10)),
                            ncol = size, nrow = size)


# pontos em um mar de competidores -------------------------------------------
position_matrix_0 <- matrix("Y", ncol = size, nrow = size)
position_matrix_0[33, 33] <- "B"
position_matrix_0[66, 66] <- "O"

# mosaico  -----------------------------------------------------------

position_matrix_0 <- matrix("", nrow = size, ncol = size)

# Preencher a matriz em blocos de 10x10
for (i in seq(1, 100, by = 5)) {
  for (j in seq(1, 100, by = 5)) {
    # Escolher um valor aleatório para o bloco
    bloco_valor <- sample(strategies, 1)
    
    # Preencher o bloco 10x10 com esse valor
    position_matrix_0[i:(i+4), j:(j+4)] <- bloco_valor
  }
} # forma espiral

# a --------------------------------------------------------------------------

freq_0 <- table(position_matrix_0)/size^2
freq_0

### Teste da função
resultados <- evolucao_matriz(position_matrix_0, num_geracoes = 100)

# Exibe o dataframe com as frequências
#print(resultados$frequencias)


### Generate plot --------------------------------------------------------------
## Plot lattice
generate_image <- function(matrix_data, generation) {
  
  matrix_df <- matrix2longdf(matrix_data)
  
  p <- ggplot(matrix_df, aes(x = column, y = row, fill = value)) +
    geom_tile() +
    scale_fill_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                      name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
    scale_y_reverse() +
    labs(x = element_blank(), y = element_blank()) +
    theme_classic() +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    labs(title = paste("Geração:", generation)) +
    theme(text = element_text(size = 20))
  
  img_path <- paste0("./output/gifs/generation_", generation, ".png")
  ggsave(img_path, plot = p, width = 7, height = 5, dpi = 300)
  
  return(img_path)
}

image_paths <- c()

for (i in 1:length(resultados$matrizes)) {
  img_path <- generate_image(resultados$matrizes[[i]], i - 1)
  image_paths <- c(image_paths, img_path)
}

img_list <- lapply(image_paths, image_read)
gif <- image_animate(image_join(img_list), fps = 5) # Criar o GIF usando magick

image_write(gif, "./output/gifs/evolution_lines.gif") # Salvar o GIF

file.remove(image_paths) # Limpar arquivos temporários

browseURL("./output/gifs/evolution_lines.gif")

df_freq <- resultados$frequencias
df_freq <- df_freq %>%
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "freq")

ggplot(df_freq, aes(x = geracao, y = freq, color = as.factor(strategy))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_y_continuous(limits = c(0, 1))
