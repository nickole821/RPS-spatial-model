simulation_adap <- function(position_matrix, payoff_matrix, num_generations, prob_reproduce,
                              strategy, propO, divide) {
  
  # to store matrices and frequencies
  all_matrices <- list(position_matrix)  
  frequencies <- data.frame(Generation = 0,
                            O = sum(position_matrix == "O") / (size^2),
                            Y = sum(position_matrix == "Y") / (size^2),
                            B = sum(position_matrix == "B") / (size^2))
  
  
  # to obtain the 8' neighbors of the focal agent on the matrix with periodic boundaries
  get_neighbors <- function(i, j, position_matrix, strategy = FALSE, propO) {
    size <- nrow(position_matrix)
    neighbors <- c()
    
    if (strategy == FALSE) {
      i_seq <- ((i - 1):(i + 1) - 1) %% size + 1
      j_seq <- ((j - 1):(j + 1) - 1) %% size + 1
      neighbors_matrix <- position_matrix[i_seq, j_seq]
      neighbors <- as.vector(neighbors_matrix)
      neighbors <- neighbors[-5]
      
    } else {
      current_strategy <- position_matrix[i, j]
      
      if (current_strategy == "B") {
        # Vizinhança pequena (5x5 - 24 vizinhos)
        i_seq <- ((i - 2):(i + 2) - 1) %% size + 1
        j_seq <- ((j - 2):(j + 2) - 1) %% size + 1
        neighbors_matrix <- position_matrix[i_seq, j_seq]
        neighbors <- as.vector(neighbors_matrix)
        neighbors <- neighbors[-13]  # remove o próprio agente
        
      } else if (current_strategy == "O") {
        # Vizinhança grande (7x7 - 48 vizinhos)
        i_seq <- ((i - 3):(i + 3) - 1) %% size + 1
        j_seq <- ((j - 3):(j + 3) - 1) %% size + 1
        neighbors_matrix <- position_matrix[i_seq, j_seq]
        neighbors <- as.vector(neighbors_matrix)
        neighbors <- neighbors[-25]
        
      } else if (current_strategy == "Y") {
        # Primeiro, usar a vizinhança pequena pra inspecionar
        i_seq_small <- ((i - 2):(i + 2) - 1) %% size + 1
        j_seq_small <- ((j - 2):(j + 2) - 1) %% size + 1
        small_matrix <- position_matrix[i_seq_small, j_seq_small]
        small_neighbors <- as.vector(small_matrix)[-13]  # remove o centro
        prop_O <- sum(small_neighbors == "O") / length(small_neighbors)
        
        if (prop_O > propO) {
          # Adota vizinhança de B (pequena)
          neighbors <- small_neighbors
        } else {
          # Adota vizinhança de O (grande)
          i_seq_large <- ((i - 3):(i + 3) - 1) %% size + 1
          j_seq_large <- ((j - 3):(j + 3) - 1) %% size + 1
          large_matrix <- position_matrix[i_seq_large, j_seq_large]
          neighbors <- as.vector(large_matrix)
          neighbors <- neighbors[-25]
        }
      } else {
        # Fallback: padrão 3x3
        i_seq <- ((i - 1):(i + 1) - 1) %% size + 1
        j_seq <- ((j - 1):(j + 1) - 1) %% size + 1
        neighbors_matrix <- position_matrix[i_seq, j_seq]
        neighbors <- as.vector(neighbors_matrix)
        neighbors <- neighbors[-5]
      }
    }
    
    return(neighbors)
  }
  # to calculate the payoffs of two agents on the RPS-game
  get_payoff <- function(a1, a2, payoff_matrix) {
    index <- c("O" = 1, "Y" = 2, "B" = 3)
    return(payoff_matrix[index[a1], index[a2]])
  }
  
  # to calculate the fitness matrix based on the initial position matrix
  get_fitness_matrix <- function(position_matrix, divide) {
    fitness_matrix <- matrix(NA, nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        fitness_ID <- 0
        a <- get_neighbors(i, j, position_matrix, strategy, propO)
        for (z in 1:length(a)) {
          fitness_ID <- fitness_ID + get_payoff(position_matrix[i, j], a[z],
                                                payoff_matrix)
        }
        if (divide == TRUE) {
          fitness_ID <- fitness_ID / length(a)
        }
        fitness_matrix[i, j] <- fitness_ID
      }
    }
    return(fitness_matrix)
  }
  
  # to determine which agents are going to reproduce on the next generation (random)
  going_to_die <- function(position_matrix, prob_reproduce) {
    intermediate_matrix <- matrix(NA, nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        a <- runif(1, 0, 1)
        if (a > prob_reproduce) {
          intermediate_matrix[i, j] <- "E"  # agents died and the cell is empty
        } else {
          intermediate_matrix[i, j] <- position_matrix[i, j] # agent reproduced
        }
      }
    }
    return(intermediate_matrix)
  }
  
  # to generate the new matrix with the fittest neighbors
  get_new_matrix <- function(position_matrix_0, intermediate_matrix, fitness_matrix) {
    new_matrix <- matrix(NA, nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        if (intermediate_matrix[i, j] == "E") {
          neighbors_strategies <- get_neighbors(i, j, position_matrix_0, strategy, propO)
          neighbors_fitness <- get_neighbors(i, j, fitness_matrix, strategy, propO)
          max_fitness <- which.max(neighbors_fitness)
          new_matrix[i, j] <- neighbors_strategies[max_fitness]
        } else {
          new_matrix[i, j] <- position_matrix_0[i, j]
        }
      }
    }
    return(new_matrix)
  }
  
  # principal loop
  for (generation in 1:num_generations) {
    
    # step 1: calculate fitness matrix
    fitness_matrix <- get_fitness_matrix(position_matrix, divide)
    
    # step 2: select agents to reproduce and die
    intermediate_matrix <- going_to_die(position_matrix, prob_reproduce)
    
    # step 3: ocupying the empty spaces with fittest neighbors
    new_matrix <- get_new_matrix(position_matrix, intermediate_matrix, fitness_matrix)
    
    # save matrix
    all_matrices[[generation + 1]] <- new_matrix
    
    # save frequency
    frequencies <- rbind(frequencies, data.frame(Generation = generation,
                                                 O = sum(new_matrix == "O") / (size^2),
                                                 Y = sum(new_matrix == "Y") / (size^2),
                                                 B = sum(new_matrix == "B") / (size^2)))
    
    # new_matrix become the initial position matrix for the next generation
    position_matrix <- new_matrix
  }
  return(list(matrices = all_matrices, frequencies = frequencies))
}