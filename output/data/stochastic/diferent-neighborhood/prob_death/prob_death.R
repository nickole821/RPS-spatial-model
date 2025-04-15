source("./r-scripts/stochastic-model/function-prob_death.R")

library(readr)

strategies <- c("O", "Y", "B")
a <- 2
b <- 1/a
payoff_matrix <- matrix(c(1, b, a,
                          a, 1, b,
                          b, a, 1), byrow = T, nrow = 3)
colnames(payoff_matrix) <- strategies
rownames(payoff_matrix) <- strategies
payoff_matrix
n_sim <- 1
size <- 100

dir.create("./output/data/stochastic/diferent-neighborhood/prob_death")

seeds <- readRDS("data/sementes_simulacao.rds")

start_time <- Sys.time()
for (i in 1:n_sim) {
  cat("Rodando simulação", i, "\n")  
  
  set.seed(seeds[i])
  position_matrix <- matrix(sample(strategies, size^2, replace = TRUE),
                            nrow = size, ncol = size)
  
  result <- simulation_prob_death(position_matrix, payoff_matrix, num_generations = 100,
                                  strategy = TRUE, divide = FALSE, k = 0.03)
  
  pasta_resultados <- file.path("./output/data/stochastic/diferent-neighborhood/prob_death",
                                paste0("sim_", i))
  dir.create(pasta_resultados, recursive = TRUE)
  
  matriz_path <- file.path(pasta_resultados, paste0("matrix.csv"))
  freq_path <- file.path(pasta_resultados, paste0("freq.csv"))
  
  matrizes <- do.call(rbind, lapply(seq(1, length(result$matrices), by = 20), function(i) {
    data.frame(Generation = i - 1, as.data.frame(result$matrices[[i]]))
  }))
  
  write.csv(matrizes, matriz_path)
  write.csv(result$frequencies, freq_path)
}
cat("Acabou! Ufa! :)\n")
end_time <- Sys.time()
end_time - start_time

result$frequencies
