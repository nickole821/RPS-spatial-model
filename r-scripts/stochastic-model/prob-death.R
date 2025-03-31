source("./r-scripts/stochastic-model/functions.R")

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
n_sim <- 100
prob_death <- 0.5
size <- 100

dir.create("./output/data/stochastic/prob_death/0-5")

for (i in 1:n_sim) {
  cat("Rodando simulação", i, "\n")  
  
  set.seed(i)
  position_matrix <- matrix(sample(strategies, size^2, replace = TRUE),
                              nrow = size, ncol = size)
  
  result <- simulation(position_matrix, payoff_matrix, num_generations = 100,
                       prob_death)
  
  pasta_resultados <- file.path("./output/data/stochastic/prob_death/0-5/",
                                paste0("sim_", i))
  dir.create(pasta_resultados, recursive = TRUE)
  
  matriz_path <- file.path(pasta_resultados, paste0("matrix.csv"))
  freq_path <- file.path(pasta_resultados, paste0("freq.csv"))
  
  matrizes <- do.call(rbind, lapply(1:length(result$matrices), function(i) {
    data.frame(Generation = i - 1, as.data.frame(result$matrices[[i]]))
  }))
  
  write.csv(matrizes, matriz_path)
  write.csv(result$frequencies, freq_path)
}
cat("Acabou! Ufa! :)\n")

