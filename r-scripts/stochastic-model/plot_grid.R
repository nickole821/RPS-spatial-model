source("./r-scripts/stochastic-model/functions.R")

library(readr)
library(ribiosUtils)
library(magick)

strategies <- c("O", "Y", "B")
a <- 2
b <- 1/a
payoff_matrix <- matrix(c(1, b, a,
                          a, 1, b,
                          b, a, 1), byrow = T, nrow = 3)
colnames(payoff_matrix) <- strategies
rownames(payoff_matrix) <- strategies
payoff_matrix
#n_sim <- 100
prob_reproduce <- 0.5
size <- 100

dir.create("./output/data/stochastic/simetric_payoff-matrix/neighbors/temporary")

seeds <- readRDS("data/sementes_simulacao.rds")

set.seed(seeds[1])
position_matrix <- matrix(sample(strategies, size^2, replace = TRUE),
                          nrow = size, ncol = size)

result <- simulation(position_matrix, payoff_matrix, num_generations = 100,
                     prob_reproduce, strategy = TRUE, devide = TRUE)

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
  
  img_path <- paste0("./output/data/stochastic/simetric_payoff-matrix/neighbors/temporary/", generation, ".png")
  ggsave(img_path, plot = p, width = 7, height = 5, dpi = 300)
  
  return(img_path)
}

image_paths <- c()

for (i in 1:length(result$matrices)) {
  img_path <- generate_image(result$matrices[[i]], i - 1)
  image_paths <- c(image_paths, img_path)
}

img_list <- lapply(image_paths, image_read)
gif <- image_animate(image_join(img_list), fps = 10) # Criar o GIF usando magick

image_write(gif, "./output/data/stochastic/simetric_payoff-matrix/neighbors/devide_fitness.gif") # Salvar o GIF

file.remove(image_paths) # Limpar arquivos temporários

browseURL("./output/data/stochastic/simetric_payoff-matrix/neighbors/devide_fitness.gif")
 