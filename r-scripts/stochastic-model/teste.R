source("./r-scripts/stochastic-model/functions.R")

library(magick)
library(tidyverse)
library(ribiosUtils)

strategies <- c("O", "Y", "B")
a <- 2
b <- 1/a
payoff_matrix <- matrix(c(1, b, a,
                          a, 1, b,
                          b, a, 1), byrow = T, nrow = 3)
colnames(payoff_matrix) <- strategies
rownames(payoff_matrix) <- strategies
payoff_matrix
prob_reproduce <- 0.5
size <- 100

position_matrix <- matrix(sample(strategies, size^2, replace = TRUE),
                          nrow = size, ncol = size)

result <- simulation(position_matrix, payoff_matrix, num_generations = 100,
                     prob_reproduce, strategy = TRUE)

df_freq <- result$frequencies

df_freq <- df_freq %>%
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "freq")

str(df_freq)

# Calcular a média das estratégias por geração
df_media <- df_freq %>%
  group_by(geracao, strategy) %>%
  summarise(freq_media = mean(freq), .groups = "drop")
df_media

# Criar o gráfico com todas as simulações, a média destacada e os valores no final
ggplot(df_freq, aes(x = Generation, y = freq, color = as.factor(strategy))) +
  geom_line() +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "O = 0,25 Y = 0,25 B = 0,5") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo"))


generate_image <- function(matrix_data, generation) {
  
  matrix_df <- ribiosUtils::matrix2longdf(matrix_data)
  
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

for (i in 1:length(result$matrices)) {
  img_path <- generate_image(result$matrices[[i]], i - 1)
  image_paths <- c(image_paths, img_path)
}

img_list <- lapply(image_paths, image_read)
gif <- image_animate(image_join(img_list), fps = 5) # Criar o GIF usando magick

image_write(gif, "./output/gifs/evolution_radio.gif") # Salvar o GIF

file.remove(image_paths) # Limpar arquivos temporários

browseURL("./output/gifs/evolution_radio.gif")
