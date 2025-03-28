source("./r-scripts/stochastic-model/functions.R")

strategies <- c("O", "Y", "B") # agents in the game

a <- 2
b <- 1/a
payoff_matrix <- matrix(c(1, b, a,
                          a, 1, b,
                          b, a, 1), byrow = T, nrow = 3)
colnames(payoff_matrix) <- strategies
rownames(payoff_matrix) <- strategies
payoff_matrix # payoff matrix of the game

size <- 100
position_matrix <- matrix(sample(strategies, size^2, replace = TRUE,
                                 prob = c(0.5, 0.25, 0.25)),
                          nrow = size, ncol = size) # random initial position
position_matrix

prob_death <- 0.5  # probability of death (goes from 0 to 1) equal to all players

resultados <- simulation(position_matrix, payoff_matrix, num_generations = 100, prob_death = 0.5)

generate_image <- function(matrix_data, generation, output_dir) {
  
  # Converter matriz para formato longo (data.frame)
  matrix_df <- matrix2longdf(matrix_data)
  
  # Criar o gráfico
  p <- ggplot(matrix_df, aes(x = column, y = row, fill = value)) +
    geom_tile() +
    scale_fill_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                      name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
    scale_y_reverse() +
    labs(title = paste("Geração:", generation), x = NULL, y = NULL) +
    theme_classic() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          text = element_text(size = 20))
  
  # Definir o caminho do arquivo
  img_path <- file.path(output_dir, paste0("frame_", generation, ".png"))
  
  # Salvar imagem
  ggsave(img_path, plot = p, width = 7, height = 5, dpi = 300)
  
  return(img_path)
}

# Criar diretório para salvar imagens temporárias
output_dir <- "./output/gifs/temporary"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Gerar imagens para todas as matrizes
image_paths <- map_chr(seq_along(resultados$matrices), 
                       ~ generate_image(resultados$matrices[[.x]], .x - 1, output_dir))

# Criar GIF
gif <- image_animate(image_join(lapply(image_paths, image_read)), fps = 5)

# Salvar o GIF final
gif_path <- "./output/gifs/stochastic-freq.gif"
image_write(gif, gif_path)

# Remover arquivos temporários
unlink(output_dir, recursive = TRUE)

# Abrir o GIF no navegador
browseURL(gif_path)

df_freq <- resultados$frequencies
df_freq <- df_freq %>%
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "freq")

ggplot(df_freq, aes(x = Generation, y = freq, color = as.factor(strategy))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_y_continuous(limits = c(0, 1))
