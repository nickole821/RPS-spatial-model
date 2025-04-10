### plotting initial_freq ###

library(tidyverse)
library(purrr)

# Definir o diretório onde os arquivos estão
path <- "./output/data/initial_freq/O025-Y025-B05/freq"

# Listar todos os arquivos que começam com "freq_"
files <- list.files(path = path, pattern = "^freq_\\d+.csv", full.names = TRUE)

# Verificar se encontrou arquivos
if (length(files) == 0) {
  stop("Nenhum arquivo encontrado na pasta.")
} else {
  print("Tudo certo!")
}

# Ler todos os arquivos em uma lista
lista_files <- lapply(files, read.csv)  # Altere `read.csv` para outro formato se necessário

# Aplicando a função map2 para adicionar a coluna crescente
lista_files <- map2(lista_files, seq_along(lista_files),
                    ~ mutate(.x, simulacao = .y))

# Opcional: Unir todos os arquivos em um único data frame
df_freq <- do.call(rbind, lista_files)

df_freq <- df_freq %>%
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "freq")

str(df_freq)

# Calcular a média das estratégias por geração
df_media <- df_freq %>%
  group_by(geracao, strategy) %>%
  summarise(freq_media = mean(freq), .groups = "drop")
df_media

# Criar o gráfico com todas as simulações, a média destacada e os valores no final
plot_freq <- ggplot(df_freq, aes(x = geracao, y = freq, color = as.factor(strategy),
                    group = interaction(simulacao, strategy))) +
  geom_line(size = 0.5, alpha = 0.3) +
  geom_line(data = df_media, aes(x = geracao, y = freq_media, 
                                 color = as.factor(strategy),
                                 group = strategy), 
            size = 1.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "O = 0,25 Y = 0,25 B = 0,5") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24))

ggsave(filename = "./output/figures/initial_freq/O025-Y025-B05.pdf", plot = plot_freq,
       device = pdf, width = 30, height = 25, units = "cm")

