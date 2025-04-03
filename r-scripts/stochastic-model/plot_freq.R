### plotting initial_freq ###

library(tidyverse)
library(purrr)

# Defina o diretório principal onde estão as pastas
path <- "./output/data/stochastic/initial_freq/02-04-04"

files <- list.files(path, pattern = ".*freq\\.csv$",
                    full.names = TRUE, recursive = TRUE)

print(files)

freq_list <- lapply(files, read.csv, sep = ",", stringsAsFactors = FALSE)
freq_list[[1]]

df_freq <- bind_rows(freq_list, .id = "arquivo_origem")
df_freq <- df_freq[,-2]
df_freq <- df_freq %>%
  pivot_longer(cols = 3:5, names_to = "strategy", values_to = "freq")
df_freq <- df_freq %>% 
  rename(sim = arquivo_origem)
str(df_freq)

df_media <- df_freq %>%
  group_by(Generation, strategy) %>%
  summarise(freq_media = mean(freq), .groups = "drop")
df_media

# Criar o gráfico com todas as simulações, a média destacada e os valores no final
ggplot(df_freq, aes(x = Generation, y = freq, color = as.factor(strategy),
                                 group = interaction(sim, strategy))) +
  geom_line(size = 0.5, alpha = 0.3) +
  #geom_line(data = df_media, aes(x = Generation, y = freq_media, 
                                 #color = as.factor(strategy),
                                 #group = strategy), 
            #size = 1.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "Prob_reproduce_0") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24)) +
  scale_y_continuous(lim = c(0,1))

ggsave(filename = "./output/figures/initial_freq/O025-Y025-B05.pdf", plot = plot_freq,
       device = pdf, width = 30, height = 25, units = "cm")