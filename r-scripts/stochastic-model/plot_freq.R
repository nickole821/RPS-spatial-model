### plotting initial_freq ###

library(tidyverse)
library(purrr)

df_freq <- result$frequencies
str(df_freq)

df_freq <- df_freq %>%
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "freq")
str(df_freq)

df_media <- df_freq %>%
  group_by(Generation, strategy) %>%
  summarise(freq_media = mean(freq), .groups = "drop")
df_media

# Criar o gráfico com todas as simulações, a média destacada e os valores no final
ggplot(df_freq, aes(x = Generation, y = freq, color = as.factor(strategy))) +
  geom_line(size = 1, alpha = 1) +
  geom_line(data = df_media, aes(x = Generation, y = freq_media, 
                                 color = as.factor(strategy),
                                 group = strategy), 
            size = 1.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "death") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24)) +
  scale_y_continuous(lim = c(0,1))
