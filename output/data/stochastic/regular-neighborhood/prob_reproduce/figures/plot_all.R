library(tidyverse)

df <- read.csv("./output/data/stochastic/regular-neighborhood/prob_reproduce/mean/freq_mean_sd.csv",
                                 header = T)

str(df)

plot <- df %>%
  ggplot(aes(x = Generation, y = freq_media, color = strategy)) +
  geom_line(size = 1) +
  labs(x = "Tempo", y = "Frequência média") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~as.factor(prob)) +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24))

ggsave(filename = "./output/data/stochastic/regular-neighborhood/prob_reproduce/figures/plot_all.pdf",
       plot = plot, device = pdf, width = 40, height = 25, units = "cm")
