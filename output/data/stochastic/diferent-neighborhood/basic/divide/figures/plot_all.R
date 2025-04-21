
library(tidyverse)

p0_divideneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/divide/mean/mean_p0.csv",
                                 header = T)
p025_divideneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/divide/mean/mean_p025.csv",
                                   header = T)
p05_divideneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/divide/mean/mean_p05.csv",
                                  header = T)

df <- dplyr::bind_rows(p0_divideneighborhood, p025_divideneighborhood, p05_divideneighborhood)
str(df)

plot_basic <- df %>%
  ggplot(aes(x = Generation, y = freq_media, linetype = as.factor(prob), color = strategy)) +
  geom_line(size = 1) +
  labs(title = "Raio Y > O > B, dividindo", x = "Tempo", y = "Frequência média") +
  theme_minimal() +
  labs(linetype = "Probabilidade de\nreprodução") +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~strategy) +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24))

ggsave(filename = "./output/data/stochastic/diferent-neighborhood/divide/figures/figure_all.pdf",
       plot = plot_basic, device = pdf, width = 40, height = 25, units = "cm")
