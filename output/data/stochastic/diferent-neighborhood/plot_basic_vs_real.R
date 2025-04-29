library(tidyverse)

p0_basicneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/basic/mean/mean_p0.csv",
                                  header = T)
p0_basicneighborhood <- p0_basicneighborhood %>%
  mutate(raio = "basico")

p025_basicneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/basic/mean/mean_p025.csv",
                                    header = T)
p025_basicneighborhood <- p025_basicneighborhood %>%
  mutate(raio = "basico")

p05_basicneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/basic/mean/mean_p05.csv",
                                   header = T)
p05_basicneighborhood <- p05_basicneighborhood %>%
  mutate(raio = "basico")


p0_realneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/real-r/mean/mean_p0.csv",
                                header = T)
p0_realneighborhood <- p0_realneighborhood %>%
  mutate(raio = "real")

p025_realneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/real-r/mean/mean_p025.csv",
                                  header = T)
p025_realneighborhood <- p025_realneighborhood %>%
  mutate(raio = "real")

p05_realneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/real-r/mean/mean_p05.csv",
                                 header = T)
p05_realneighborhood <- p05_realneighborhood %>%
  mutate(raio = "real")


p0_adapneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/adaptative-neighborhood/mean/p0.csv",
                                header = T)
p0_adapneighborhood <- p0_adapneighborhood %>%
  mutate(raio = "adap")

p025_adapneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/adaptative-neighborhood/mean/p025.csv",
                                  header = T)
p025_adapneighborhood <- p025_adapneighborhood %>%
  mutate(raio = "adap")

p05_adapneighborhood <- read.csv("./output/data/stochastic/diferent-neighborhood/adaptative-neighborhood/mean/p05.csv",
                                 header = T)
p05_adapneighborhood <- p05_adapneighborhood %>%
  mutate(raio = "adap")

regular_p0 <- read.csv("./output/data/stochastic/regular-neighborhood/prob_reproduce/mean/mean_p0.csv")
regular_p0 <- regular_p0 %>%
  mutate(raio = "homogenous")
regular_p025 <- read.csv("./output/data/stochastic/regular-neighborhood/prob_reproduce/mean/mean_p025.csv")
regular_p025 <- regular_p025 %>%
  mutate(raio = "homogenous")
regular_p05 <- read.csv("./output/data/stochastic/regular-neighborhood/prob_reproduce/mean/mean_p05.csv")
regular_p05 <- regular_p05 %>%
  mutate(raio = "homogenous")


df <- dplyr::bind_rows(p0_basicneighborhood, p025_basicneighborhood, p05_basicneighborhood,
                       p0_realneighborhood, p025_realneighborhood, p05_realneighborhood,
                       p0_adapneighborhood, p025_adapneighborhood, p05_adapneighborhood,
                       regular_p0, regular_p025, regular_p05)
str(df)

plot_raio <- df %>%
  ggplot(aes(x = Generation, y = freq_media)) +
  # Linhas coloridas (por strategy) para todos os tipos de raio exceto "homogenous"
  geom_line(data = df %>% filter(raio != "homogenous"),
            aes(color = strategy, linetype = as.factor(raio)), size = 1) +
  # Linha preta para o tipo de raio "homogenous"
  geom_line(data = df %>% filter(raio == "homogenous"),
            aes(linetype = as.factor(raio)), color = "black", size = 1) +
  labs(x = "Tempo", y = "Frequência média") +
  theme_minimal() +
  labs(linetype = "Tipo de\nvizinhança") +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~as.factor(prob) + strategy) +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_linetype_manual(values = c(basico = 4, real = 2, adap = 3, homogenous = 1),
                        labels = c("Y>O>B", "O>B=Y", "Adaptative Y", "O=Y=B")) +
  theme(text = element_text(size = 24))


ggsave(filename = "./output/data/stochastic/diferent-neighborhood/figure_raios.pdf",
       plot = plot_raio, device = pdf, width = 40, height = 25, units = "cm")
