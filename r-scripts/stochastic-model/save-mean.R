library(tidyverse)

df_freq <- read.csv("./output/data/stochastic/regular-neighborhood/prob_reproduce/p025.csv",
                     header = T)

str(df_freq)
df_freq$freq_0_O

df_freq <- df_freq[,-1]

df_freq <- df_freq %>%
  filter(prob == 0.25)
str(df_freq)

df_media <- df_freq %>%
  group_by(Generation, strategy, prob) %>%
  summarise(freq_media = mean(freq), freq_sd = sd(freq), .groups = "drop")
df_media

dir.create("./output/data/stochastic/diferent-neighborhood/real-r/divide/mean/")


write.csv(df_media,
          "./output/data/stochastic/regular-neighborhood/prob_reproduce/mean/mean_p025.csv")


freq_p0 <- ggplot(df_freq, aes(x = Generation, y = freq, color = as.factor(strategy),
                              group = interaction(sim, strategy))) +
  geom_line(size = 0.5, alpha = 0.3) +
  geom_line(data = df_media, aes(x = Generation, y = freq_media, 
                                 color = as.factor(strategy),
                                 group = strategy), 
            size = 1.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "real, divide = TRUE, p = 0") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24)) +
  scale_y_continuous(lim = c(0,1))

ggsave(filename = "./output/data/stochastic/diferent-neighborhood/real-r/divide/p0.pdf",
       plot = freq_p0, device = pdf, width = 30, height = 25, units = "cm")

freq_mean_p025 <- ggplot(df_media, aes(x = Generation, y = freq_media,
                                      color = as.factor(strategy), group = strategy)) +
  geom_ribbon(aes(ymin = freq_media - freq_sd, ymax = freq_media + freq_sd, 
                  fill = as.factor(strategy)), alpha = 0.6, color = NA) +
  geom_line(size = 1) +  # Linha da média
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "adaptative neighborhood, p = 0.25") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_fill_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"))+
  theme(text = element_text(size = 24))

ggsave(filename = "./output/data/stochastic/diferent-neighborhood/real-r/mean/mean_p025.pdf",
       plot = freq_mean_p025, device = pdf, width = 30, height = 25, units = "cm")

