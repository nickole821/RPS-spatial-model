df_freq <- read.csv("./output/data/stochastic/neighbors/real-r/p05.csv",
                     header = T)
str(df_freq)
df_freq$freq_0_O

df_freq <- df_freq[,-1]

df_freq <- df_freq %>%
  filter(prob == 0.5)
str(df_freq)

df_media <- df_freq %>%
  group_by(Generation, strategy, prob) %>%
  summarise(freq_media = mean(freq), freq_sd = sd(freq), .groups = "drop")
df_media

freq_p05 <- ggplot(df_freq, aes(x = Generation, y = freq, color = as.factor(strategy),
                              group = interaction(sim, strategy))) +
  geom_line(size = 0.5, alpha = 0.3) +
  geom_line(data = df_media, aes(x = Generation, y = freq_media, 
                                 color = as.factor(strategy),
                                 group = strategy), 
            size = 1.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "prob = 0.5, real r") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24)) +
  scale_y_continuous(lim = c(0,1))

ggsave(filename = "./output/data/stochastic/neighbors/real-r/freq_p05.pdf",
       plot = freq_p05, device = pdf, width = 30, height = 25, units = "cm")

write.csv(df_media, "./output/data/stochastic/neighbors/mean_p05.csv")

freq_mean_p05 <- ggplot(df_media, aes(x = Generation, y = freq_media,
                                      color = as.factor(strategy), group = strategy)) +
  geom_ribbon(aes(ymin = freq_media - freq_sd, ymax = freq_media + freq_sd, 
                  fill = as.factor(strategy)), alpha = 0.6, color = NA) +
  geom_line(size = 1) +  # Linha da média
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "Média, prob = 0.5") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_fill_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"))+
  theme(text = element_text(size = 24))

ggsave(filename = "./output/data/stochastic/neighbors/mean_p05.pdf",
       plot = freq_mean_p05, device = pdf, width = 30, height = 25, units = "cm")

