df_freq <- read.csv("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/freq.csv",
                     header = T)
str(df_freq)
df_freq <- df_freq[,-1]

df_media <- df_freq %>%
  group_by(Generation, strategy, prob) %>%
  summarise(freq_media = mean(freq), freq_sd = sd(freq), .groups = "drop")
df_media

ggplot(df_media, aes(x = Generation, y = freq_media, color = as.factor(strategy),
                     group = interaction(prob, strategy))) +
  geom_ribbon(aes(ymin = freq_media - freq_sd, ymax = freq_media + freq_sd, 
                  fill = as.factor(strategy)), alpha = 0.2, color = NA) +
  geom_line(alpha = 0.3, size = 0.5) +  # Linha da média
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "Frequência Média com Desvio Padrão") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_fill_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"))

write.csv(df_media, "./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/freq_mean_sd.csv")
