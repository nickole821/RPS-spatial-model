library(tidyverse)
library(cowplot)

df_freq <- read.csv("./output/data/stochastic/simetric_payoff-matrix/neighbors/mean_p05.csv",
                    header = T)

str(df_freq)

df_freq <- df_freq[,-1]

df_media <- df_freq %>%
  group_by(Generation, strategy, prob) %>%
  summarise(freq_media = mean(freq), freq_sd = sd(freq), .groups = "drop")
df_media

df_media <- df_media %>%
  filter(prob == 0.5)

df_O <- filter(df_media, strategy == "O")
df_B <- filter(df_media, strategy == "B")
df_Y <- filter(df_media, strategy == "Y")

freq_MF_O <- fft(df_O$freq_media)
freq_MF_Y <- fft(df_Y$freq_media)
freq_MF_B <- fft(df_B$freq_media)

mod_MF_O <- Mod(freq_MF_O)
mod_MF_O <- mod_MF_O[-1]
mod_MF_Y <- Mod(freq_MF_Y)
mod_MF_Y <- mod_MF_Y[-1]
mod_MF_B <- Mod(freq_MF_B)
mod_MF_B <- mod_MF_B[-1]

n <- length(df_O$Generation) - 1
frequencias <- (0:(n-1)) / n

fft <- data.frame(
  Frequencia = rep(frequencias, 3),
  Magnitude = c(mod_MF_O, mod_MF_Y, mod_MF_B),
  Estrategia = rep(c("O", "Y", "B"), each = n)
)

str(fft)

fft %>%
  ggplot(aes(x = Frequencia, y = Magnitude, color = Estrategia)) +
  geom_line(linetype = 1, size = 1.3) +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  scale_x_continuous(limits = c(0, 0.5)) + # Normalização de Nyquist
  labs(x = "Frequência Normalizada",
       y = "Magnitude", title = "Transformada de Fourier, prob = 0.5") +
  theme_minimal() +
  theme(text = element_text(size = 24), legend.position = "bottom",
        legend.justification = "center")
