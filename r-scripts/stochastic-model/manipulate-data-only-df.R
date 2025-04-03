### plotting initial_freq ###

library(tidyverse)
library(purrr)

# Defina o diretório principal onde estão as pastas
path <- "./output/data/deterministic/initial_freq/"

files <- list.files(path, pattern = "^freq.*\\.csv$",
                    full.names = TRUE, recursive = TRUE)

print(files)

freq_list <- lapply(files, read.csv, sep = ",", stringsAsFactors = FALSE)
freq_list[[2]]

df_freq <- bind_rows(freq_list, .id = NULL)
str(df_freq)
df_freq <- df_freq[,-1]
df_freq <- df_freq %>%
  pivot_longer(cols = 3:5, names_to = "strategy", values_to = "freq")
str(df_freq)
df_freq <- df_freq %>% 
  rename(sim = id)
str(df_freq)
df_freq <- df_freq %>% 
  mutate(prob = 0, freq_0_O = 0.25, freq_0_Y = 0.25, freq_0_B = 0.5)
str(df_freq)

write.csv(df_freq, "./output/data/deterministic/initial_freq/freq.csv")

##
path_all <- "./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/"

files_all <- list.files(path_all, pattern = "^p.*\\.csv$",full.names = TRUE,
                        recursive = TRUE)
print(files_all)

freq_list_all <- lapply(files_all, read.csv, sep = ",", stringsAsFactors = FALSE)
freq_list_all[[1]]

df_freq_all <- bind_rows(freq_list_all)
df_freq_all <- df_freq_all[,-1]
df_freq_all <- df_freq_all %>% 
  mutate(neighbour = "equal")
str(df_freq_all)

write.csv(df_freq_all, "./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/freq.csv")

unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-1", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-2", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-3", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-4", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-5", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-6", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-7", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-8", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/0-9", recursive = TRUE)
unlink("./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/1", recursive = TRUE)

a <- read.csv("./output/data/deterministic/initial_freq/freq.csv")
a <- a %>% 
  mutate(freq_0_O = 1/3, freq_0_Y = 1/3, freq_0_B = 1/3)
write.csv(a, "./output/data/stochastic/simetric_payoff-matrix/prob_reproduce/freq.csv")

