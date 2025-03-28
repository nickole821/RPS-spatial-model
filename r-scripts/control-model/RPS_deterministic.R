install.packages("deSolve")
require(deSolve)


#Resolvendo numericamente como x e y variam no tempo
game.theory.RPS <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        wBlue   =  mat.payoff[1,1]*x + mat.payoff[1,2]*y+  mat.payoff[1,3]*(1-x-y)
        wYellow =  mat.payoff[2,1]*x + mat.payoff[2,2]*y+  mat.payoff[2,3]*(1-x-y)
        wOrange =  mat.payoff[3,1]*x + mat.payoff[3,2]*y+  mat.payoff[3,3]*(1-x-y)
        dx.dt=x*(wBlue-(x*wBlue+y*wYellow+(1-x-y)*wOrange)) # eq dif de freq de azul 
        dy.dt=y*(wYellow-(x*wBlue+y*wYellow+(1-x-y)*wOrange)) # eq dif de freq de amarelo
        return(list(c(dx.dt, dy.dt)))
    })
}

mat.payoff=matrix(c(0.5,1,0,
                    0,0.5,1, 
                    1,0,0.5),byrow=TRUE, nrow=3) # matriz de payoffs
mat.payoff 
#se quiser, vc pode fazer as oscialcoes com maior periodo, aí só multiplicar a matriz por um numero entre 0 e 1. Por ex:
#mat.payoff = mat.payoff*0.6

#Parametros do modelo: so a matriz de payoffs
Pars <- c(mat.payoff= mat.payoff)
#Freq inicial de cooperadores
State <- c(x=0.495, y=0.495) # x = B, y = Y, out = O
#Tempo a ser rodado a solucao numerica
Time <- seq(0, 100, by = 1)

out <- as.data.frame(ode(func = game.theory.RPS, y = State, parms = Pars,
                         times = Time))
colnames(out)=c("geracao", "B", "Y")

out$O=1-out$B-out$Y # definindo w=1-x-y como a frequencia de orange
out
#write.csv(out,"./data/freq-RPS-meanfield.csv", row.names = FALSE)


df_freq <- out
df_freq <- df_freq %>%
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "freq")
str(df_freq)

plot_freq <- ggplot(df_freq, aes(x = geracao, y = freq, color = as.factor(strategy))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Gerações", y = "Frequência", title = "O = 0,001 Y = 0,495 B = 0,495") +
  scale_color_manual(values = c("O" = "tomato2", "Y" = "gold2", "B" = "blue"),
                     name = "Estratégia", labels = c("Azul", "Laranja", "Amarelo")) +
  theme(text = element_text(size = 24)) +
  scale_y_continuous(limits = c(0, 1))

ggsave(filename = "./output/figures/initial_freq/deterministic/O0001-Y049-B049.pdf",
       plot = plot_freq,
       device = pdf, width = 30, height = 25, units = "cm")
