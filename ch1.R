library(tidyverse)
library(ggplot2)

N <- 100
t_max <- 500


population <- tibble(trait = sample(c("A", "A", "A", "B"), N, replace=TRUE))
output <- tibble(generation = 1:t_max, p = rep(NA, t_max))


output$p[1] <- sum(population$trait == "A") / N

for (t in 2:t_max) {
  previous_population <- population
  
  population <- tibble(trait = sample(previous_population$trait, N, replace=TRUE))
  
  output$p[t] <- sum(population$trait == "A") / N
}

ggplot(data = output, aes(y=p, x = generation)) +
  geom_line() +
  ylim(c(0, 1)) +
  theme_bw() +
  labs(y = "p (proportion of agents with trait A")
