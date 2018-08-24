library(tidyverse)
library(rethinking)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
as_tibble(samples) %>% ggplot() + geom_histogram(aes(value))

#3E1, 3E2, 3E3
posterior_below_0.2 <- sum(samples < 0.2)/10000
posterior_above_0.8 <- sum(samples>0.8)/10000
posterior_btwn_0.2_0.8 <- sum(samples>0.2 & samples<0.8)/10000

#3E4, 3E5
`20th_percentile` <- quantile(samples, 0.2)
`80th_percentile` <- quantile(samples, 0.8)

#3E6
rethinking::HPDI(samples, prob=0.66)

#3E7
quantile(samples, c(1/6,5/6))

#3M1
l<- 1000
prob <- tibble(
  p_grid = seq(from=0, to=1, length.out=l),
  prior = rep(1, l),
  likelihood = dbinom(8, size = 15, prob=p_grid),
  posterior = likelihood * prior / sum(likelihood * prior)
)
prob %>% ggplot() + geom_col(aes(x=p_grid, y=posterior))

#3M2
samples <- sample(prob$p_grid, prob = prob$posterior, size=1e4, replace=TRUE)
rethinking::HPDI(samples, prob=0.9)
as_tibble(samples) %>% ggplot() + geom_histogram(aes(x=value))
