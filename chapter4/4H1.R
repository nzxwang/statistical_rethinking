library(tidyverse)
library(modelr)
library(rethinking)
library(MASS)

#4H1
data(Howell1)
d <- as_tibble(Howell1)

#first visualize your data
d %>% ggplot() + geom_point(aes(weight,height)) + 
  coord_cartesian(ylim=c(0,200))

model <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*d$weight + b2*weight^2,
    a ~ dnorm( 50 , 100 ) ,
    b1 ~ dnorm( 0 , 10 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=as.data.frame(d) )

sampled_parameters <- as_tibble(mvrnorm(n=1e4,
                                        mu=coef(model),
                                        Sigma=vcov(model))
)
weight_sequence <- seq_range(d$weight, by=1, pretty=TRUE)
weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
sampled_mu <- sapply(weights, function(weight)
  sampled_parameters$a + 
    sampled_parameters$b1*weight + 
    sampled_parameters$b2*weight^2)

simulated_height <- sapply(weights, function(weight)
  rnorm(n=nrow(sampled_parameters),
        mean=sampled_parameters$a + 
              sampled_parameters$b1*weight + 
              sampled_parameters$b2*weight^2,
        sd = sampled_parameters$sigma))

answers <- tibble(
  weight = weights,
  expected_height= apply(simulated_height, 2, mean),
  mu_lb = apply(sampled_mu, 2, HPDI)[1,],
  mu_ub = apply(sampled_mu, 2, HPDI)[2,]
)
#visualize the expected heights
d %>% ggplot() + geom_point(aes(weight,height)) + 
  coord_cartesian(ylim=c(0,200)) + 
  geom_point(data=answers, aes(weight, expected_height), color='red') +
  geom_ribbon(data=answers, aes(x = weight, ymin=mu_lb, ymax=mu_ub), fill='red')
