library(tidyverse)
library(modelr)
library(rethinking)
library(MASS)

data(Howell1)
d <- as_tibble(Howell1)
d <- d %>% mutate(weight.s = (weight-mean(d$weight))/sd(d$weight),
         weight.s2 = weight.s^2)

d %>% ggplot() + geom_point(aes(weight.s,height)) + coord_cartesian(ylim=c(0,200))

m4.5 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*d$weight.s + b2*weight.s2 ,
    a ~ dnorm( 178 , 100 ) ,
    b1 ~ dnorm( 0 , 10 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=as.data.frame(d) )
sampled_params <- as_tibble(
  mvrnorm(n=1e4, mu=coef(m4.5), Sigma = vcov(m4.5)))

weight.s_sequence <- seq(-2,2,0.05)
sampled_mu <- sapply(weight.s_sequence, function(weight.s) 
             sampled_params$a + 
               sampled_params$b1*weight.s + 
               sampled_params$b2*weight.s^2 )

simulated_height <- sapply( weight.s_sequence , function(weight.s)
  rnorm(
    n=nrow(sampled_mu) ,
    mean=samples$a + samples$b1*weight.s + samples$b2*weight.s^2,
    sd=samples$sigma ) )

data <- tibble(
  weight.s=weight.s_sequence,
  mu = apply(sampled_mu, 2, mean),
  mu_lb = apply( sampled_mu , 2 , HPDI , prob=0.89 )[1,],
  mu_ub = apply( sampled_mu , 2 , HPDI , prob=0.89 )[2,],
  height_lb = apply(simulated_height, 2, HPDI, prob=0.89)[1,],
  height_ub = apply(simulated_height, 2, HPDI, prob=0.89)[2,])