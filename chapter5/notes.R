library(tidyverse)
library(modelr)
library(rethinking)
library(MASS)

data(WaffleDivorce)
d <- as_tibble(WaffleDivorce) 
d <- d %>% mutate(MedianAgeMarriage.s = (MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage),
                  Marriage.s = (Marriage - mean(d$Marriage))/sd(d$Marriage)
                  )
m5.1 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bA * MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = as.data.frame(d) )

sampled_parameters <- as_tibble(
  mvrnorm(n=1e4, 
          mu=coef(m5.1), 
          Sigma=vcov(m5.1)))

MAM_sequence <- seq_range(d$MedianAgeMarriage.s, n=30, pretty=TRUE)

sampled_mu <- sapply(MAM_sequence, function (age)
  sampled_parameters$a + sampled_parameters$bA*age)
bounds <- tibble(  
  age = MAM_sequence,
  mu_lb = apply( sampled_mu , 2 , PI , prob=0.89 )[1,],
  mu_ub = apply( sampled_mu , 2 , PI , prob=0.89 )[2,])

d %>% ggplot() + 
  geom_point(aes(MedianAgeMarriage.s, Divorce)) +
  geom_abline(aes(intercept=coef(m5.1)["a"] , slope=coef(m5.1)["bA"])) +
  geom_ribbon(data=bounds, mapping=aes(x = age, ymin=mu_lb, ymax=mu_ub),
              alpha=0.1, fill='blue')

#here another variable affects the divorce rate.
# m5.2 <- map(
#   alist(
#     Divorce ~ dnorm( mu , sigma ) ,
#     mu <- a + bR * Marriage.s ,
#     a ~ dnorm( 10 , 10 ) ,
#     bR ~ dnorm( 0 , 1 ) ,
#     sigma ~ dunif( 0 , 10 )
#   ) , data = d )

m5.3 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data = as.data.frame(d) )
