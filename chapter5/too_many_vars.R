library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

d <- tibble(
  height = rnorm(100,10,2),
  leg_prop = runif(100,0.4,0.5),
  leg_left = leg_prop*height + rnorm(1, 0 ,0.02),
  leg_right = leg_prop*height + rnorm(1, 0 ,0.02)
)
m5.8 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data= as.data.frame(d) )
precis(m5.8)
sampled5.8 <- as_tibble(
  mvrnorm(n=1e3, 
          mu=coef(m5.8), 
          Sigma=vcov(m5.8)))
#br+bl is correlated with height
sampled5.8 %>% ggplot() + geom_point(aes(br,bl), alpha=0.05)
sampled5.8 %>% ggplot() + 
  geom_histogram((aes(br+bl)), binwidth=0.01)

m5.9 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data= as.data.frame(d) )
precis(m5.9)

data(milk)
d <- as_tibble(milk)

m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d) )

m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bl*perc.lactose ,
    a ~ dnorm( 0.6 , 10 ) ,
    bl ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d) )
precis( m5.10 , digits=3 )
precis( m5.11 , digits=3 )
#both betas seem very like mirror images about zero

m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat + bl*perc.lactose ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    bl ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data= as.data.frame(d) )
precis( m5.12 , digits=3 )

#perc.fat and perc.lactose are practically redundant
cor( d$perc.fat , d$perc.lactose )
