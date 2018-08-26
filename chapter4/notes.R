library(tidyverse)
library(rethinking)

pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
as_tibble(pos) %>% ggplot() + 
  geom_histogram(aes(x=value, y=..density..)) + 
  geom_density(aes(x=value))

data(Howell1)
d <- as_tibble(Howell1)

d2 <- d %>% filter(age >=18)
d2 %>% ggplot() + geom_histogram(aes(x=height, y=..density..), binwidth=2)

#mean prior
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
#sd prior
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

tibble(
  sample_mu = rnorm( 1e4 , 178 , 20 ),
  sample_sigma = runif( 1e4 , 0 , 50 ),
  prior_h = rnorm( 1e4 , sample_mu , sample_sigma )
) %>% ggplot() + 
  geom_histogram(aes(x=prior_h, y=..density..), binwidth=2) +
  ggtitle("prior distribution")

#code 4.14
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )
