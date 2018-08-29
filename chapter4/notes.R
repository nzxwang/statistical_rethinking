library(tidyverse)
library(modelr)
library(rethinking)

normal <- replicate( 1000 , sum( runif(16,-1,1) ) )
as_tibble(normal) %>% ggplot() + 
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
  sample_mu = rnorm( 1e4 , mean=178 , sd=20 ),
  sample_sigma = runif( 1e4 , min=0 , max=50 ),
  prior_h = rnorm( 1e4 , mean=sample_mu , sd=sample_sigma )
) %>% ggplot() + 
  geom_histogram(aes(x=prior_h, y=..density..), binwidth=2) +
  ggtitle("prior distribution") +
  coord_cartesian(xlim = c(0,300))

#code 4.14
grid <- tibble(
  mu = seq( from=140, to=160 , length.out=200 ),
  sigma = seq( from=4 , to=9 , length.out=200 )
) %>% data_grid(sigma,mu) %>%
  mutate(log_likelihood = sum(dnorm(
    d2$height, mean=mu, sd=sigma, log=TRUE
  )))
#ask Chris why mutating this doesn't work
grid$LL <- sapply(seq_along(grid$mu), function (i) sum(dnorm(
  d2$height,
  mean=grid$mu[[i]],
  sd=grid$sigma[[i]],
  log=TRUE
)))
#the log of the product of the prior and likelihood
grid <- grid %>% mutate(prod = LL + dnorm(mu, 178,20,log=TRUE) + 
                          dunif(sigma,0,50,log=TRUE))
#the relative proportional posterior
grid <- grid %>% mutate(prob = exp(prod - max(grid$prod)))

#visualize the posterior given the parameters
grid %>% ggplot() + 
  geom_contour(aes(x=mu, y=sigma, z=prob))+
  coord_cartesian(xlim = c(153.5,155.5), ylim=c(7,8.5))
grid %>% ggplot() + 
  geom_tile(aes(x=mu, y=sigma, fill=prob)) +
  coord_cartesian(xlim = c(153.5,155.5), ylim=c(7,8.5))

#sample from the posterior
sample <- tibble(row = sample(seq_along(grid$prob), size=1e4, replace=TRUE,
                               prob=grid$prob),
                 mu = grid$mu[row],
                 sigma = grid$sigma[row])

sample %>% ggplot(aes(x=mu,y=sigma)) + geom_point(alpha=0.01)
sample %>% ggplot(aes(x=mu,y=sigma)) + geom_bin2d()
library(hexbin)
sample %>% ggplot(aes(x=mu,y=sigma)) + geom_hex()

hist(sample$mu)
hist(sample$sigma) #slightly longer right tail
