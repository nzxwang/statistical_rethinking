library(tidyverse)
library(modelr)
library(rethinking)
library(MASS)

data(Howell1)
d <- as_tibble(Howell1)

d2 <- d %>% filter(age >=18)
d2 %>% ggplot() + geom_histogram(aes(x=height, y=..density..), binwidth=2)

m4.2 <- map(
  flist = alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 0.1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data = as.data.frame(d2) )
precis( m4.2 )

m4.1 <- map(
  flist = alist(
    height ~ dnorm(mu, sigma), 
    u ~ dnorm(178, 20),
    sigma ~ dunif (0, 50)), 
  data = as.data.frame(d2) )
precis(m4.1)
vcov( m4.1 )
diag( vcov( m4.1 ) )
cov2cor( vcov( m4.1 ) )

#sample from the 2dim posterior
post <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )

#these visualisations are very similar to those of the grid sample
post %>% ggplot(aes(x=mu,y=sigma)) + geom_point(alpha=0.01)
library(hexbin)
post %>% ggplot(aes(x=mu,y=sigma)) + geom_hex()

#using weight as a predictor variable

m4.3 <- map(
  flist=alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=as.data.frame(d2) )
precis( m4.3, corr=TRUE )
samples <- as_tibble(extract.samples( m4.3 ))
d2 %>% ggplot(aes(weight, height)) + geom_point() + 
  geom_abline(aes(intercept=coef(m4.3)["a"] , slope=coef(m4.3)["b"])) +
  geom_abline(data = post[1:50,], 
              aes(intercept=a, slope=b), alpha=0.1)

#find the 89% highest posterior density interval of mu at 50kg
samples <- samples %>% mutate (mu_at_50kg = a + b*50)
HPDI(post$mu_at_50kg, prob=0.89)

weight.seq <- seq( from=25 , to=70 , by=1 )
#sample 1000 from the posterior for each value in weight.seq
sampled_mu <- sapply( weight.seq , function(weight) samples$a + samples$b*weight )
str(sampled_mu)

pred <- tibble(weight.seq=weight.seq,
               mu = apply( sampled_mu , 2 , mean ),
               mu_lb = apply( sampled_mu , 2 , HPDI , prob=0.89 )[1,],
               mu_ub = apply( sampled_mu , 2 , HPDI , prob=0.89 )[2,]
               )
d2 %>% ggplot() + geom_point(aes(weight, height)) + 
  geom_abline(aes(intercept=coef(m4.3)["a"] , slope=coef(m4.3)["b"])) +  
  geom_ribbon(data=pred, mapping=aes(x = weight.seq, ymin=mu_lb, ymax=mu_ub),
              alpha=0.2)

simulated_height <- sapply( weight.seq , function(weight)
  rnorm(
    n=nrow(samples) ,
    mean=samples$a + samples$b*weight ,
    sd=samples$sigma ) )
simulated <- pred %>% mutate(
  height_lb = apply(simulated_height, 2, HPDI, prob=0.89)[1,],
  height_ub = apply(simulated_height, 2, HPDI, prob=0.89)[2,]
)
d2 %>% ggplot() + geom_point(aes(weight, height)) + 
  geom_abline(aes(intercept=coef(m4.3)["a"] , slope=coef(m4.3)["b"])) +  
  geom_ribbon(data=pred, mapping=aes(x = weight.seq, ymin=mu_lb, ymax=mu_ub),
              alpha=0.5) + 
  geom_ribbon(data=simulated, mapping=aes(x = weight.seq, ymin=height_lb, ymax=height_ub),
              alpha=0.1, fill='blue')
