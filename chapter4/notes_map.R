library(tidyverse)
library(modelr)
library(rethinking)
library(MASS)
library(hexbin)

data(Howell1)
d <- as_tibble(Howell1)

d2 <- d %>% filter(age >=18)
d2 %>% ggplot() + 
  geom_histogram(aes(x=height, y=..density..), binwidth=2)

m4.1 <- map(
  flist = alist(
    height ~ dnorm(mu, sigma), 
    mu ~ dnorm(178, 20),
    sigma ~ dunif (0, 50)), 
  data = as.data.frame(d2) )

#summarise the MAP estimate
precis(m4.1)
vcov( m4.1 )
diag( vcov( m4.1 ) )
cov2cor( vcov( m4.1 ) )

#sample from the 2dim posterior
parameter_samples <- as_tibble(
  mvrnorm(n=1e4, mu=coef(m4.1), Sigma=vcov(m4.1))
)

#visualize the sampled parameters
parameter_samples %>% 
  ggplot(aes(x=mu,y=sigma)) + geom_point(alpha=0.01)
parameter_samples %>% ggplot(aes(x=mu,y=sigma)) + geom_hex()

#using weight as a predictor variable
d2 %>% ggplot() + geom_point(aes(weight,height))
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
parameter_samples <- as_tibble(
  mvrnorm(n=1e4, mu=coef(m4.3), 
          Sigma=vcov(m4.3)))

#plot the mean height at each weight, with 50 sampled lines
d2 %>% ggplot(aes(weight, height)) + geom_point() + 
  geom_abline(aes(intercept=coef(m4.3)["a"] , slope=coef(m4.3)["b"])) +
  geom_abline(data = parameter_samples[1:50,], 
              aes(intercept=a, slope=b), alpha=0.1)

#find the 89% highest posterior density interval of mu at 50kg
parameter_samples <- 
  parameter_samples %>% mutate (mu_at_50kg = a + b*50)
HPDI(parameter_samples$mu_at_50kg, prob=0.89)

weight.seq <- seq( from=25 , to=70 , by=1 )
#sample 1000 from the posterior for each value in weight.seq
sampled_mu <- sapply( weight.seq , function(weight) 
  parameter_samples$a + parameter_samples$b*weight )
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
    n=nrow(parameter_samples) ,
    mean=parameter_samples$a + parameter_samples$b*weight ,
    sd=parameter_samples$sigma ) )
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
