library(tidyverse)
library(modelr)
library(rethinking)
library(MASS)

data(Howell1)
d <- as_tibble(Howell1)
d2 <- d %>% filter(age <18)
d2 %>% ggplot() + geom_point(aes(weight,height))

model <- map(flist = alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*d2$weight,
  a ~ dnorm(50,20),
  b ~ dunif(0,10),
  sigma ~ dunif(0,50)),
  data = as.data.frame(d2)
)
#the predicted b is 2.7cm/kg.

sampled_parameters <- as_tibble(mvrnorm(n=1e4,
                                        mu=coef(model),
                                        Sigma=vcov(model))
)

weights <- seq_range(d2$weight, by=1, pretty=TRUE)
sampled_mu <- sapply(weights, function(weight)
  sampled_parameters$a + sampled_parameters$b*weight)
simulated_height <- sapply(weights, function(weight) 
  rnorm(n=nrow(sampled_mu),
        mean=sampled_parameters$a + sampled_parameters$b*weight,
        sd=sampled_parameters$sigma
))
bounds <- tibble(
  weight=weights,
  mu = apply(sampled_mu, 2, mean),
  mu_lb = apply( sampled_mu , 2 , HPDI , prob=0.89 )[1,],
  mu_ub = apply( sampled_mu , 2 , HPDI , prob=0.89 )[2,],
  height_lb = apply(simulated_height, 2, HPDI, prob=0.89)[1,],
  height_ub = apply(simulated_height, 2, HPDI, prob=0.89)[2,])

ggplot(d2) + 
  geom_point(mapping=aes(weight, height)) + 
  geom_abline(aes(intercept=coef(model)["a"] , slope=coef(model)["b"])) +  
  geom_ribbon(data=bounds, mapping=aes(x = weight, ymin=mu_lb, ymax=mu_ub),
              alpha=0.5) + 
  geom_ribbon(data=bounds, mapping=aes(x = weight, ymin=height_lb, ymax=height_ub),
              alpha=0.1, fill='blue') + 
  coord_cartesian(ylim=c(0,200))