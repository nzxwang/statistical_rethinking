library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

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

m5.2 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = as.data.frame(d) )

sampled_parameters_m5.2 <- as_tibble(
  mvrnorm(n=1e4, 
          mu=coef(m5.2), 
          Sigma=vcov(m5.2)))

Marriage_sequence <- seq_range(d$Marriage.s, n=30, pretty=TRUE)
sampled_mu_m5.2 <- sapply(Marriage_sequence, function (rate)
  sampled_parameters_m5.2$a + sampled_parameters_m5.2$bR*rate)
bounds_m5.2 <- tibble(  
  rate = Marriage_sequence,
  mu_lb = apply( sampled_mu_m5.2 , 2 , PI , prob=0.89 )[1,],
  mu_ub = apply( sampled_mu_m5.2 , 2 , PI , prob=0.89 )[2,])

d %>% ggplot() + 
  geom_point(aes(Marriage.s, Divorce)) +
  geom_abline(aes(intercept=coef(m5.2)["a"] , slope=coef(m5.2)["bR"])) +
  geom_ribbon(data=bounds_m5.2, mapping=aes(x = rate, ymin=mu_lb, ymax=mu_ub),
              alpha=0.1, fill='blue')

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
#bR is much less than bA
precis(m5.3)

m5.4 <- map(
  alist(
    Marriage.s ~ dnorm( mu , sigma ) ,
    mu <- a + b*MedianAgeMarriage.s ,
    a ~ dnorm( 0 , 10 ) ,
    b ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data = as.data.frame(d) )

#visualize the residual marriage rate in each state
#after accounting for median age at marriage
d %>% ggplot() + 
  geom_point(aes(MedianAgeMarriage.s, Marriage.s)) +
  geom_abline(aes(intercept=coef(m5.4)["a"] , slope=coef(m5.4)["b"])) +
  geom_segment(aes(x=MedianAgeMarriage.s, xend=MedianAgeMarriage.s, y=Marriage.s,
                   yend=coef(m5.4)["a"] + coef(m5.4)["b"]*MedianAgeMarriage.s), color='red', alpha=0.33)
#compute them  
d <- d %>% mutate(muMarriage.s=coef(m5.4)['a'] + coef(m5.4)['b']*MedianAgeMarriage.s,
                  reMarriage.s=Marriage.s - muMarriage.s)
m5.4.1 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + b*reMarriage.s ,
    a ~ dnorm( 0 , 10 ) ,
    b ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data = as.data.frame(d) )
d %>% ggplot() +
  geom_point(aes(reMarriage.s, Divorce)) +
  geom_abline(aes(intercept=coef(m5.4.1)["a"] , slope=coef(m5.4.1)["b"]))

#counterfactual plot
cfx <- tibble(
  Marriage.s=seq_range(c(-3,3), n=30, pretty=TRUE),
  MedianAgeMarriage.s=mean(d$MedianAgeMarriage.s)
)
sampled_parameters_m5.3 <- as_tibble(
  mvrnorm(n=1e4, 
          mu=coef(m5.3), 
          Sigma=vcov(m5.3))
  )
sampled_mu_m5.3 <- sapply(cfx$Marriage.s, function (rate)
  sampled_parameters_m5.3$a + sampled_parameters_m5.3$bR*rate + 
    sampled_parameters_m5.3$bA*cfx$MedianAgeMarriage.s[[1]]
  )
simulated_divorce <- mapply(function(rate, age) 
  rnorm(n=nrow(sampled_mu_m5.3),
        mean=sampled_parameters_m5.3$a + sampled_parameters_m5.3$bR*rate +sampled_parameters_m5.3$bA*age,
        sd=sampled_parameters_m5.3$sigma
  ), cfx$Marriage.s, cfx$MedianAgeMarriage.s
  )
cfx <- cfx %>% 
  mutate(mu.mean = apply(sampled_mu_m5.3, 2, mean),
         mu.lb = apply(sampled_mu_m5.3, 2, PI)[1,],
         mu.ub = apply(sampled_mu_m5.3, 2, PI)[2,],
         divorce.lb = apply(simulated_divorce, 2, PI)[1,],
         divorce.ub = apply(simulated_divorce, 2, PI)[2,]
         )
ggplot(cfx) + 
  geom_abline(aes(intercept=coef(m5.3)["a"] , slope=coef(m5.3)["bR"])) +  
  geom_ribbon(mapping=aes(x = Marriage.s, ymin=mu.lb, ymax=mu.ub),
              alpha=0.5) + 
  geom_ribbon(mapping=aes(x = Marriage.s, ymin=divorce.lb, ymax=divorce.ub),
              alpha=0.1, fill='blue') + 
  coord_cartesian(xlim=c(-2,3)
  )

pp <- dplyr::select(d, Location, MedianAgeMarriage.s, Marriage.s, Divorce)
sampled_mu_m5.3 <- mapply(function (rate, age)
  sampled_parameters_m5.3$a + sampled_parameters_m5.3$bR*rate + 
    sampled_parameters_m5.3$bA*age,
  pp$Marriage.s, pp$MedianAgeMarriage.s
)
simulated_divorce <- mapply(function(rate, age) 
  rnorm(n=nrow(sampled_mu_m5.3),
        mean=sampled_parameters_m5.3$a + sampled_parameters_m5.3$bR*rate +sampled_parameters_m5.3$bA*age,
        sd=sampled_parameters_m5.3$sigma
  ), pp$Marriage.s, pp$MedianAgeMarriage.s
)
pp <- pp %>% 
  mutate(mu.mean = apply(sampled_mu_m5.3, 2, mean),
         mu.lb = apply(sampled_mu_m5.3, 2, PI)[1,],
         mu.ub = apply(sampled_mu_m5.3, 2, PI)[2,],
         divorce.lb = apply(simulated_divorce, 2, PI)[1,],
         divorce.ub = apply(simulated_divorce, 2, PI)[2,],
         error = Divorce - mu.mean,
         error.lb = Divorce - mu.lb,
         error.ub = Divorce - mu.ub
  )
pp %>% ggplot() + geom_point(aes(Divorce,mu.mean)) + 
  geom_errorbar(aes(x= Divorce, ymin=mu.lb, ymax=mu.ub)) +
  geom_abline(color='green')
pp %>% ggplot() + 
  geom_point(aes(error, fct_reorder(Location,error))) +
  geom_errorbarh(aes(xmin=error.lb, xmax=error.ub,y=fct_reorder(Location,error)))
