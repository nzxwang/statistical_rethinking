library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

data(milk)
d <- milk %>%
  filter(complete.cases(milk)) %>%
  mutate(neocortex = neocortex.perc / 100) %>% 
  select(neocortex, mass, kcal.per.g)

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))

m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm( a , exp(log.sigma) )
  ) ,
  data=d , start=list(a=a.start,log.sigma=sigma.start) )
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex
  ) ,
  data=d , start=list(a=a.start,bn=0,log.sigma=sigma.start) )
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bm*log(mass)
  ) ,
  data=d , start=list(a=a.start,bm=0,log.sigma=sigma.start) )
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex + bm*log(mass)
  ) ,
  data=d , start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start) )

milk.models <- compare( m6.11 , m6.12 , m6.13 , m6.14 )

coeftab(m6.11,m6.12,m6.13,m6.14) %>% plot()

cfx <- tibble(
  neocortex = seq(from=0.5,to=0.8,length.out=30), # sequence of neocortex
  mass = rep(4.5,30) # average mass
)
sampled_mu_m6.14 <- link( m6.14 , data=cfx )
sampled_parameters_m6.14 <- as_tibble(
  mvrnorm(n=1e4, 
          mu=coef(m6.14), 
          Sigma=vcov(m6.14))
)
sampled_mu_m6.14 <- sapply(cfx$neocortex, function (neocortex)
  sampled_parameters_m6.14$a + sampled_parameters_m6.14$bn*neocortex + sampled_parameters_m6.14$bm*cfx$mass[[1]]
)
cfx <- cfx %>% 
  mutate(mu.mean = apply(sampled_mu_m6.14, 2, mean),
         mu.lb = apply(sampled_mu_m6.14, 2, PI)[1,],
         mu.ub = apply(sampled_mu_m6.14, 2, PI)[2,],
  )
ggplot(cfx) +
  geom_abline(aes(intercept=coef(m6.14)["a"] + coef(m6.14)['bm']*cfx$mass[[1]], slope=coef(m6.14)["bn"])) +
  geom_ribbon(mapping=aes(x = neocortex, ymin=mu.lb, ymax=mu.ub), alpha=0.5) +
  coord_cartesian(xlim=c(0.53, 0.78), ylim=c(0.45,1.0))
  