library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

data(foxes)
d <- as_tibble(foxes) %>% mutate(group = as.factor(group))

#visualize area vs body weight
d %>% ggplot() + geom_point(aes(area,weight))

#fit weight ~ area
m_wa <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b.area*area,
    a ~ dunif( 0 , 10 ) ,
    b.area ~ dnorm( 0 , 5 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d)
)
sampled_parameters_wa <- as_tibble(
  mvrnorm(n=1e4,
          mu=coef(m_wa),
          Sigma=vcov(m_wa))
)
area_seq <- seq_range(d$area, by=0.1, pretty=TRUE)
sampled_mu_wa <- sapply(area_seq, function (area)
  sampled_parameters_wa$a + sampled_parameters_wa$b.area*area)
bounds_wa <- tibble(
  area = area_seq,
  mu.lb = apply(sampled_mu_wa, 2, PI)[1,],
  mu.ub = apply(sampled_mu_wa, 2, PI)[2,],
)
d %>% ggplot() + 
  geom_point(aes(area, weight)) +
  geom_abline(aes(intercept=coef(m_wa)["a"] , slope=coef(m_wa)["b.area"])) +
  geom_ribbon(mapping=aes(x = area, ymin=mu.lb, ymax=mu.ub),
            alpha=0.5, data = bounds_wa)

#visualize group size vs body weight
d %>% ggplot() + geom_point(aes(groupsize, weight))

m_wgs <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b.gs*groupsize,
    a ~ dunif( 0 , 10 ) ,
    b.gs ~ dnorm( 0 , 5 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d)
)
sampled_parameters_wgs <- as_tibble(
  mvrnorm(n=1e4,
          mu=coef(m_wgs),
          Sigma=vcov(m_wgs))
)
gs_seq <- seq_range(d$groupsize, by=1, pretty=TRUE)
sampled_mu_wgs <- sapply(gs_seq, function (gs)
  sampled_parameters_wgs$a + sampled_parameters_wgs$b.gs*gs)
bounds_wgs <- tibble(
  groupsize = gs_seq,
  mu.lb = apply(sampled_mu_wgs, 2, PI)[1,],
  mu.ub = apply(sampled_mu_wgs, 2, PI)[2,],
)
d %>% ggplot() + 
  geom_point(aes(groupsize, weight)) +
  geom_abline(aes(intercept=coef(m_wgs)["a"] , slope=coef(m_wgs)["b.gs"])) +
  geom_ribbon(mapping=aes(x = groupsize, ymin=mu.lb, ymax=mu.ub),
              alpha=0.5, data = bounds_wgs)

#both variables seem to be weak predictors of weight


##multivariate model
mm <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b.area*area + b.gs*groupsize,
    a ~ dunif( 0 , 10 ) ,
    b.area ~ dnorm ( 0, 5 ),
    b.gs ~ dnorm( 0 , 5 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d)
)
#the two variables mask each other's effect;
#they correlate with each other, but one is positive with weight;
#the other is negative with weight
# Mean StdDev  5.5% 94.5%
#   a       4.45   0.37  3.86  5.04
# b.area  0.62   0.20  0.30  0.94
# b.gs   -0.43   0.12 -0.62 -0.24
# sigma   1.12   0.07  1.00  1.24
d %>% ggplot() + geom_point(aes(area, groupsize))

sampled_parameters_mm <- as_tibble(
  mvrnorm(n=1e4,
          mu=coef(mm),
          Sigma=vcov(mm))
)
seq <- tibble(
  area=seq_range(d$area, n=30),
  area.mean=mean(d$area),
  groupsize=seq_range(d$groupsize, n=30),
  groupsize.mean=mean(d$groupsize)
)

sampled_mu_area <- sapply(seq$area, function (area)
  sampled_parameters_mm$a + sampled_parameters_mm$b.area*area +
    sampled_parameters_mm$b.gs*seq$groupsize.mean[[1]])
sampled_mu_gs <- sapply(seq$groupsize, function (gs)
  sampled_parameters_mm$a + sampled_parameters_mm$b.area*seq$area.mean[[1]] +
    sampled_parameters_mm$b.gs*gs)
bounds <- tibble(
  area = seq$area,
  groupsize = seq$groupsize,
  mu_area.lb = apply(sampled_mu_area, 2, PI)[1,],
  mu_area.ub = apply(sampled_mu_area, 2, PI)[2,],
  mu_gs.lb = apply(sampled_mu_gs, 2, PI)[1,],
  mu_gs.ub = apply(sampled_mu_gs, 2, PI)[2,]
)
ggplot(d) +
  geom_abline(aes(intercept=coef(mm)["a"] + coef(mm)['b.gs']*seq$groupsize.mean[[1]], slope=coef(mm)["b.area"])) +  
  geom_ribbon(mapping=aes(x = area, ymin=mu_area.lb, ymax=mu_area.ub),
              alpha=0.5, data=bounds) +
  labs(y = 'weight')
ggplot(d) +
  geom_abline(aes(intercept=coef(mm)["a"] + coef(mm)['b.area']*seq$area.mean[[1]], slope=coef(mm)["b.gs"])) +  
  geom_ribbon(mapping=aes(x = groupsize, ymin=mu_gs.lb, ymax=mu_gs.ub),
              alpha=0.5, data=bounds) +
  labs(y = 'weight')

#5H3
m2 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b.avgfood*avgfood + b.gs*groupsize,
    a ~ dunif( 0 , 10 ) ,
    b.avgfood ~ dnorm ( 0, 5 ),
    b.gs ~ dnorm( 0 , 5 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d)
)
sampled_parameters_m2 <- as_tibble(
  mvrnorm(n=1e4,
          mu=coef(m2),
          Sigma=vcov(m2))
)
seq <- tibble(
  avgfood=seq_range(d$avgfood, n=30),
  avgfood.mean=mean(d$avgfood),
  groupsize=seq_range(d$groupsize, n=30),
  groupsize.mean=mean(d$groupsize)
)
sampled_mu_avgfood <- sapply(seq$avgfood, function (avgfood)
  sampled_parameters_m2$a + sampled_parameters_m2$b.avgfood*avgfood +
    sampled_parameters_m2$b.gs*seq$groupsize.mean[[1]])
sampled_mu_gs <- sapply(seq$groupsize, function (gs)
  sampled_parameters_m2$a + sampled_parameters_m2$b.avgfood*seq$avgfood.mean[[1]] +
    sampled_parameters_m2$b.gs*gs)
bounds <- tibble(
  avgfood = seq$avgfood,
  groupsize = seq$groupsize,
  mu_avgfood.lb = apply(sampled_mu_avgfood, 2, PI)[1,],
  mu_avgfood.ub = apply(sampled_mu_avgfood, 2, PI)[2,],
  mu_gs.lb = apply(sampled_mu_gs, 2, PI)[1,],
  mu_gs.ub = apply(sampled_mu_gs, 2, PI)[2,]
)
ggplot(d) +
  geom_abline(aes(intercept=coef(m2)["a"] + coef(m2)['b.gs']*seq$groupsize.mean[[1]], slope=coef(m2)["b.avgfood"])) +  
  geom_ribbon(mapping=aes(x = avgfood, ymin=mu_avgfood.lb, ymax=mu_avgfood.ub),
              alpha=0.5, data=bounds) +
  labs(y = 'weight')
ggplot(d) +
  geom_abline(aes(intercept=coef(m2)["a"] + coef(m2)['b.avgfood']*seq$avgfood.mean[[1]], slope=coef(m2)["b.gs"])) +  
  geom_ribbon(mapping=aes(x = groupsize, ymin=mu_gs.lb, ymax=mu_gs.ub),
              alpha=0.5, data=bounds) +
  labs(y = 'weight')


m3 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + 
      b.avgfood*avgfood + 
      b.gs*groupsize +
      b.area*area,
    a ~ dunif( 0 , 10 ) ,
    b.avgfood ~ dnorm ( 0, 5 ),
    b.gs ~ dnorm( 0 , 5 ) ,
    b.area ~ dnorm( 0 , 5 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d)
)
# Mean StdDev  5.5% 94.5%
#   a          4.09   0.42  3.42  4.77
# b.avgfood  2.32   1.39  0.09  4.55
# b.gs      -0.59   0.15 -0.84 -0.35
# b.area     0.40   0.24  0.02  0.78
# sigma      1.10   0.07  0.99  1.22

d %>% ggplot() + geom_point(aes(avgfood,area))
#avgfood and area are closely correlated with each other.
#avgfood and area are also closely correlated with weight