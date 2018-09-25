library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

data(rugged)
d <- rugged %>% 
  as_tibble() %>% 
  mutate(log_gdp = log(rgdppc_2000)) %>%
  filter(!is.na(rgdppc_2000)) %>%
  select(cont_africa, rugged, log_gdp, everything())
d.A1 <- d %>% filter(cont_africa==1)
d.A0 <- d %>% filter(cont_africa==0)

m7.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = as.data.frame(d.A1)
)

m7.2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = as.data.frame(d.A0)
)

# inside africa, rugged terrain pos corr with gdp
d.A1 %>% ggplot() + 
  geom_point(aes(rugged, log_gdp)) +
  geom_abline(aes(intercept=coef(m7.1)["a"] , slope=coef(m7.1)["bR"]))
# outside africa, rugged terrain neg corr with gdp
d.A0 %>% ggplot() + 
  geom_point(aes(rugged, log_gdp)) +
  geom_abline(aes(intercept=coef(m7.2)["a"] , slope=coef(m7.2)["bR"]))
# splitting the data into two dfs is a horrible way to do this.

m7.3 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d) )
m7.4 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d) )

compare( m7.3 , m7.4 )
# m7.4 gets weight=1!
sampled_parameters_m7.4 <- as_tibble(
  mvrnorm(n=1e4, 
          mu=coef(m7.4), 
          Sigma=vcov(m7.4)))
rugged_seq <- seq_range(d$rugged, by=0.25, expand=0.3, pretty=TRUE)
sampled_mu_0 <- sapply(rugged_seq, function (rugged)
  sampled_parameters_m7.4$a + sampled_parameters_m7.4$bR*rugged)
sampled_mu_1 <- sapply(rugged_seq, function (rugged)
  sampled_parameters_m7.4$a + sampled_parameters_m7.4$bR*rugged + sampled_parameters_m7.4$bA)

plot_m7.4 <- tibble(  
  rugged = rugged_seq,
  mu0 = apply(sampled_mu_0, 2, mean),
  mu0_lb = apply( sampled_mu_0 , 2 , PI , prob=0.97 )[1,],
  mu0_ub = apply( sampled_mu_0 , 2 , PI , prob=0.97 )[2,],
  mu1 = apply(sampled_mu_1, 2, mean),
  mu1_lb = apply( sampled_mu_1 , 2 , PI , prob=0.97 )[1,],
  mu1_ub = apply( sampled_mu_1 , 2 , PI , prob=0.97 )[2,])

plot_m7.4 %>% 
  ggplot() + 
  geom_point(data=d.A0, aes(x=rugged, y=log_gdp), color="red") +
  geom_point(data=d.A1, aes(x=rugged, y=log_gdp), color="blue") +
  geom_abline(aes(slope=coef(m7.4)["bR"], intercept=coef(m7.4)["a"]), color='red') +
  geom_ribbon(aes(x = rugged, ymin=mu0_lb, ymax=mu0_ub), fill='red', alpha=0.2) +
  geom_abline(aes(slope=coef(m7.4)["bR"], intercept=coef(m7.4)["a"] + coef(m7.4)['bA']), color='blue') +
  geom_ribbon(aes(x = rugged, ymin=mu1_lb, ymax=mu1_ub), fill='blue', alpha=0.2)

m7.5 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bAR*cont_africa*rugged + bA*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data= as.data.frame(d) )
compare( m7.3 , m7.4, m7.5 )
#m7.5 gets weight=0.96

#stop doing this long process. just use link
sampled_parameters_m7.5 <- mvrnorm(n=1e4, mu = coef(m7.5), Sigma = vcov(m7.5)) %>% as_tibble()

mu.Africa = link( m7.5 , data=data.frame(cont_africa=1,rugged=rugged_seq) )
mu.NotAfrica = link( m7.5 , data=data.frame(cont_africa=0,rugged=rugged_seq))
bounds_m7.5 <- tibble(
  rugged = rugged_seq,
  mu.Africa.mean = apply( mu.Africa , 2 , mean ),
  mu.Africa.lb = apply( mu.Africa , 2 , PI , prob=0.97 )[1,],
  mu.Africa.ub = apply( mu.Africa , 2 , PI , prob=0.97 )[2,],
  mu.NotAfrica.mean = apply( mu.NotAfrica , 2 , mean ),
  mu.NotAfrica.lb = apply( mu.NotAfrica , 2 , PI , prob=0.97 )[1,],
  mu.NotAfrica.ub = apply( mu.NotAfrica , 2 , PI , prob=0.97 )[2,]
)
bounds_m7.5 %>% 
  ggplot() + 
  geom_point(data=d.A0, aes(x=rugged, y=log_gdp), color="red") +
  geom_point(data=d.A1, aes(x=rugged, y=log_gdp), color="blue") +
  geom_abline(aes(slope=coef(m7.5)["bR"], intercept=coef(m7.5)["a"]), color='red') +
  geom_ribbon(aes(x = rugged, ymin=mu.NotAfrica.lb, ymax=mu.NotAfrica.ub), fill='red', alpha=0.2) +
  geom_abline(aes(slope=coef(m7.5)["bR"]+coef(m7.5)["bAR"], intercept=coef(m7.5)["a"] + coef(m7.5)['bA']), color='blue') +
  geom_ribbon(aes(x = rugged, ymin=mu.Africa.lb, ymax=mu.Africa.ub), fill='blue', alpha=0.2)

#lets look at the distributions of the slopes inside and outside africa
sampled_parameters_m7.5 <- sampled_parameters_m7.5 %>%
  mutate(gamma.Africa = bR + bAR,
         gamma.NotAfrica = bR,
         diff = gamma.Africa - gamma.NotAfrica) 
sampled_parameters_m7.5 %>% 
  ggplot() +
  geom_histogram(aes(gamma.Africa), fill='blue', alpha=0.5) +
  geom_histogram(aes(gamma.NotAfrica), fill='red', alpha=0.5)
  
#probability that slope within Africa is less than slope outside Africa?
sum(sampled_parameters_m7.5$diff<0) / length(sampled_parameters_m7.5$diff)
