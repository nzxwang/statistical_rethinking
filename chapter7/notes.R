library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

data(rugged)
d <- rugged %>% 
  as_tibble() %>% 
  mutate(log_gdp = log(rgdppc_2000)) %>%
  filter(!is.na(rgdppc_2000)) 
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