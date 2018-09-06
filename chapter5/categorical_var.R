library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

data(milk)
d <- as_tibble(milk) %>% mutate(
  clade.NWM = ifelse(clade=="New World Monkey", 1, 0),
  clade.OWM = ifelse(clade=="Old World Monkey", 1, 0),
  clade.S = ifelse(clade=="Strepsirrhine", 1, 0)
)
m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S ,
    a ~ dnorm( 0.6 , 10 ) ,
    b.NWM ~ dnorm( 0 , 1 ) ,
    b.OWM ~ dnorm( 0 , 1 ) ,
    b.S ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=as.data.frame(d) )
precis(m5.16)
# Mean StdDev  5.5% 94.5%
#   a      0.55   0.04  0.49  0.61
# b.NWM  0.17   0.05  0.08  0.25
# b.OWM  0.24   0.06  0.15  0.34
# b.S   -0.04   0.06 -0.14  0.06
# sigma  0.11   0.02  0.09  0.14

sampled_parameters_m5.16 <- as_tibble(
  mvrnorm(n=1e4,
          mu=coef(m5.16),
          Sigma=vcov(m5.16))
) %>% mutate(
  mu.ape =a,
  mu.NWM = a + b.NWM,
  mu.OWM = a + b.OWM,
  mu.S = a + b.S
)
sampled_parameters_m5.16 %>% 
  select(starts_with("mu")) %>% 
  as.data.frame() %>% 
  precis()
#        Mean StdDev |0.89 0.89|
# mu.ape 0.55   0.04  0.49  0.61
# mu.NWM 0.71   0.04  0.65  0.77
# mu.OWM 0.79   0.05  0.71  0.86
# mu.S   0.51   0.05  0.43  0.59