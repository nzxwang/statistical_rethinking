library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

data(milk)
d <- as_tibble(milk)
dcc <- d %>% 
  filter(!is.na(neocortex.perc)) %>% 
  mutate(log.mass = log(mass))

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bn*neocortex.perc ,
    a ~ dnorm( 0 , 100 ) ,
    bn ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) ,
  data= as.data.frame(dcc) 
)
precis(m5.5, digits=5)
# Mean  StdDev     5.5%   94.5%
#   a     0.35334 0.47072 -0.39896 1.10564
# bn    0.00450 0.00694 -0.00659 0.01560
# sigma 0.16570 0.02841  0.12029 0.21111

m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bm*log.mass ,
    a ~ dnorm( 0 , 100 ) ,
    bm ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) ,
  data=as.data.frame(dcc) 
)
precis(m5.6, digits=4)
# Mean StdDev    5.5%  94.5%
#   a      0.7051 0.0487  0.6273 0.7830
# bm    -0.0317 0.0203 -0.0641 0.0007
# sigma  0.1569 0.0269  0.1139 0.1998

m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bn*neocortex.perc + bm*log.mass ,
    a ~ dnorm( 0 , 100 ) ,
    bn ~ dnorm( 0 , 1 ) ,
    bm ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) , data=as.data.frame(dcc) 
)
precis(m5.7, digits=4)
# Mean StdDev    5.5%   94.5%
#   a     -1.0844 0.4676 -1.8317 -0.3372
# bn     0.0279 0.0073  0.0163  0.0395
# bm    -0.0964 0.0225 -0.1322 -0.0605
# sigma  0.1148 0.0197  0.0833  0.1463
