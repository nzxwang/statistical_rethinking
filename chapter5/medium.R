library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

N<-1e4
#5M1
d <- tibble(
  x.real = rnorm(N, mean=1, sd=1),
  x.spurious = rnorm(N, mean=x.real, sd=0.03),
  y = rnorm(N, mean=x.real, sd=1)
)
m <- map(alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b.real*x.real + b.spurious*x.spurious,
  a ~ dnorm(0,10),
  sigma ~ dunif(0,10),
  b.real ~ dnorm(0,10),
  b.spurious ~ dnorm(0,10)),
  data = as.data.frame(d)
)
sampled_params <- as_tibble(
  mvrnorm()
)


