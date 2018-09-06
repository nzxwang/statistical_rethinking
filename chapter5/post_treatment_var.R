library(MASS)
library(tidyverse)
library(modelr)``
library(rethinking)

# number of plants
N <- 100

d <- tibble(
  h0 = rnorm(N,10,2),
  treatment = rep(0:1, each=N/2),
  fungus = rbinom(N, size=1, prob = 0.5 - treatment*0.4),
  h1 = h0 + rnorm(N, 5-3*fungus)
)

m5.13 <- map(
  alist(
    h1 ~ dnorm(mu,sigma),
    mu <- a + bh*h0 + bt*treatment + bf*fungus,
    a ~ dnorm(0,100),
    c(bh,bt,bf) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=as.data.frame(d) )
precis(m5.13)
# Mean StdDev  5.5% 94.5%
#   a      5.55   0.49  4.77  6.33
# bh     0.95   0.05  0.87  1.02
# bt     0.19   0.18 -0.10  0.49
# bf    -3.23   0.21 -3.57 -2.90
# sigma  0.83   0.06  0.74  0.93

# Mean StdDev  5.5% 94.5%
#   a      4.79   0.55  3.90  5.68
# bh     1.03   0.05  0.95  1.12
# bt    -0.33   0.26 -0.74  0.09
# bf    -3.48   0.28 -3.93 -3.02
# sigma  1.03   0.07  0.91  1.14

m5.14 <- map(
  alist(
    h1 ~ dnorm(mu,sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0,100),
    c(bh,bt) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=as.data.frame(d) )
precis(m5.14)
# Mean StdDev 5.5% 94.5%
#   a     3.57   0.86 2.20  4.95
# bh    0.95   0.08 0.82  1.09
# bt    1.62   0.32 1.11  2.14
# sigma 1.62   0.11 1.44  1.80