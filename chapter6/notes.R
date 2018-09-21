library(MASS)
library(tidyverse)
library(modelr)
library(rethinking)

xentropy <- function(p,q) -sum(p*log(q))
entropy <- function(p) xentropy(p,p)
divergence <- function(p,q) xentropy(p,q) - entropy(p)

rd <- c(0.3, 0.7)
md1 <- c(0.5, 0.5)
md2 <- c(0.1, 0.99)
entropy(rd) #0.6108643
divergence(rd, md1) #0.08228288

####################################################
df <- tibble(
  q1 = seq(0.01, 0.99, 0.01),
  q2 = 1 - q1,
  p1 = 0.3,
  p2 = 0.7,
  entropy_p = -( p1*log(p1) + p2*log(p2)),
  xentropy_pq = -( p1*log(q1) + p2*log(q2)),
  div_pq = xentropy_pq - entropy_p
)

df %>% ggplot() + 
  geom_line(aes(q1, div_pq)) +
  geom_vline(aes(xintercept = p1))
####################################################

data(cars)
#visualize
cars %>% ggplot() + geom_point(aes(speed,dist))

m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars 
)

cars %>% ggplot() + geom_point(aes(speed,dist)) +
  geom_abline(aes(intercept=coef(m)["a"] , slope=coef(m)["b"]))

n_samples <- 1000
sampled_parameters <- as_tibble(
  mvrnorm(n= n_samples,
          mu=coef(m),
          Sigma=vcov(m))
)

# 50 in cars$speed * 1000 samples
ll <- sapply(seq_len(n_samples), function(i) {
  mu <- sampled_parameters$a[i] + sampled_parameters$b[i]*cars$speed
  dnorm( cars$dist , mu , sampled_parameters$sigma[i] , log=TRUE )})
lppd <- sapply(seq_len(nrow(cars)), function(i) log_sum_exp(ll[i,]) - log(n_samples)
)
pWAIC <- sapply(seq_len(nrow(cars)), function(i) var(ll[i,])
)
#compute WAIC
-2*( sum(lppd) - sum(pWAIC) )

avD <- -2*rowMeans(ll)
posteriorMean <- coef(m)['a'] + coef(m)['b']*cars$speed
#what to use for sigma?
#D_at_mean <- dnorm (cars$dist, posteriorMean, )