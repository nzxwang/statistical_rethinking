library(tidyverse)
library(rethinking)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
as_tibble(samples) %>% ggplot() + geom_histogram(aes(value))

#3E1, 3E2, 3E3
posterior_below_0.2 <- sum(samples < 0.2)/10000
posterior_above_0.8 <- sum(samples>0.8)/10000
posterior_btwn_0.2_0.8 <- sum(samples>0.2 & samples<0.8)/10000

#3E4, 3E5
`20th_percentile` <- quantile(samples, 0.2)
`80th_percentile` <- quantile(samples, 0.8)

#3E6
rethinking::HPDI(samples, prob=0.66)

#3E7
quantile(samples, c(1/6,5/6))

#3M1
l<- 1000
prob <- tibble(
  p_grid = seq(from=0, to=1, length.out=l),
  prior = rep(1, l),
  likelihood = dbinom(8, size = 15, prob=p_grid),
  posterior = likelihood * prior / sum(likelihood * prior)
)
prob %>% ggplot() + geom_col(aes(x=p_grid, y=posterior))

#3M2
samples <- sample(prob$p_grid, prob = prob$posterior, size=1e4, replace=TRUE)
rethinking::HPDI(samples, prob=0.9)
as_tibble(samples) %>% ggplot() + geom_histogram(aes(x=value))

#3M3
ppc <- rbinom(n=samples, size=15, prob=samples)
as_tibble(ppc) %>% ggplot() + geom_histogram(aes(x=value), bins = 16)
P_8_in_15 <- mean(ppc==8)

#3M4
ppc <- rbinom(n=samples, size=9, prob=samples)
as_tibble(ppc) %>% ggplot() + geom_histogram(aes(x=value), bins = 10)
P_6_in_9 <- mean(ppc==6)

#3M5
l<- 1000
prob <- tibble(
  p_grid = seq(from=0, to=1, length.out=l),
  prior = ifelse( p_grid < 0.5 , 0 , 1 ),
  likelihood = dbinom(8, size = 15, prob=p_grid),
  posterior = likelihood * prior / sum(likelihood * prior)
)
prob %>% ggplot() + geom_col(aes(x=p_grid, y=posterior))

samples <- sample(prob$p_grid, size = 10000, prob = prob$posterior, replace=TRUE)
rethinking::HPDI(samples, prob=0.9)
as_tibble(samples) %>% ggplot() + geom_histogram(aes(x=value))

ppc <- rbinom(n=samples, size=15, prob=samples)
as_tibble(ppc) %>% ggplot() + geom_histogram(aes(x=value), binwidth=1)
P_8_in_15 <- mean(ppc==8)
#the ppc is quite robust to using a bad prior I suppose

#3H1
l=1000
data(homeworkch3)
mean(c(birth1,birth2))
hard <- tibble(
  p_grid = seq(from=0, to=1, length.out=l),
  prior = rep(1, l),
  likelihood = dbinom(sum(c(birth1,birth2)), 
                      size = length(c(birth1,birth2)), 
                      prob=p_grid),
  posterior = prior * likelihood / sum(prior*likelihood)
)
hard %>% ggplot() + geom_col(aes(x=p_grid, y=posterior))
#the parameter that maximizes the posterior is close to the mean of the data
hard$p_grid[[which.max(hard$posterior)]]

#3H2
samples <- sample(hard$p_grid, prob = hard$posterior, size =1e4, replace = TRUE)
as_tibble(samples) %>% ggplot() + geom_histogram(aes(x=value), binwidth=0.01) +
  scale_x_continuous(limits=c(0,1))
rethinking::HPDI(samples, prob=0.5)
rethinking::HPDI(samples, prob=0.89)
rethinking::HPDI(samples, prob=0.97)

#3H3
ppc <- rbinom(n=samples, size=200, prob=samples)
as_tibble(ppc) %>% ggplot() + geom_histogram(aes(x=value), binwidth=1) +
  scale_x_continuous(limits=c(0,200)) + 
  geom_vline(xintercept=sum(c(birth1,birth2)))

#3H4
ppc <- rbinom(n=samples, size=100, prob=samples)
as_tibble(ppc) %>% ggplot() + geom_histogram(aes(x=value), binwidth=1) +
  scale_x_continuous(limits=c(0,100)) + geom_vline(xintercept=sum(birth1))
#our model predicts more boys when combining birth1 and brith2

#3H5
n_first_born_girls <-sum(birth1==0)
second_birth_following_female <- birth2[birth1==0]
ppc <- rbinom(n=samples, size=n_first_born_girls, prob=samples)
as_tibble(ppc) %>% ggplot() + geom_histogram(aes(x=value), binwidth=1) +
  scale_x_continuous(limits=c(0,n_first_born_girls)) + 
  geom_vline(xintercept=sum(second_birth_following_female))
#model predicts less boys in birth2 than actual amount following female in birth1
