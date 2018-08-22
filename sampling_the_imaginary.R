library(tidyverse)
library(rethinking)

PrP_g_V <- 0.95
PrP_g_M <- 0.01
PrV <- 0.001
#Probability of Vampire given Positive test
PrV_g_P <- (PrP_g_V * PrV) / (PrP_g_V * PrV + PrP_g_M * (1-PrV))
#This is because FalseP much bigger than TrueP
FalseP <- PrP_g_M * (1-PrV)
TrueP <- PrP_g_V * PrV

###3.1
globe_toss <- tibble(p_grid = seq( from=0 , to=1 , length.out=1000 ),
                     prior = rep( 1 , length(p_grid) ),
                     likelihood = dbinom( 6 , size=9 , prob=p_grid ),
                     posterior = likelihood * prior / sum(likelihood * prior))
globe_toss %>% ggplot() + geom_col(aes(p_grid, posterior))

#sample the posterior probability
samples <- sample(globe_toss$p_grid,
                   size=1000,
                   replace=TRUE,
                   prob=globe_toss$posterior)
as_tibble(samples) %>% ggplot() + geom_histogram(aes(value))

# 3.2.1 Interval of defined boundary
# add up posterior probability where p < 0.5
sum( globe_toss$posterior[ globe_toss$p_grid < 0.5 ] )
sum( samples<0.5) /1000


# 3.2.2 Interval of defined mass
quantile(samples, 0.8)
quantile(samples, c(0.1,0.9))
quantile(samples, c(0.25,0.75))
HPDI(samples, prob=0.5)

globe_toss_skewed <- tibble(
  p_grid = seq( from=0 , to=1 , length.out=1000 ),
  prior = rep(1,1000),
  likelihood = dbinom( 3 , size=3 , prob=p_grid ),
  posterior = likelihood * prior / sum(likelihood * prior))
samples_skewed = sample( globe_toss_skewed$p_grid,
                         size=1e4,
                         replace=TRUE,
                         prob=globe_toss_skewed$posterior )
as_tibble(samples_skewed) %>% ggplot() + geom_histogram(aes(value))

#percentile interval using the package rethinking
PI( samples_skewed , prob=0.5 ) #quantile(samples, c(0.25,0.75))
#highest posterior density interval
HPDI( samples, prob=0.5 )
HPDI( samples_skewed , prob=0.5 )

#maximum a posteriori
globe_toss$p_grid[ which.max(globe_toss$posterior) ]
globe_toss_skewed$p_grid[ which.max(globe_toss_skewed$posterior) ]
chainmode( samples_skewed , adj=0.01 )

mean(samples_skewed)
median(samples_skewed)

#loss function is just the absolute distance hence we optimize for median.
guess <- 0.5
sum( globe_toss_skewed$posterior*abs( guess - globe_toss_skewed$p_grid))
loss <- sapply(globe_toss_skewed$p_grid, 
              function(d) sum(globe_toss_skewed$posterior * 
                                abs(d - globe_toss_skewed$p_grid) ))
globe_toss_skewed$p_grid[ which.min(loss) ]
