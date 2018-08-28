library(tidyverse)
library(modelr)
library(rethinking)

data(Howell1)
d <- as_tibble(Howell1)
d2 <- d %>% filter(age >=18)


data<-tibble(
  mu = seq( from=140, to=160 , length.out=200 ),
  sigma= seq( from=4 , to=9 , length.out=200 )
) %>% 
  data_grid(mu, sigma) %>%
  mutate(log_likelihood_mutated = sum(dnorm(d2$height, 
                                    mean = mu, 
                                    sd= sigma,
                                    log= TRUE)))

data$log_likelihood <- sapply(X=seq_along(data$mu),
                              FUN= function (i) sum(dnorm(d2$height,
                                                          mean=data$mu[i],
                                                          sd=data$sigma[i],
                                                          log=TRUE))
                              )
data <- data %>% mutate(log_prior = dnorm(mu, mean = 178, sd = 20, log=TRUE) + 
                            dunif(x=sigma, min = 0, max = 50, log=TRUE),
                log_posterior_proportional = log_likelihood + log_prior)
data <- data %>% mutate(relative_posterior_proportional = 
                  exp(log_posterior_proportional - 
                        max(data$log_posterior_proportional)))

data %>% ggplot() + geom_contour(aes(x=mu, y=sigma, z=relative_posterior_proportional))
contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )
image_xyz( data$mu , data$sigma , data$log_posterior_proportional )
