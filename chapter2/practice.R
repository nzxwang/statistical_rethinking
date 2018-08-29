library(tidyverse)

#2M1
tibble(p_grid=seq(0, 1, length.out=20),
       prior=rep(1,length(p_grid)),
       distrib_1=dbinom(3, size=3, prob=p_grid),
       distrib_2=dbinom(3, size=4, prob=p_grid),
       distrib_3=dbinom(5, size=7, prob=p_grid)) %>% 
  gather(key="data", value="likelihood", -p_grid, -prior) %>%
  mutate(posterior=likelihood*prior/sum(likelihood*prior)) %>% 
  ggplot() + geom_col(aes(p_grid,posterior)) + facet_wrap(~data)

#2M2
tibble(p_grid=seq(0, 1, length.out=20),
       prior=ifelse( p_grid < 0.5 , 0 , 1 ),
       distrib_1=dbinom(3, size=3, prob=p_grid),
       distrib_2=dbinom(3, size=4, prob=p_grid),
       distrib_3=dbinom(5, size=7, prob=p_grid)) %>% 
  gather(key="data", value="likelihood", -p_grid, -prior) %>%
  mutate(posterior=likelihood*prior/sum(likelihood*prior)) %>% 
  ggplot() + geom_col(aes(p_grid,posterior)) + facet_wrap(~data)

#2M4
M4 <- tibble(
  likelihood = c(WW=0, BW=1, BB=2),
  prior = rep(x=1, length=length(likelihood)),
  posterior = likelihood*prior/sum(likelihood*prior)
)

#2M5
M5 <- tibble(
  likelihood = c(WW=0, BW=1, BB=4),
  prior = rep(x=1,length=length(likelihood)),
  posterior = likelihood*prior/sum(likelihood*prior)
)

#2M6
M6 <- tibble(
  likelihood = c(WW=0, BW=1, BB=2),
  prior = c(WW=3, BW=2, BB=1),
  posterior = likelihood*prior/sum(likelihood*prior)
)

#2M7
M7 <- tibble(
  likelihood = c(WW=0, BW=1, BB=3),
  prior = rep(x=1,length=length(likelihood)),
  posterior = likelihood*prior/sum(likelihood*prior)
)

#2H2
H2 <- tibble(
  species = c("A","B"),
  likelihood = c(T_given_A=0.1, T_given_B=0.2),
  prior = rep(x=1,length(likelihood)),
  posterior = likelihood*prior/sum(likelihood*prior)
)
#2H1
P_TT <- sum(H1$posterior*H1$likelihood)

#2H3
H3 <- tibble(
  species = c("A","B"),
  likelihood = c(TS_given_A=0.1*0.9,
                 TS_given_B=0.2*0.8),
  prior = rep(x=1,length(likelihood)),
  posterior = likelihood*prior/sum(likelihood*prior)
)
P_A_given_TS <- H3$posterior[[1]]

#2H4
no_birth_info <- tibble(
  species = c("A","B"),
  likelihood = c(A_given_tA=0.8, B_given_tA=1-0.65),
  prior = rep(1,length(likelihood)),
  posterior = likelihood*prior/sum(likelihood*prior)
)

first_birth_info <- tibble(
  species = c("A","B"),
  likelihood = H2$likelihood,
  prior = no_birth_info$posterior,
  posterior = likelihood*prior/sum(likelihood*prior)
)

second_birth_info <- tibble(
  species = c("A","B"),
  prior = first_birth_info$posterior,
  likelihood = c(S_given_A=0.9, S_given_B=0.8),
  posterior = likelihood*prior/sum(likelihood*prior)
)
