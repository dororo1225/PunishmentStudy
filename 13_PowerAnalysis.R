library(here)
library(brms)
library(cmdstanr)
library(tidybayes)
library(tidyverse)
options(mc.cores = parallel::detectCores())

# define function
inv_logit <- function(x){return(1/(1 + exp(-x)))}
logit <- function(x){return(log(x/(1-x)))}

# initiall model fitting to avoid recompiling the exact same Stan model in the later simulation 
prop_post <- 0.68
prop_pre <- 0.5
effect_test <- logit(prop_post) - logit(prop_pre)
sd_ri <- 1
sd_rs <- 1

tibble(subject = rep(LETTERS[1:24], each = 10),
       trial = rep(1:10, times = 24),
       trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
       ri = rep(rnorm(24, 0, sd_ri), each = 10),
       rs = rep(rnorm(24, 0, sd_rs), each = 10)) %>%
  mutate(z_pre = logit(prop_pre) + ri,
         z_post = logit(prop_pre) + ri + effect_test + rs) %>% 
  pivot_longer(starts_with("z_"), names_prefix = "z_", names_to = "test") %>% 
  mutate(q = inv_logit(value)) -> d_q
Y <- rbinom(nrow(d_q), size = 1, prob = d_q$q)
d_q %>% 
  mutate(test = fct_rev(test),
         Y = Y) -> df

fit <- brm(Y ~ test * trial_z + (test * trial_z |subject),
           data = df,
           family = "bernoulli",
           core = 4,
           iter = 2000,
           warmup = 1000,
           backend = "cmdstanr")

# simulation (it takes so long time)
prop_post <- 0.68 # theoretically expected value (see Margoni et al., 2018)   
prop_pre <- 0.5 # probability of selective looks at an aggressor in pretest
effect_test <- logit(prop_post) - logit(prop_pre) # theoretically expected effect size

set.seed(1234) # for reproducibility
df_rnd <- expand.grid(sd_ri = seq(0.1, 2, by = 0.1), # SD of random intercept
                      sd_rs = seq(0.1, 2, by = 0.1)) # SD of random slope for the effect of test type

r_max <- numeric()
lwr <- numeric()
upr <- numeric()

for (i in 1:nrow(df_rnd)){
  RepNum <- 100
  sd_ri <- df_rnd$sd_ri[i] # SD of random intercept
  sd_rs <- df_rnd$sd_rs[i] # SD of random slope for test type effect
  r_max_tmp <- numeric(RepNum)
  lwr_tmp <- numeric(RepNum)
  upr_tmp <- numeric(RepNum)
  for (j in 1:RepNum){
    print(sprintf("i = %3d, j = %3d", i, j))
    ## generate a sample
    tibble(subject = rep(LETTERS[1:24], each = 10),
           trial = rep(1:10, times = 24),
           trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
           ri = rep(rnorm(24, 0, sd_ri), each = 10),
           rs = rep(rnorm(24, 0, sd_rs), each = 10)) %>%
      mutate(z_pre = logit(prop_pre) + ri,
             z_post = logit(prop_pre) + ri + effect_test + rs) %>%
      pivot_longer(starts_with("z_"), names_prefix = "z_", names_to = "test") %>%
      mutate(q = inv_logit(value)) -> d_q
    Y <- rbinom(nrow(d_q), size = 1, prob = d_q$q)
    d_q %>%
      mutate(test = fct_rev(test),
             Y = Y) -> df
    
    ## model fitting
    fit <- update(fit, newdata = df)
    
    ## output max Rhat
    max(brms::rhat(fit), na.rm = TRUE) -> r_max_tmp[j]
    
    ## check 95% credible interval
    fit %>%
      spread_draws(b_testpost) %>%
      median_qi() -> d_par
    lwr_tmp[j] <- d_par$.lower
    upr_tmp[j] <- d_par$.upper
  }
  r_max <- c(r_max, r_max_tmp) # max Rhat
  lwr <- c(lwr, lwr_tmp) # lower 95% CI for test type effect
  upr <- c(upr, upr_tmp) # upper 95% CI for test type effect
}

tibble(sd_ri = rep(df_rnd$sd_ri, each = RepNum),
       sd_rs = rep(df_rnd$sd_rs, each = RepNum),
       Repeat = rep(1:RepNum, times = nrow(df_rnd))) %>%
  mutate(r_max = r_max,
         lwr = lwr,
         upr = upr,
         convergence = if_else(r_max < 1.1, 1, 0),
         Hit = if_else(lwr > 0 | upr < 0, 1, 0)) -> df_result_bernoulli

df_result_bernoulli %>%
  write_csv(here("Results", "SimulatedPower.csv"))
