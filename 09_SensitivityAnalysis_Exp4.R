library(here)
library(brms)
library(tidyverse)
options(mc.cores=parallel::detectCores())

# prior settings
sd_priors <- seq(0.05, 1.5, by = 0.05)

# read data
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp4") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

# sensitivity analysis 
set.seed(1234)
BF_int_incl <- numeric(length(sd_priors))
BF_test_incl <- numeric(length(sd_priors))
BF_trial_incl <- numeric(length(sd_priors))

for (i in 1:length(sd_priors)){
  target_sd <- sd_priors[i]
  target_prior <- str_c("cauchy(0, ", target_sd, ")")
  
  fit1 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
              data = df,
              family = bernoulli(link = "logit"),
              warmup = 1000,
              iter = 11000,
              prior = prior_string(target_prior, class = "b"),
              cores = 4,
              save_pars = save_pars(all = TRUE))
  
  fit2 <- update(fit1,
                 formula = value ~ test + trial_z + (test * trial_z | subject),
                 family = bernoulli(link = "logit"),
                 warmup = 1000,
                 iter = 11000,
                 prior = prior_string(target_prior, class = "b"),
                 cores = 4,
                 save_pars = save_pars(all = TRUE))
  
  fit3 <- update(fit1,
                 formula = value ~ trial_z + (test * trial_z | subject),
                 family = bernoulli(link = "logit"),
                 warmup = 1000,
                 iter = 11000,
                 prior = prior_string(target_prior, class = "b"),
                 cores = 4,
                 save_pars = save_pars(all = TRUE))
  
  fit4 <- update(fit1,
                 formula = value ~ test + (test * trial_z | subject),
                 family = bernoulli(link = "logit"),
                 warmup = 1000,
                 iter = 11000,
                 prior = prior_string(target_prior, class = "b"),
                 cores = 4,
                 save_pars = save_pars(all = TRUE))
  
  fit5 <- update(fit1,
                 formula = value ~ 1 + (test * trial_z | subject),
                 family = bernoulli(link = "logit"),
                 warmup = 1000,
                 iter = 11000,
                 prior = prior_string(target_prior, class = "b"),
                 cores = 4,
                 save_pars = save_pars(all = TRUE))
  
  ## check convergence
  r1 <- max(brms::rhat(fit1), na.rm = TRUE)
  r2 <- max(brms::rhat(fit2), na.rm = TRUE)
  r3 <-max(brms::rhat(fit3), na.rm = TRUE)
  r4 <-max(brms::rhat(fit4), na.rm = TRUE)
  r5 <-max(brms::rhat(fit5), na.rm = TRUE)
  r_diff <- c(r1, r2, r3, r4, r5) - 1.1
  
  if (all(r_diff < 0)){
    ## BF10
    BF1 <- bayes_factor(fit1, fit5)$bf
    BF2 <- bayes_factor(fit2, fit5)$bf
    BF3 <- bayes_factor(fit3, fit5)$bf
    BF4 <- bayes_factor(fit4, fit5)$bf
    BF5 <- bayes_factor(fit5, fit5)$bf
    
    ## posterior model probabilities
    p_M_givenData <- post_prob(fit5, fit4, fit3, fit2, fit1, prior_prob = rep(0.2, times = 5))
    
  } else{
    BF1 <- NA_real_
    BF2 <- NA_real_
    BF3 <- NA_real_
    BF4 <- NA_real_
    BF5 <- NA_real_
    p_M_givenData <- NA_real_
  }
  
  ## Model comaparison
  tibble(Models = c("Null Model", "test model", "trial model", "test + trial", "test * trial"),
         test = c(0, 1, 0, 1, 1),
         trial = c(0, 0, 1, 1, 1),
         interaction = c(0, 0, 0, 0, 1),
         p_M = rep(0.2, times = 5),
         p_M_givenData = p_M_givenData,
         BF_M = (p_M_givenData/(1 - p_M_givenData))/(p_M/(1-p_M)),
         F_10 = c(BF5, BF4, BF3, BF2, BF1)) -> df_model
  
  ## Inclusion Bayes factor
  df_model %>%
    pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
    group_by(Effects, incl) %>% 
    summarise(p_incl = sum(p_M),
              p_incl_givenData = sum(p_M_givenData),
              .groups = "drop") %>% 
    filter(incl == 1) %>% 
    select(!incl) %>% 
    mutate(BF_inclusion = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
           Effects = fct_relevel(Effects, "test", "trial")) %>% 
    arrange(Effects) -> df_incl
  
  df_incl %>% 
    filter(Effects == "test") %>% 
    pull(BF_inclusion) -> BF_test_incl[i]
  
  df_incl %>% 
    filter(Effects == "trial") %>% 
    pull(BF_inclusion) -> BF_trial_incl[i]
  
  df_incl %>% 
    filter(Effects == "interaction") %>% 
    pull(BF_inclusion) -> BF_int_incl[i]
}


df_SA <- tibble(scale = sd_priors,
                BF_int_incl = BF_int_incl,
                BF_test_incl = BF_test_incl,
                BF_trial_incl = BF_trial_incl) 

df_SA %>% 
  pivot_longer(starts_with("BF"), names_prefix = "BF_", names_to = "Effect", values_to = "BF_incl") %>% 
  mutate(Effect = case_when(Effect == "int_incl" ~ "interaction",
                            Effect == "test_incl" ~ "test",
                            Effect == "trial_incl" ~ "trial")) %>% 
  ggplot(aes(x = scale, y = BF_incl, color = Effect)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, lwd = 1) +
  labs(x = "Scaling factor of the Cauchy prior", y = "Bayes factor") +
  scale_color_viridis_d(option = "A", begin = 0.25, end = 0.7) +
  theme(axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12))

df_SA %>% 
  write_csv(here("Results", "SensitivityAnalysis_Exp4.csv"))