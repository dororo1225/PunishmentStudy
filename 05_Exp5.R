library(here)
library(brms)
library(cmdstanr)
library(tidyverse)
library(tidybayes)
library(bayestestR)
library(effectsize)
library(broom.mixed)
options(mc.cores = parallel::detectCores()) 

# read data
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp5") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

# Bayes Factor Analysis
## Model Comparison (Comparison with the null model)
fit1 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = df,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            prior = prior(cauchy(0, 1/sqrt(2)), class = b),
            cores = 4,
            save_pars = save_pars(all = TRUE),
            seed = 1234)

fit2 <- update(fit1,
               formula = value ~ test + trial_z + (test * trial_z | subject),
               family = bernoulli(link = "logit"),
               warmup = 1000,
               iter = 11000,
               prior = prior(cauchy(0, 1/sqrt(2)), class = b),
               cores = 4,
               save_pars = save_pars(all = TRUE),
               seed = 1234)

fit3 <- update(fit1,
               formula = value ~ trial_z + (test * trial_z | subject),
               family = bernoulli(link = "logit"),
               warmup = 1000,
               iter = 11000,
               prior = prior(cauchy(0, 1/sqrt(2)), class = b),
               cores = 4,
               save_pars = save_pars(all = TRUE),
               seed = 1234)

fit4 <- update(fit1,
               formula = value ~ test + (test * trial_z | subject),
               family = bernoulli(link = "logit"),
               warmup = 1000,
               iter = 11000,
               prior = prior(cauchy(0, 1/sqrt(2)), class = b),
               cores = 4,
               save_pars = save_pars(all = TRUE),
               seed = 1234)

fit5 <- update(fit1,
               formula = value ~ 1 + (test * trial_z | subject),
               family = bernoulli(link = "logit"),
               warmup = 1000,
               iter = 11000,
               prior = prior(cauchy(0, 1/sqrt(2)), class = b),
               cores = 4,
               save_pars = save_pars(all = TRUE),
               seed = 1234)

### check convergence
r1 <- max(brms::rhat(fit1), na.rm = TRUE)
r2 <- max(brms::rhat(fit2), na.rm = TRUE)
r3 <-max(brms::rhat(fit3), na.rm = TRUE)
r4 <-max(brms::rhat(fit4), na.rm = TRUE)
r5 <-max(brms::rhat(fit5), na.rm = TRUE)
r_diff <- c(r1, r2, r3, r4, r5) - 1.1
all(r_diff < 0)

### BF10
set.seed(1234) # for reproducibility
BF1 <- bayes_factor(fit1, fit5)
BF2 <- bayes_factor(fit2, fit5)
BF3 <- bayes_factor(fit3, fit5)
BF4 <- bayes_factor(fit4, fit5)
BF5 <- bayes_factor(fit5, fit5)

### posterior model probabilities
p_M_givenData <- post_prob(fit5, fit4, fit3, fit2, fit1, prior_prob = rep(0.2, times = 5))

### Model comaparison
tibble(Models = c("Null Model", "test model", "trial model", "test + trial", "test * trial"),
       test = c(0, 1, 0, 1, 1),
       trial = c(0, 0, 1, 1, 1),
       interaction = c(0, 0, 0, 0, 1),
       p_M = rep(0.2, times = 5),
       p_M_givenData = p_M_givenData,
       BF_M = (p_M_givenData/(1 - p_M_givenData))/(p_M/(1-p_M)),
       BF_10 = c(BF5$bf, BF4$bf, BF3$bf, BF2$bf, BF1$bf),
       interpret = interpret_bf(BF_10)) -> df_model

df_model %>% 
  select(c(1, 5:9)) 

df_model %>% 
  write_csv(here("Results", "ModelComparison_Exp5.csv"))

## Analysis of Effects
### Inclusion Bayes factors
df_model %>% 
  pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
  group_by(Effects, incl) %>% 
  summarise(p_incl = sum(p_M),
            p_incl_givenData = sum(p_M_givenData),
            .groups = "drop") %>% 
  filter(incl == 1) %>% 
  select(!incl) %>% 
  mutate(BF_inclusion = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
         interpret = interpret_bf(BF_inclusion),
         Effects = fct_relevel(Effects, "test", "trial")) %>% 
  arrange(Effects)

# Parameter Estimation
## fit the best model to the data 
fit <- brm(formula = value ~ test + (test * trial_z | subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)

### Posterior distribution of the model parameters
#### Posterior distribution
tidy(fit, robust = TRUE, conf.method = "quantile")

#### Probability of direction
pd(fit, method = "direct", null = 0)

#### Odds ratio
fit %>% 
  gather_draws(b_Intercept, b_testPosttest) %>%
  ungroup() %>% 
  mutate(.variable = fct_relevel(.variable, "b_Intercept", "b_testPosttest"),
         OR = exp(.value)) %>%
  group_by(.variable) %>% 
  select(-.value) %>% 
  median_qi() 

#### ESS
effective_sample(fit, effects = "all",
                 parameters = c("b_Intercept", "b_testPosttest",
                                "sd_subject__Intercept", "sd_subject__testPosttest", "sd_subject__trial_z", "sd_subject__testPosttest:trial_z"))

### check posterior predictive distribution
df %>% 
  mutate(id = str_pad(as.numeric(as.factor(subject)), width = 2, side = "left", pad = "0")) %>% 
  group_by(id, test) %>% 
  summarise(N_Look = sum(value),
            .groups = "drop") -> df_summary1

df %>% 
  select(subject, test, trial, trial_z, value) %>% 
  add_predicted_draws(fit, re_formula = ~ (test * trial_z |subject), seed = 1234) %>% 
  ungroup() %>% 
  mutate(id = str_pad(as.numeric(as.factor(subject)), width = 2, side = "left", pad = "0")) %>% 
  group_by(id, test, .draw) %>% 
  summarise(N_Look_pred = sum(.prediction),
            .groups = "drop_last") -> df_prediction

df_prediction %>% 
  median_qi(.width = 0.95) %>% 
  left_join(df_summary1, by = c("id", "test")) %>% 
  mutate(checkPPD = if_else(N_Look >= .lower & N_Look <= .upper, 1, 0)) %>% 
  summarise(N = n(),
            Within95 = sum(checkPPD),
            Prop = Within95/N)

#### visualize posterior prediction intervals (Supplementary Figure 2)
df_prediction %>% 
  ungroup() %>% 
  ggplot(aes(x = id, y = N_Look_pred)) +
  stat_interval(.width = c(.50, .80, .95)) +
  geom_point(data = df_summary1, aes(y = N_Look), size = 2) +
  facet_grid(test~.) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_color_brewer() +
  labs(x = "Participant ID", y = "Number of trials with\nselective look at an aggressor",
       color = "Posterior\nprediction\ninterval") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12))

## fit the full model to the data 
fit_full <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
                data = df,
                family = bernoulli(link = "logit"),
                warmup = 1000,
                iter = 11000,
                cores = 4,
                backend = "cmdstanr",
                refresh = 0,
                seed = 1234)

### Posterior distribution of the model parameters
#### Posterior distribution
tidy(fit_full, robust = TRUE, conf.method = "quantile") 

#### Probability of direction
pd(fit_full, method = "direct", null = 0)

#### Odds ratio
fit_full %>% 
  gather_draws(b_Intercept, b_testPosttest, b_trial_z, `b_testPosttest:trial_z`) %>%
  ungroup() %>% 
  mutate(.variable = fct_relevel(.variable, "b_Intercept", "b_testPosttest", "b_trial_z", "b_testPosttest:trial_z"),
         OR = exp(.value)) %>%
  group_by(.variable) %>% 
  select(-.value) %>% 
  median_qi() 

#### ESS
effective_sample(fit_full, effects = "all",
                 parameters = c("b_Intercept", "b_testPosttest", "b_trial_z", "b_testPosttest:trial_z",
                                "sd_subject__Intercept", "sd_subject__testPosttest", "sd_subject__trial_z", "sd_subject__testPosttest:trial_z"))

### Visualization (Figure 3)
df %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            Prop = sum(value)/N,
            .groups = "drop") -> df_summary2

df %>% 
  select(test, trial, trial_z) %>% 
  distinct() %>% 
  add_epred_draws(fit_full, re_formula = NA) %>%
  median_qi() %>% 
  ungroup() -> df_MED

df_MED %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower, fill = test), alpha = 0.2) +
  geom_line(aes(y = .epred, color = test), lwd = 1.5) +
  geom_point(data = df_summary2, aes(y = Prop, color = test), size = 3) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_x_continuous(breaks = 1:10)+
  ylim(0, 1) +
  theme_classic() +
  labs(x = "Trial number", y = "Proportion of selective look\nat an aggressor", color = "Test type", fill = "Test type", tag = "e",
       title = "Replication of Exp.1") +
  guides(color = "none", fill = "none") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 21, face = "bold"),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18))