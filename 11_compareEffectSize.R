library(here)
library(brms)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(knitr)
library(bayestestR)
library(broom.mixed)
options(mc.cores=parallel::detectCores())

# read data
read_csv("data.csv", col_types = "cccdd") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest"))) -> df 

# compare effect size of test type
## Model fitting
fit <- brm(value ~  test * Exp + trial_z + (test + trial_z| subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)
mcmc_rhat(rhat(fit))

### Posterior distribution
tidy(fit, robust = TRUE, conf.method = "quantile") 

### Probability of direction
pd(fit, method = "direct", null = 0)

### Odds ratio
fit %>% 
  gather_draws(b_Intercept, b_testPosttest, b_ExpExp2, b_ExpExp3, b_ExpExp4, b_ExpExp5, b_trial_z,
               `b_testPosttest:ExpExp2`, `b_testPosttest:ExpExp3`, `b_testPosttest:ExpExp4`, `b_testPosttest:ExpExp5`) %>%
  ungroup() %>% 
  mutate(.variable = fct_relevel(.variable, "b_Intercept", "b_testPosttest", "b_ExpExp2", "b_ExpExp3", "b_ExpExp4", "b_ExpExp5", "b_trial_z",
                                 "b_testPosttest:ExpExp2", "b_testPosttest:ExpExp3", "b_testPosttest:ExpExp4", "b_testPosttest:ExpExp5"),
         OR = exp(.value)) %>%
  group_by(.variable) %>% 
  select(-.value) %>% 
  median_qi() 

### ESS
effective_sample(fit, effects = "all",
                 parameters = c("b_Intercept", "b_testPosttest", "b_ExpExp2", "b_ExpExp3", "b_ExpExp4",  "b_ExpExp5", "b_trial_z",
                                "b_testPosttest:ExpExp2", "b_testPosttest:ExpExp3", "b_testPosttest:ExpExp4", "b_testPosttest:ExpExp5",
                                "sd_subject__Intercept", "sd_subject__testPosttest", "sd_subject__trial_z"))

## Effect size comparison
fit %>% 
  gather_draws(`b_testPosttest:ExpExp2`, `b_testPosttest:ExpExp3`, `b_testPosttest:ExpExp4`, `b_testPosttest:ExpExp5`) %>% 
  mutate(.value = - .value,
         .variable = str_c("Exp.1 - ", str_replace(.variable, pattern = "b_testPosttest:ExpExp", replacement = "Exp."))) %>%
  ungroup() -> diff_1

fit %>%
  gather_draws(`b_testPosttest:ExpExp2`, `b_testPosttest:ExpExp3`, `b_testPosttest:ExpExp4`, `b_testPosttest:ExpExp5`) %>%
  mutate(.variable = str_replace(.variable, pattern = "b_testPosttest:ExpExp", replacement = "Exp.")) %>%
  compare_levels(.value, by = .variable) %>%
  ungroup() -> diff_others

bind_rows(diff_1, diff_others) %>%
  mutate(.variable = fct_rev(fct_relevel(.variable,
                                         "Exp.1 - Exp.2", "Exp.1 - Exp.3", "Exp.1 - Exp.4", "Exp.1 - Exp.5",
                                         "Exp.5 - Exp.2", "Exp.5 - Exp.3", "Exp.5 - Exp.4",
                                         "Exp.4 - Exp.2", "Exp.4 - Exp.3", "Exp.3 - Exp.2"))) %>%
  rename(Diff_ef_test = ".variable") -> df_diff

df_diff %>%
  group_by(Diff_ef_test) %>%
  summarise_draws() %>%
  arrange(desc(Diff_ef_test)) 

df_diff %>%
  group_by(Diff_ef_test) %>%
  median_qi() %>%
  arrange(desc(Diff_ef_test)) 

df_diff %>% 
  write_csv(here("Results", "EffectSizeComparison.csv"))

## Visualization (Supplementary Figure 1)
ggplot(df_diff, aes(x = .value, y = Diff_ef_test)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference of effect of test type", y = "Comparison") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        # axis.title.y = element_text(vjust = 1.5),
        axis.text = element_text(color = "black", size = 15),
        strip.text = element_text(face = "bold", size = 18)) -> gp
print(gp)