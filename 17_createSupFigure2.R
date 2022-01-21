library(tidyverse)
library(here)
library(brms)
library(cmdstanr)
library(tidybayes)
library(ggpubr)

# Exp.1
## model fitting
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp1") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

fit <- brm(formula = value ~ test + (test * trial_z | subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)

## posterior prediction distriburuion
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
  ungroup() %>% 
  ggplot(aes(x = id, y = N_Look_pred)) +
  stat_interval(.width = c(.50, .80, .95)) +
  geom_point(data = df_summary1, aes(y = N_Look), size = 2) +
  facet_grid(test~.) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_color_brewer() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp1

# Exp.2
## model fittiing
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp2") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

fit <- brm(formula = value ~ 1 + (test * trial_z | subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)

## posterior prediction distriburuion
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
  ungroup() %>% 
  ggplot(aes(x = id, y = N_Look_pred)) +
  stat_interval(.width = c(.50, .80, .95)) +
  geom_point(data = df_summary1, aes(y = N_Look), size = 2) +
  facet_grid(test~.) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_color_brewer() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp2

# Exp.3
## model fittiing
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp3") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

fit <- brm(formula = value ~ 1 + (test * trial_z | subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)

## posterior prediction distriburuion
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
  ungroup() %>% 
  ggplot(aes(x = id, y = N_Look_pred)) +
  stat_interval(.width = c(.50, .80, .95)) +
  geom_point(data = df_summary1, aes(y = N_Look), size = 2) +
  facet_grid(test~.) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_color_brewer() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp3

# Exp.4
## model fittiing
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp4") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

fit <- brm(formula = value ~ 1 + (test * trial_z | subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)

## check posterior prediction distriburuion
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
  ungroup() %>% 
  ggplot(aes(x = id, y = N_Look_pred)) +
  stat_interval(.width = c(.50, .80, .95)) +
  geom_point(data = df_summary1, aes(y = N_Look), size = 2) +
  facet_grid(test~.) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_color_brewer() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp4

# Exp.5
## model fittiing
read_csv("data.csv", col_types = "cccdd") %>% 
  filter(Exp == "Exp5") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> df 

fit <- brm(formula = value ~ test + (test * trial_z | subject),
           data = df,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 11000,
           cores = 4,
           backend = "cmdstanr",
           refresh = 0,
           seed = 1234)

## posterior prediction distriburuion
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
  ungroup() %>% 
  ggplot(aes(x = id, y = N_Look_pred)) +
  stat_interval(.width = c(.50, .80, .95)) +
  geom_point(data = df_summary1, aes(y = N_Look), size = 2) +
  facet_grid(test~.) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_color_brewer() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp5

# Supplementary Figure 2
gp1 + labs(x = "Participant ID", y = "Number of trials with\nselective look at an aggressor",
           color = "Posterior predictive distribution") -> gp1

gp2 + labs(x = "Participant ID", y = "Number of trials with\nselective look at an aggressor",
           color = "Posterior predictive distribution") -> gp2

gp3 + labs(x = "Participant ID", y = "Number of trials with\nselective look at an aggressor",
           color = "Posterior predictive distribution") -> gp3

gp4 + labs(x = "Participant ID", y = "Number of trials with\nselective look at a causer",
           color = "Posterior predictive distribution") -> gp4

gp5 + labs(x = "Participant ID", y = "Number of trials with\nselective look at an aggressor",
           color = "Posterior predictive distribution") -> gp5

ggarrange(gp1, gp2, gp3, gp4, gp5, ncol = 3, nrow = 2, common.legend = TRUE, labels = letters[1:5]) %>% 
  ggexport(filename = here("Figures", "SupFigure2.jpg"), width = 6000, height = 3000, res = 300)