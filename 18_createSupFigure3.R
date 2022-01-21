library(here)
library(brms)
library(cmdstanr)
library(tidybayes)
library(ggrepel)
library(tidyverse)
library(knitr)
library(ggpubr)
options(mc.cores = parallel::detectCores())

# read data
df <- read_csv("data.csv", col_types = "cccdd")
## Exp.1
df %>% 
  filter(Exp == "Exp1") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> d1 
## Exp.2
df %>% 
  filter(Exp == "Exp2") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> d2 
## Exp.3
df %>% 
  filter(Exp == "Exp3") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> d3 
## Exp.4
df %>% 
  filter(Exp == "Exp4") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> d4 
## Exp.5
df %>% 
  filter(Exp == "Exp5") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> d5 

## read result file of 13_PowerAnalysis.R
here("Results", "SimulatedPower.csv") %>%
  read_csv(col_types = "ddddddd") -> df_result

df_result %>%
  group_by(sd_ri, sd_rs) %>% 
  summarise(N = n(),
            N_conv = sum(convergence),
            N_detect = sum(convergence==1 & Hit==1),
            Prop_conv = N_conv/N * 100,
            Power = N_detect/N_conv,
            .groups = "drop") -> df_power

# model fitting
## Exp.1
fit1 <- brm(formula = value ~ test * trial_z + (test*trial_z | subject),
            data = d1,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)
fit1 %>%
  gather_draws(sd_subject__Intercept, sd_subject__testPosttest) %>% 
  summarise_draws() -> sd1

## Exp.2
fit2 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d2,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)
fit2 %>% 
  gather_draws(sd_subject__Intercept, sd_subject__testPosttest) %>% 
  summarise_draws() -> sd2

## Exp.3
fit3 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d3,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)
fit3 %>% 
  gather_draws(sd_subject__Intercept, sd_subject__testPosttest) %>% 
  summarise_draws() -> sd3

## Exp.4
fit4 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d4,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)
fit4 %>% 
  gather_draws(sd_subject__Intercept, sd_subject__testPosttest) %>% 
  summarise_draws() -> sd4

## Exp.5
fit5 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d5,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)
fit5 %>% 
  gather_draws(sd_subject__Intercept, sd_subject__testPosttest) %>% 
  summarise_draws() -> sd5

# Supplememntary Figure 3
bind_rows(sd1, sd2, sd3, sd4, sd5) %>% 
  ungroup() %>% 
  mutate(Exp = rep(str_c("Exp.", 1:5), each = length(unique(.variable)))) %>% 
  relocate(Exp) %>% 
  filter(.variable == "sd_subject__Intercept" | .variable == "sd_subject__testPosttest") %>% 
  mutate(.variable = if_else(.variable == "sd_subject__Intercept", "int", "slp")) %>%
  select(Exp, .variable, median) %>% 
  pivot_wider(names_from = c(".variable"), values_from = "median") -> df_rnd_sum

## Supplememntary Figure 3a
df_power %>% 
  ggplot() +
  geom_tile(aes(x = sd_ri, y = sd_rs, fill = Power), color = "grey75") +
  geom_point(data = df_rnd_sum, aes(x = int, y = slp), size = 3) +
  geom_text_repel(data = df_rnd_sum, aes(x = int, y = slp, label = Exp), size = 4) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 2, by = 0.2)) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(x = "SD of random intercept", y = "SD of random slope for the test type effect", fill = "Simulated\nPower") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(color = "black", size = 12),
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 12)) -> gp1
print(gp1)

## Supplememntary Figure 3b
df_power %>% 
  mutate(Power_over = if_else(Power >= 0.8, ">= 0.8", "< 0.8")) %>% 
  ggplot() +
  geom_tile(aes(x = sd_ri, y = sd_rs, fill = Power_over), color = "grey75") +
  geom_point(data = df_rnd_sum, aes(x = int, y = slp), size = 3) +
  geom_text_repel(data = df_rnd_sum, aes(x = int, y = slp, label = Exp), size = 4) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 2, by = 0.2)) +
  scale_fill_viridis_d(begin = 0.5, end = 0.95) +
  labs(x = "SD of random intercept", y = "SD of random slope for the test type effect", fill = "Simulated\nPower") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(color = "black", size = 12),
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 12)) -> gp2
print(gp2)

## Supplementary Figure 3
ggarrange(gp1, gp2, labels = letters[1:2], ncol = 1) %>% 
  ggexport(filename = here("Figures", "SupFigure3.jpg"), width = 2000, height = 3000, res = 300)