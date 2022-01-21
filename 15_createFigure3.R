library(here)
library(brms)
library(cmdstanr)
library(tidyverse)
library(tidybayes)
library(modelr)
library(ggpubr)

# read data
read_csv("data.csv", col_types = "cccdd") %>% 
  mutate(trial_z = scale(trial, center = TRUE, scale = TRUE)[, 1],
         test = factor(test, levels = c("Pretest", "Posttest"))) -> df 
filter(df, Exp == "Exp1") -> d1
filter(df, Exp == "Exp2") -> d2
filter(df, Exp == "Exp3") -> d3
filter(df, Exp == "Exp4") -> d4
filter(df, Exp == "Exp5") -> d5

# Exp.1
fit1 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d1,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)

d1 %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            Prop = sum(value)/N,
            .groups = "drop") -> df1

d1 %>% 
  select(test, trial, trial_z) %>% 
  distinct() %>% 
  add_epred_draws(fit1, re_formula = NA) %>%
  median_qi() %>% 
  ungroup() -> df_MED1

df_MED1 %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower, fill = test), alpha = 0.2) +
  geom_line(aes(y = .epred, color = test), lwd = 1.5) +
  geom_point(data = df1, aes(y = Prop, color = test), size = 3) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_x_continuous(breaks = 1:10)+
  ylim(0, 1) +
  theme_classic() +
  labs(x = "Trial number", y = "Proportion of selective look\nat an aggressor", color = "Test type", fill = "Test type", tag = "a",
       title = "Punishment") +
  guides(color = "none", fill = "none") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 21, face = "bold"),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18))  -> gp1
print(gp1)

# Exp.2
fit2 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d2,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)

d2 %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            Prop = sum(value)/N,
            .groups = "drop") -> df2

d2 %>% 
  select(test, trial, trial_z) %>% 
  distinct() %>% 
  add_epred_draws(fit2, re_formula = NA) %>%
  median_qi() %>% 
  ungroup() -> df_MED2

df_MED2 %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower, fill = test), alpha = 0.2) +
  geom_line(aes(y = .epred, color = test), lwd = 1.5) +
  geom_point(data = df2, aes(y = Prop, color = test), size = 3) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_x_continuous(breaks = 1:10)+
  ylim(0, 1) +
  theme_classic() +
  labs(x = "Trial number", y = "Proportion of selective look\nat an aggressor", color = "Test type", fill = "Test type", tag = "b",
       title = "Contacting less negative") +
  guides(color = "none", fill = "none") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 21, face = "bold"),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18)) -> gp2
print(gp2)

# Exp.3
fit3 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d3,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)

d3 %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            Prop = sum(value)/N,
            .groups = "drop") -> df3

d3 %>% 
  select(test, trial, trial_z) %>% 
  distinct() %>% 
  add_epred_draws(fit3, re_formula = NA) %>%
  median_qi() %>% 
  ungroup() -> df_MED3

df_MED3 %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower, fill = test), alpha = 0.2) +
  geom_line(aes(y = .epred, color = test), lwd = 1.5) +
  geom_point(data = df3, aes(y = Prop, color = test), size = 3) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_x_continuous(breaks = 1:10)+
  ylim(0, 1) +
  theme_classic() +
  labs(x = "Trial number", y = "Proportion of selective look\nat an aggressor", color = "Test type", fill = "Test type", tag = "c",
       title = "50% reinforcement probability") +
  guides(color = "none", fill = "none") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 21, face = "bold"),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18)) -> gp3
print(gp3)

# Exp.4
fit4 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d4,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)

d4 %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            Prop = sum(value)/N,
            .groups = "drop") -> df4

d4 %>% 
  select(test, trial, trial_z) %>% 
  distinct() %>% 
  add_epred_draws(fit4, re_formula = NA) %>%
  median_qi() %>% 
  ungroup() -> df_MED4

df_MED4 %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower, fill = test), alpha = 0.2) +
  geom_line(aes(y = .epred, color = test), lwd = 1.5) +
  geom_point(data = df4, aes(y = Prop, color = test), size = 3) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_x_continuous(breaks = 1:10)+
  ylim(0, 1) +
  theme_classic() +
  labs(x = "Trial number", y = "Proportion of selective look\nat a causer", color = "Test type", fill = "Test type", tag = "d",
       title = "Inanimate agents") +
  guides(color = "none", fill = "none") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 21, face = "bold"),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18))  -> gp4
print(gp4)

# Exp.5
fit5 <- brm(formula = value ~ test * trial_z + (test * trial_z | subject),
            data = d5,
            family = bernoulli(link = "logit"),
            warmup = 1000,
            iter = 11000,
            cores = 4,
            backend = "cmdstanr",
            refresh = 0,
            seed = 1234)

d5 %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            Prop = sum(value)/N,
            .groups = "drop") -> df5

d5 %>% 
  select(test, trial, trial_z) %>% 
  distinct() %>% 
  add_epred_draws(fit5, re_formula = NA) %>%
  median_qi() %>% 
  ungroup() -> df_MED5

df_MED5 %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower, fill = test), alpha = 0.2) +
  geom_line(aes(y = .epred, color = test), lwd = 1.5) +
  geom_point(data = df5, aes(y = Prop, color = test), size = 3) +
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
        strip.text = element_text(face = "bold", size = 18)) -> gp5
print(gp5)

# legend
ggplot(df1,aes(x = trial)) +
  geom_point(size = 3, aes(y = Prop, color = test)) +
  geom_line(aes(y = Prop, color = test), lwd = 1.5) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        # axis.title.y = element_text(vjust = 1.5),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18),
        legend.position = c(1, 1),
        legend.justification = c(0.1, 0.5),
        legend.box.background = element_rect(size = 1.2),
        legend.key.size = unit(1.2, "cm"),
        legend.title = element_text(face = "bold", size = 21),
        legend.text = element_text(size = 21)) -> gp_legend
gp_leg <- get_legend(gp_legend, position = "left")
as_ggplot(gp_leg)

# Figure 3
ggarrange(gp1, gp2, gp3, gp4, gp5, gp_leg, ncol = 3, nrow = 2) %>%
  ggexport(filename = here("Figures", "Figure3.jpg"), width = 5000, height = 2500, res = 300)
