library(tidyverse)
library(lme4)
library(broom.mixed)
library(boot)
library(car)

# user-defined functions
inv_logit <- function(z){1/(1+exp(-z))}
myFunc <- function(mm) {
  # forgot the re.form argument on first try
  predict(mm, newdata = nd, type = "response", re.form = ~0)
}

# read data
read_csv("Ex4.csv", col_types = "cdddddddddddddddddddd") %>% 
  gather(key, value, -1) %>% 
  separate(key, c("test", "trial"), sep = " ") %>% 
  mutate(trial = as.numeric(trial),
         test = if_else(test == "pre", "Pretest", "Posttest"),
         test = factor(test, levels = c("Pretest", "Posttest")),
         id = as.numeric(factor(subject))) -> d4


# LRT test (full-null comparison)
fit4 <- glmer(value ~ test * trial + (1|id), data = d4, family = binomial)
fit_null <- glmer(value ~ 1 + (1|id), data = d4, family = binomial)
anova(fit4, fit_null, test = "Chisq")


# full model
## model summary (full model)
summary(fit4)


# reduced model
## model summary(reduced model)
fit4_reduced <- glmer(value ~ test + trial + (1|id), data = d4, family = binomial)
summary(fit4_reduced)


# calculate Bootstrap CIs of estimates
## full model
set.seed(1234)
fullBoot4 <- bootMer(fit4, fixef, nsim = 1000, parallel = "multicore", ncpus = 4)
tidy(fit4, effects = "fixed", conf.int = TRUE) %>% 
  bind_cols(tibble(lwr = envelope(fullBoot4, level = 0.95)$point[2, ],
                   upr = envelope(fullBoot4, level = 0.95)$point[1, ]))

## reduced model
reducedBoot4 <- bootMer(fit4_reduced, fixef, nsim = 1000, parallel = "multicore", ncpus = 4)
tidy(fit4_reduced, effects = "fixed", conf.int = TRUE) %>% 
  bind_cols(tibble(lwr = envelope(reducedBoot4, level = 0.95)$point[2, ],
                   upr = envelope(reducedBoot4, level = 0.95)$point[1, ]))


## calculate 95%CIs of predictive values (full model)
d4 %>% 
  group_by(test, trial) %>% 
  summarise(N = n(),
            res = sum(value),
            nres = N - res,
            prop = res / N,
            .groups = "drop") -> d4_plot
expand.grid(test = unique(d4_plot$test),
            trial = seq(1, 10, by = 0.25)) %>% 
  as_tibble() -> nd
bigBoot4 <- bootMer(fit4, myFunc, nsim = 1000, parallel = "multicore", ncpus = 4)

## visualization (full model)
nd1 <- nd
nd2 <- nd
nd3 <- nd
envPred1 <- envelope(bigBoot4, level = 0.95)
nd1$upper.point <- envPred1$point[1, ]
nd1$lower.point <- envPred1$point[2, ]

envPred2 <- envelope(bigBoot4, level = 0.9)
nd2$upper.point <- envPred2$point[1, ]
nd2$lower.point <- envPred2$point[2, ]

envPred3 <- envelope(bigBoot4, level = 0.5)
nd3$upper.point <- envPred3$point[1, ]
nd3$lower.point <- envPred3$point[2, ]

expand.grid(test = unique(d4_plot$test),
            trial = 1:10,
            stringsAsFactors = TRUE) %>% 
  mutate(dammy_test = if_else(test == "Posttest", 1, 0),
         z = summary(fit4)$coefficients[1] + summary(fit4)$coefficients[2] * dammy_test + summary(fit4)$coefficients[3] * trial + summary(fit4)$coefficients[4] *  dammy_test * trial,
         q = inv_logit(z)) %>% 
  ggplot(aes(x = trial)) +
  geom_ribbon(data = nd1, aes(ymax = upper.point, ymin = lower.point, fill = test), alpha = 0.25) +
  geom_point(data = d4_plot, size = 3, aes(y = prop, color = test), shape = 19) +
  geom_line(aes(y = q, color = test), lwd = 1.5) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_fill_viridis_d(begin = 0, end = 0.5, option = "D") +
  scale_x_continuous(breaks = 1:10)+
  ylim(0, 1) +
  labs(x = "Trial number", y = "Proportion of selective looks\nat an causer", fill = "Test type", color = "Test type", tag = "A") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 21, face = "bold"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(size = 0.75),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 18)) -> gp4
print(gp4)
