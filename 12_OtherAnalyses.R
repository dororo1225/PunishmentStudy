library(here)
library(tidyverse)
library(BayesFactor)

# read data
here("data_LookingTime.csv") %>% 
  read_csv(col_types = "ccddd") -> d

# Table S7
d %>%
  mutate(Movie_avg = (Movie1 + Movie2 + Movie3)/3) %>% 
  pivot_longer(starts_with("Movie"), names_to = "Movie") %>% 
  group_by(Exp, Movie) %>% 
  summarise(N = n(),
            Mean = mean(value),
            SD = sd(value),
            .groups = "drop") %>% 
  pivot_wider(names_from = "Movie", values_from = c("Mean", "SD"))

# Bayes Factor
d %>% 
  mutate(Movie_avg = (Movie1 + Movie2 + Movie3)/3) %>% 
  pivot_longer(starts_with("Movie"), names_to = "Movie") %>% 
  filter(Movie == "Movie_avg") -> df

df %>% 
  mutate(Exp = as.factor(Exp)) %>% 
  as.data.frame() %>% 
  anovaBF(value ~ Exp, data = ., iteration = 10000) -> fit
fit
