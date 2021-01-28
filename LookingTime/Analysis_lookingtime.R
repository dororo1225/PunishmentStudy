library(tidyverse)
read_csv("data_lookingtime.csv", col_types = "cd") %>% 
  mutate(id = row_number()) %>% 
  relocate(id, .before = "exp") -> df

source("anovakun_485.txt")
anovakun(df, "As", 5, long = TRUE, eta = TRUE, eps = TRUE)
