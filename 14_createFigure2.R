library(here)
library(tidyverse)
library(knitr)
library(cowplot)
library(effectsize)

# Exp.1
read_csv(here("Results",  "SensitivityAnalysis_Exp1.csv"), col_types = "dddd") %>% 
  mutate(Exp = "Exp.1") -> df_SA1

read_csv(here("Results", "ModelComparison_Exp1.csv"), col_types = "cdddddddc") %>% 
  pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
  group_by(Effects, incl) %>% 
  summarise(p_incl = sum(p_M),
            p_incl_givenData = sum(p_M_givenData),
            .groups = "drop") %>% 
  filter(incl == 1) %>% 
  select(!incl) %>% 
  mutate(BF_incl = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
         Effects = fct_relevel(Effects, "test", "trial"),
         Exp = "Exp.1") -> df_default1

# Exp.2
read_csv(here("Results",  "SensitivityAnalysis_Exp2.csv"), col_types = "dddd") %>% 
  mutate(Exp = "Exp.2") -> df_SA2

read_csv(here("Results", "ModelComparison_Exp2.csv"), col_types = "cdddddddc") %>% 
  pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
  group_by(Effects, incl) %>% 
  summarise(p_incl = sum(p_M),
            p_incl_givenData = sum(p_M_givenData),
            .groups = "drop") %>% 
  filter(incl == 1) %>% 
  select(!incl) %>% 
  mutate(BF_incl = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
         Effects = fct_relevel(Effects, "test", "trial"),
         Exp = "Exp.2") -> df_default2

# Exp.3
read_csv(here("Results",  "SensitivityAnalysis_Exp3.csv"), col_types = "dddd") %>% 
  mutate(Exp = "Exp.3") -> df_SA3

read_csv(here("Results", "ModelComparison_Exp3.csv"), col_types = "cdddddddc") %>% 
  pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
  group_by(Effects, incl) %>% 
  summarise(p_incl = sum(p_M),
            p_incl_givenData = sum(p_M_givenData),
            .groups = "drop") %>% 
  filter(incl == 1) %>% 
  select(!incl) %>% 
  mutate(BF_incl = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
         Effects = fct_relevel(Effects, "test", "trial"),
         Exp = "Exp.3") -> df_default3

# Exp.4
read_csv(here("Results",  "SensitivityAnalysis_Exp4.csv"), col_types = "dddd") %>% 
  mutate(Exp = "Exp.4") -> df_SA4

read_csv(here("Results", "ModelComparison_Exp4.csv"), col_types = "cdddddddc") %>% 
  pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
  group_by(Effects, incl) %>% 
  summarise(p_incl = sum(p_M),
            p_incl_givenData = sum(p_M_givenData),
            .groups = "drop") %>% 
  filter(incl == 1) %>% 
  select(!incl) %>% 
  mutate(BF_incl = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
         Effects = fct_relevel(Effects, "test", "trial"),
         Exp = "Exp.4") -> df_default4

# Exp.5
read_csv(here("Results",  "SensitivityAnalysis_Exp5.csv"), col_types = "dddd") %>% 
  mutate(Exp = "Exp.5") -> df_SA5

read_csv(here("Results", "ModelComparison_Exp5.csv"), col_types = "cdddddddc") %>% 
  pivot_longer(2:4, names_to = "Effects", values_to = "incl") %>% 
  group_by(Effects, incl) %>% 
  summarise(p_incl = sum(p_M),
            p_incl_givenData = sum(p_M_givenData),
            .groups = "drop") %>% 
  filter(incl == 1) %>% 
  select(!incl) %>% 
  mutate(BF_incl = (p_incl_givenData/(1 - p_incl_givenData))/(p_incl/(1 - p_incl)),
         Effects = fct_relevel(Effects, "test", "trial"),
         Exp = "Exp.5") -> df_default5

# Figure2
bind_rows(df_default1, df_default2, df_default3, df_default4, df_default5) %>% 
  mutate(scale = 1/sqrt(2)) -> df_default

bind_rows(df_SA1, df_SA2, df_SA3, df_SA4, df_SA5) %>% 
  pivot_longer(starts_with("BF"), names_prefix = "BF_", names_to = "Effects", values_to = "BF_incl") %>% 
  mutate(Effects = case_when(Effects == "int_incl" ~ "interaction",
                             Effects == "test_incl" ~ "test",
                             Effects == "trial_incl" ~ "trial"),
         Effects = fct_relevel(Effects, "test", "trial")) -> df_BF

tibble(thr = c(1/500, 1/100, 1/30, 1/10, 1/3, 1, 3, 10, 40),
       thr_log = log(thr)) -> df_thr

df_thr %>% 
  mutate(thr_log_lag = lag(thr_log),
         lbl_pos = (thr_log + thr_log_lag)/2,
         label = c(NA_character_, "Extreame", "Very Strong", "Strong", "Moderate", "Anecdotal", "Anecdotal", "Moderate", "Strong")) %>% 
  filter(!is.na(label)) -> df_label

df_thr %>% 
  filter(thr >= 1/100, thr <= 10) -> df_thr

df_BF %>% 
  ggplot(aes(x = scale, y = log(BF_incl), color = Effects)) +
  facet_wrap(~Exp) +
  geom_hline(data = df_thr, aes(yintercept = thr_log), lty = 3) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks = df_label$lbl_pos,
                     labels = df_label$label,
                     position = "right",
                     limits = c(min(log(df_BF$BF_incl)) - 0.2, max(log(df_BF$BF_incl)) + 0.5)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "top") -> dp1

df_BF %>% 
  ggplot(aes(x = scale, y = log(BF_incl))) +
  geom_smooth(aes(color = Effects), se = FALSE, lwd = 1) +
  geom_point(aes(color = Effects)) +
  geom_point(data = df_default, aes(x = 1/sqrt(2), fill = Effects), size = 3, shape = 21) +
  labs(x = "Scaling factor of the Cauchy prior", y = "log(Inclusion Bayes factor)") +
  scale_y_continuous(breaks = log(c(1/100, 1/30, 1/10, 1/3, 1, 3, 10)),
                     labels = c("log(1/100)", "log(1/30)", "log(1/10)", "log(1/3)", "0", "log(3)", "log(10)"),
                     limits = c(min(log(df_BF$BF_incl)) - 0.2, max(log(df_BF$BF_incl)) + 0.5)) +
  facet_wrap(~Exp) +
  theme_half_open() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "top")  -> dp2

aligned_plots <- align_plots(dp1, dp2, align = "hv", axis = "tblr")
gp <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
print(gp)
save_plot(here("Figures", "Figure2.jpg"), gp, base_width = 12, base_height = 8)