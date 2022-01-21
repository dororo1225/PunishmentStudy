library(here)
library(tidyverse)
library(tidybayes)

# read result file of 11_compareEffectSize.R
read_csv(here("Results", "EffectSizeComparison.csv")) %>% 
  mutate(Diff_ef_test = fct_rev(fct_relevel(Diff_ef_test,
                                            "Exp.1 - Exp.2", "Exp.1 - Exp.3", "Exp.1 - Exp.4", "Exp.1 - Exp.5",
                                            "Exp.5 - Exp.2", "Exp.5 - Exp.3", "Exp.5 - Exp.4",
                                            "Exp.4 - Exp.2", "Exp.4 - Exp.3", "Exp.3 - Exp.2"))) -> df_diff

# Supplementary Figure 1
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
ggsave(filename = here("Figures", "SupFigure1.jpg"), plot = gp, dpi = 300, width = 8, height = 8)