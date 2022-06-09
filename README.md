Third-party punishment by infants
====

## Overview
This repository includes the R codes and dataset for the following paper:

Kanakogi, Y., Miyazaki, M., Takahashi, H., Yamamoto, H., Kobayashi, T., & Hiraki, K. (2022). Third-party punishment by preverbal infants. Nature Human Behaviour. https://doi.org/10.1038/s41562-022-01354-2

## Description
This repository consists of 2 folders, 2 csv files, and 18 R scripts. Some R scripts output figures and csv files are shown in the folders ‘Figures’ and ‘Results’, respectively.

A simple folder structure.csv files is shown below:

- csv files
  - data.csv
  - data_LookingTime.csv
- Folders
  - Results
  - Figures
- R scripts
  - 01_Exp1.R
  - 02_Exp2.R
  - 03_Exp3.R
  - 04_Exp4.R
  - 05_Exp5.R
  - 06_SensitivityAnalysis_Exp1.R
  - 07_SensitivityAnalysis_Exp2.R
  - 08_SensitivityAnalysis_Exp3.R
  - 09_SensitivityAnalysis_Exp4.R
  - 10_SensitivityAnalysis_Exp5.R
  - 11_compareEffectSize.R
  - 12_OtherAnalyses.R
  - 13_PowerAnalysis.R
  - 14_createFigure2.R
  - 15_createFigure3.R
  - 16_createSupFigure1.R
  - 17_createSupFigure2.R
  - 18_createSupFigure3.R

  The R scripts beginning with the numbers 01 through 05 are the codes for the main analysis of Experiments 1 to 5, respectively. These R scripts compare statistical models based on Bayes Factor, conduct analysis for each effect with Inclusion Bayes factor, and estimate parameters of the best model.

  The R scripts beginning with the numbers 06 through 10 are the codes for the sensitivity analysis for each effect in Experiments 1 to 5, respectively.

  11_compareEffectSize.R is used to compare the effect size for the test type between Experiments. 12_OtherAnalyses.R analyses whether there is a difference in the infant's looking time for an aggressive interaction between experiments. 13_PowerAnalysis.R simulates sampling to compute power, taking into consideration the actual sample size and the theoretically expected effect size (see Supplementary Information of our paper).

  The R scripts beginning with the numbers 14 through 18 are the graphing codes for figures in the main text and supplementary material.

  Although we placed numbers on the file name of the R scripts, these codes can be processed in any order preferred. Note that the codes for sensitivity analysis and power analysis is a long time consuming process. We placed csv files for the results of these analyses in the folder, ‘Results’ (i.e., ‘SimulatedPower.csv’ and csv files begins with ‘SensitivityAnalysis_’). In the R scripts for figures, these csv files can be used to create figures without waiting for an entire run of prerequired codes.


## Data Structure
- data.csv
  - This csv file is used with all R scripts except for 12_OtherAnalyses.R, 14_createFigure2.R, and 16_createSupFigure1.

| Column Name     | Variable                | Explanation                                                                        |
| ----            | ----                    |   ----                                                                             |
| Exp             |qualitative              | Experiment (‘Exp1’, ‘Exp2’, ‘Exp3’, ‘Exp4’, or ‘Exp5’)                             |
| subject         |qualitative              | Participant ID                                                                     |
| test            |qualitative              | Phase of the trial (‘Pretest’ or ‘Posttest’)                                       |
| trial           |quantitative (integer)   | Order of the trial (1 - 10)                                                        |
| value           |quantitative (binary)    | Target of infant's selective looking (an aggressor or a causer = 1; otherwise = 0) |



- data_LookingTime.csv
  - This csv file is used with 12_OtherAnalyses.R.

| Column Name     | Variable                | Explanation                                                                   |
| ----            | ----                    |   ----                                                                        |
| Exp             |qualitative              | Experiment (‘Exp1’, ‘Exp2’, ‘Exp3’, ‘Exp4’, or ‘Exp5’)                        |
| subject         |qualitative              | Participant ID                                                                |
| Animation1      |quantitative (continuous)| Infant's looking time to the first aggressive interaction in the movie phase  |
| Animation2      |quantitative (continuous)| Infant's looking time to the second aggressive interaction in the movie phase |
| Animation3      |quantitative (continuous)| Infant's looking time to the third aggressive interaction in the movie phase  |


## Software & Package Versions
- RStudio: 1.4.1717
- R: 4.0.3
- Stan: 2.26.1
- tidyverse: 1.3.1
- brms: 2.15.0
- rstan: 2.26.2
- cmdstanr: 0.3.0
- tidybayes: 3.0.0
- broom.mixed: 0.2.7
- BayesFactor: 0.9.12.4.3
- bayestestR: 0.11.5
- effectsize: 0.5
- broom.mixed: 0.2.7
- ggpubr: 0.4.0
- cowplot: 1.1.1
- ggrepl: 0.9.1
- here: 1.0.1


## The Author of this README File
- [Hiroki Yamamoto](https://github.com/dororo1225)
