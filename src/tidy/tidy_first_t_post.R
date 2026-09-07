###############################################################################
########    Investigating impact on RT, TJ first trial post switch    ##########
########                    Sadie Lane, 2026                          ##########
################################################################################

rm(list=ls())
library(tidyverse)
library(emmeans)
library(afex)
library(PsyR)
library(broom)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

switch_avg <- read_csv(
  "routine_vs_habit_sw_frst-scnd_stvmt_avg.csv",
  na = c("", "NA")
)

switch_trl <- read_csv(
  "routine_vs_habit_sw_frst-scnd_stvmt_trl.csv",
  na = c("", "NA")
)

# quick_tidy ---------------------------------------------------------

switch_avg <- switch_avg |>
  mutate(
    block = fct_relevel(block, "st", "mt"),
    resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt")
  )

#and vis
switch_avg |>
  ggplot(aes(x = resp_type, y = mean_rt, colour = block, shape = block)) +
  stat_summary(fun = mean, geom = "point", size = 3, aes(group = block)) +
  stat_summary(fun = mean, geom = "line", aes(group = block)) +
  #stat_summary(fun.data = "mean_sdl", geom = "errorbar", colour = "black", width = 0.1) +
  theme_classic(base_size = 14)
#visually seems to be an interaction effect. Lets test it!

mod <- aov_ez(
  "sub",
  "mean_rt",
  switch_avg,
  within = c("block", "resp_type")
)
#the overall ANOVA shows sig effects at main effect response type and at
#interaction of response type and block :)

#define contrasts

conts <- list(
  "mt-st" = c(-1, -1, 1, 1),
  "fst-scnd" = c(1, -1, 1, -1),
  "int" = c(-1, 1, 1, -1)
)

emms <- emmeans(mod, c("block", "resp_type"))

cnt_res <- contrast(emms, conts)

output <- psyci(
  model = mod,
  contrast_tables = cnt_res,
  method = "ph",
  family_list = list("w"),
  within_factors = list("block", "resp_type"),
  alpha = .05
)





