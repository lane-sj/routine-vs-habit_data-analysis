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

split_by_block <-  read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


# prepare for linear model ------------------------------------------------


