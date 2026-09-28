################################################################################
##########       A script to output wide and long dfs with        ##############
##########      reclicks, TE, errors, automaticity and routine    ##############
##########                  Sadie Lane, 2026                      ##############
################################################################################

rm(list=ls())
library(tidyverse)

#set your working drive to where your csvs are
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_no_n_nc_no_nback_outs.csv",
  na = c("", "NA")
)

averages_all <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

#long

long_re_te_auto_rout <- averages_democohs |>
  select(sub:switch, reclicks_mean, TE, all_errors_mean, task_jumps_mean auto, rout) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE", "all_errors_mean")
  ) |>
  rename(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0,
    errors_switch = all_errors_mean_1,
    errors_stay = all_errors_mean_0
  ) |>
  select(sub, block, reclicks_mean, TE, errors_stay, errors_switch, auto, rout) |>
  pivot_longer(
    cols = reclicks_mean:rout,
    names_to = "dv",
    values_to = "values"
  )

write_csv(long_re_te_auto_rout, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/long_re_te_auto_rout.csv")

#long except split by mt and st

split_by_block <- averages|>
  select(sub:switch, reclicks_mean, TE, all_errors_mean, general_errors_mean, task_jumps_mean) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE", "all_errors_mean", "task_jumps_mean", "general_errors_mean")
  ) |>
  rename(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0,
    errors_switch = all_errors_mean_1,
    errors_stay = all_errors_mean_0,
    tjs_switch = task_jumps_mean_1,
    tjs_stay = task_jumps_mean_0,
    ge_switch = general_errors_mean_1,
    ge_stay = general_errors_mean_0
  ) |>
  select(sub, block, reclicks_mean, TE, errors_stay, errors_switch, tjs_stay, tjs_switch, ge_switch, ge_stay)

write_csv(split_by_block, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/split_by_block.csv")

split_by_block_all <- averages_all |>
  select(sub:switch, reclicks_mean, TE, all_errors_mean, general_errors_mean, task_jumps_mean) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE", "all_errors_mean", "task_jumps_mean", "general_errors_mean")
  ) |>
  rename(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0,
    errors_switch = all_errors_mean_1,
    errors_stay = all_errors_mean_0,
    tjs_switch = task_jumps_mean_1,
    tjs_stay = task_jumps_mean_0,
    ge_switch = general_errors_mean_1,
    ge_stay = general_errors_mean_0
  ) |>
  select(sub, block, reclicks_mean, TE, errors_stay, errors_switch, tjs_stay, tjs_switch, ge_switch, ge_stay)

write_csv(split_by_block_all, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/split_by_block_all.csv")
