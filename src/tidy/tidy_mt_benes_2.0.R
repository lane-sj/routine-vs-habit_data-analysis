################################################################################
############          SL + KGG  tidy benes of reclicks, TE        ##############
################################################################################
#script to output a df for statistical analysis of costs
#of mt v st

rm(list=ls())
library(tidyverse)

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_no_n_nc_no_nback_outs.csv",
  na = c("", "NA")
)

split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("","NA")
)


#create ind_predictors using the split_by_block df
#using stay trials for errors
ind_predictors <- split_by_block |>
  filter(block == "st") |>
  select(sub:errors_stay, ge_stay)

ind_preds_mt <- split_by_block |>
  filter(block == "mt") |>
  select(sub:errors_stay, ge_stay)


#create costs df (difference scores in rt and errors per sub)
#note costs df has outliers (n_nc and sens < 60) taken out
costs <- averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(sub) |>
  summarise(
    RT_cost = rt_mean[block == "mt"] - rt_mean[block == "st"],
    error_cost = all_errors_mean[block == "mt"] - all_errors_mean[block == "st"],
    tj_cost = task_jumps_mean[block == "mt"] - task_jumps_mean[block == "st"]
  )
write_csv(costs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/costs.csv")

perform_dat <- inner_join(ind_predictors, costs, by = "sub")

write_csv(perform_dat, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/perform_dat_errors.csv")


cost_mt <- averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(sub) |>
  summarise(
    RT_cost = rt_mean[block == "mt"] - rt_mean[block == "st"],
    tj_cost = task_jumps_mean[block == "mt"] - task_jumps_mean[block == "st"]
  )


perform_dat_mt <- inner_join(ind_preds_mt, cost_mt, by = "sub")

write_csv(perform_dat_mt, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/perform_dat_mt_errors.csv")
