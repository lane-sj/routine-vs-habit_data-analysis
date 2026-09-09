################################################################################
############    Benefits of habits, routines on multitasking    ################
############                  Sadie lane, 2026                  ################
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

perform_dat <- read_csv(
  "perform_dat_errors.csv",
  na = c("", "NA")
)

perform_dat_mt <- read_csv(
  "perform_dat_mt_errors.csv",
  na = c("", "NA")
)

no_thirty <- perform_dat |>
  filter(sub != 30)

# TE and Reclicks ---------------------------------------------------------

#rt
rtcost_mod <- lm(RT_cost ~ TE + reclicks_mean, data = perform_dat)
summary(rtcost_mod)
#ns

rtcost_trsf_mod <- lm(RT_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(rtcost_trsf_mod)
#ns

#task jumps (our accuracy proxy)
tjcost_mod <- lm(tj_cost ~ TE + reclicks_mean, data = perform_dat)
summary(tjcost_mod)
#ns

tjcost_trsf_mod <- lm(tj_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(tjcost_trsf_mod)
#ns


# TE, reclicks and general errors ----------------------------------------

rtcost_trsf_thirtyless <- lm(RT_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat_mt)
summary(rtcost_trsf_thirtyless)
#ns overall, sig on reclicks as predictor

tjcost_trsf_mod_thirtyless <- lm(tj_cost ~ TE + sqrt(reclicks_mean) + ge_stay, data = no_thirty)
summary(tjcost_trsf_mod_thirtyless)
#errors sig - errors sig predict task jumping
#i.e they were a bit lost.


# correlations ------------------------------------------------------------

#rt versus

  #reclicks

with(no_thirty, cor(reclicks_mean, RT_cost))

  #te

with(perform_dat, cor(TE, RT_cost))

  #ge stay

with(perform_dat, cor(ge_stay, RT_cost))

#tj versus

  #reclicks

with(no_thirty, cor(reclicks_mean, tj_cost))

  #te

with(perform_dat, cor(TE, tj_cost))

  #ge stay

with(perform_dat, cor(ge_stay, tj_cost))



