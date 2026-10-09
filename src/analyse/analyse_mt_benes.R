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

rtcost_trsf_mod <- lm(RT_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(rtcost_trsf_mod)
#ns

rtcost_mod <- lm(RT_cost ~ TE + reclicks_mean, data = perform_dat)
summary(rtcost_mod)
#ns

tjcost_trsf_mod <- lm(tj_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(tjcost_trsf_mod)
#ns

tjcost_mod <- lm(tj_cost ~ TE + reclicks_mean, data = perform_dat)
summary(tjcost_mod)
#

# TE, reclicks and general errors ----------------------------------------

rtcost_trsf <- lm(RT_cost ~ TE + sqrt(reclicks_mean) + ge_stay, data = perform_dat)
summary(rtcost_trsf)
#ns

rtcost <- lm(RT_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat)
summary(rtcost)
#ns

tjcost_trsf <- lm(tj_cost ~ TE + sqrt(reclicks_mean) + ge_stay, data = perform_dat)
summary(tjcost_trsf)
#ns

tjcost <- lm(tj_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat)
summary(tjcost)
#ns

# correlations ------------------------------------------------------------

#rt versus

  #reclicks

with(perform_dat, cor(reclicks_mean, RT_cost))

  #te

with(perform_dat, cor(TE, RT_cost))

  #ge stay

with(perform_dat, cor(ge_stay, RT_cost))

#tj versus

  #reclicks

with(perform_dat, cor(reclicks_mean, tj_cost))

  #te

with(perform_dat, cor(TE, tj_cost))

  #ge stay

with(perform_dat, cor(ge_stay, tj_cost))



# sub 30 exclusion criteria -----------------------------------------------

summary(perform_dat$reclicks_mean)

sd(perform_dat$reclicks_mean) * 2.5



# check mt ----------------------------------------------------------------

mt_rtcost_mod <- lm(RT_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat_mt)
summary(mt_rtcost_mod)

mt_tjcost_mod <- lm(tj_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat_mt)
summary(mt_tjcost_mod)
#

