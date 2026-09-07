################################################################################
########    Did our response time manip lead to slower rt in mt con?    ########
########                    Sadie lane, 2026                            ########
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)
library(skimr)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data

averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

outlierless <- read_csv(
  "averages_no_n_nc_no_nback_outs.csv",
  na = c("", "NA")
)

# tidy data --------------------------------------------------------------------

#no transform, outliers included
all_for_t_tests <- averages |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean)
  )

#log transform, outliers included
all_for_t_tests_trsf <- all_for_t_tests |>
  mutate(
    rt_mt_log          = log(rt_mean_mt + 0.001),
    rt_st_log          = log(rt_mean_st + 0.001),
    tj_mt_log          = log(task_jumps_mean_mt + 0.001),
    tj_st_log          = log(task_jumps_mean_st + 0.001),
  ) |>
  select(sub, rt_mt_log, rt_st_log, tj_mt_log, tj_st_log)


#no transform, no outliers
for_t_tests <- outlierless |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean)
  )

#log transform, no outliers
for_t_tests_trsf <- for_t_tests |>
  mutate(
    rt_mt_log          = log(rt_mean_mt + 0.001),
    rt_st_log          = log(rt_mean_st + 0.001),
    tj_mt_log          = log(task_jumps_mean_mt + 0.001),
    tj_st_log          = log(task_jumps_mean_st + 0.001)
  ) |>
  select(sub, rt_mt_log, rt_st_log, tj_mt_log, tj_st_log)


# analyse ----------------------------------------------------------------------
# run t tests and summaries. Output into csvs. #n.b. this is all for switch = 0.
# rt, task jumps and ges


#first with outliers taken out
#which as a reminder is n_nc, and sens -
#(n-back, i.e. they weren't really mting)

t_rt_outless <- with(for_t_tests, t.test(rt_mean_st, rt_mean_mt, paired = T))
t_rt_outless <- tidy(t_rt_outless)
#sig (very much so)
write_csv(
  t_rt_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_rt_outless.csv"
)

t_rt_outless_trsf <- with(for_t_tests_trsf, t.test(rt_st_log, rt_mt_log, paired = T))
t_rt_outless_trsf <- tidy(t_rt_outless_trsf)
#sig (very much so)
write_csv(
  t_rt_outless_trsf,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_rt_outless_trsf.csv"
)

t_tj_outless <- with(for_t_tests, t.test(task_jumps_mean_st, task_jumps_mean_mt, paired = T))
t_tj_outless <- tidy(t_tj_outless)
#sig (very)
write_csv(
  t_tj_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless.csv"
)

t_tj_outless_trsf <- with(for_t_tests_trsf, t.test(tj_st_log, tj_mt_log, paired = T))
t_tj_outless_trsf <- tidy(t_tj_outless_trsf)
#sig (very much so)
write_csv(
  t_tj_outless_trsf,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless_trsf.csv"
)

#ge (no transform appropriate)
t_ge_outless <- with(for_t_tests, t.test(general_errors_mean_st, general_errors_mean_mt, paired = T))
t_ge_outless <- tidy(t_ge_outless)
#sig (but not too many ges being made)

write_csv(
  t_ge_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_ge_outless.csv"
)

#and get summary stats
outless_summary <- skim_without_charts(for_t_tests)
write_csv(
  outless_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/outless_summary.csv"
)

outless_trsf_summary <- skim_without_charts(for_t_tests_trsf)
write_csv(
  outless_trsf_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/outless_trsf_summary.csv"
)

#lets also get summary stats agnostic of mt or rt (for results section)
remove <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)


sum_blockagnostic_outlierless <- averages |>
  filter(!sub %in% remove) |>
  filter(ses == 4) |>
  group_by(sub) |>
  summarise(
    rt_mean = mean(rt_mean),
    tj_mean = mean(task_jumps_mean),
    ge_mean = mean(general_errors_mean),
    all_err_mean = mean(all_errors_mean)
  )

skim_blockagnostic_outlierless <- skim_without_charts(sum_blockagnostic_outlierless)

write_csv(
  skim_blockagnostic_outlierless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/skim_blockagnostic_outlierless.csv"
)

rt_se <- 0.104/sqrt(73)
te_se <- 0.307/sqrt(73)
ge_se <- 0.0243/sqrt(73)
all_err_se <- 0.0325/sqrt(73)




# now with all participants included -------------------------------------

all_t_rt <- with(all_for_t_tests, t.test(rt_mean_st, rt_mean_mt, paired = T))
all_t_rt <- tidy(all_t_rt)
#sig
write_csv(
  all_t_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_rt.csv"
)

#log adjusted is what we are interested in -> data becomes a lot more normal
all_t_rt_trsf <- with(all_for_t_tests_trsf, t.test(rt_st_log, rt_mt_log, paired = T))
all_t_rt_trsf <- tidy(all_t_rt_trsf)
#sig as well
write_csv(
  all_t_rt_trsf,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_rt_trsf.csv"
)

all_t_tj <- with(all_for_t_tests, t.test(task_jumps_mean_st, task_jumps_mean_mt, paired = T))
all_t_tj <- tidy(all_t_tj)
#ns - this makes sense - if you aren't multitasking you are less likely to task jump
write_csv(
  all_t_tj,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_tj.csv"
)

#now ge
all_t_ge <- with(all_for_t_tests, t.test(general_errors_mean_st, general_errors_mean_mt, paired = T))
all_t_ge <- tidy(all_t_ge)
#sig - highly similar with and without outliers

write_csv(
  all_t_ge,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_ge.csv"
)

all_summary <- skim_without_charts(all_for_t_tests)
write_csv(
  all_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_summary_tj_rt_ttests.csv"
)

all_trsf_summary <- skim_without_charts(all_for_t_tests_trsf)
write_csv(
  all_trsf_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_summary_tj_rt_ttests_trsf.csv"
)

#out of curiosity im gonna get rid of the 0.65 and 0.7 peeps
#and test them

sixfive <- c(13, 22, 25, 28, 51, 61, 63, 73, 76, 77, 84, 85)

seventy <- c(10, 13, 22, 25, 28, 42, 51, 55, 61, 63, 73, 76, 77, 78, 84, 85)

df1 <- all_for_t_tests |>
  filter(!sub %in% sixfive)

df1_trsf <- all_for_t_tests_trsf |>
  filter(!sub %in% sixfive)

df2 <- all_for_t_tests |>
  filter(!sub %in% seventy)

df2_trsf <- all_for_t_tests_trsf |>
  filter(!sub %in% seventy)

#trs rt tj and ge

with(df1_trsf, t.test(rt_st_log, rt_mt_log, paired = T))
#sig

with(df1_trsf, t.test(tj_st_log, tj_mt_log, paired = T))
#still sig

ge_sixfive <- with(df1, t.test(general_errors_mean_st, general_errors_mean_mt, paired = T))
tidy(ge_sixfive)
#still sig


# summary stats agnostic of st or mt --------------------------------------

#reclicks
outlierless |>
  filter(sub != 30, ses == 4, switch == 1) |>
  summarise(
    reclicks = mean(reclicks_mean),
    relcicks_sd = sd(reclicks_mean),
    rt = mean(rt_mean),
    rt_sd = sd(rt_mean),
    task_jumps = mean(task_jumps_mean),
    task_sd = sd(task_jumps_mean),
    gen_errors = mean(general_errors_mean),
    general_errors_sd = sd(general_errors_mean)
  )

#te
outlierless |>
  filter(ses == 4, switch == 0) |>
  summarise(
    TE_mean = mean(TE),
    TE_sd = sd(TE),
    rt = mean(rt_mean),
    rt_sd = sd(rt_mean),
    task_jumps = mean(task_jumps_mean),
    task_sd = sd(task_jumps_mean),
    gen_errors = mean(general_errors_mean),
    general_errors_sd = sd(general_errors_mean)
  )
