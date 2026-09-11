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

switch_stay_avg <- read_csv(
  "routine_vs_habit_sw_frst-scnd_stvmt_avg.csv",
  na = c("", "NA")
)

switch_stay_trl <- read_csv(
  "routine_vs_habit_sw_frst-scnd_stvmt_trl.csv",
  na = c("", "NA")
)

just_sens <- c(13, 22, 25, 28, 51, 61, 73, 76, 85)

# quick_tidy ---------------------------------------------------------
#remove <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

#
# switch <- switch_avg |>
#   mutate(
#     block = fct_relevel(block, "st", "mt"),
#     resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt")
#   )
#
# switch_sqrt <- switch_avg |>
#   mutate(
#     block = fct_relevel(block, "st", "mt"),
#     resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt"),
#     mean_rt = sqrt(mean_rt)
#   )

switch_outless <- switch_avg |>
  mutate(
    block = fct_relevel(block, "st", "mt"),
    resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt")
  ) |>
  filter(!sub %in% just_sens)

switch_outless_sqrt <- switch_avg |>
  mutate(
    block = fct_relevel(block, "st", "mt"),
    resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt"),
    mean_rt = sqrt(mean_rt)
  ) |>
  filter(!sub %in% just_sens)

# switch_outlesser <- switch_avg |>
#   mutate(
#     block = fct_relevel(block, "st", "mt"),
#     resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt")
#   ) |>
#   filter(!sub %in% remove)
#
# switch_outlesser_sqrt <- switch_avg |>
#   mutate(
#     block = fct_relevel(block, "st", "mt"),
#     resp_type = fct_relevel(resp_type, "frst_rt", "scnd_rt"),
#     mean_rt = sqrt(mean_rt)
#   ) |>
#   filter(!sub %in% remove)
#

#vis norms # to be adapted on the fly for viz transform
switch_avg |>
  ggplot(aes(sample = log(mean_rt))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ resp_type)

#and vis
switch_outless_sqrt |>
  ggplot(aes(x = resp_type, y = mean_rt, colour = block, shape = block)) +
  stat_summary(fun = mean, geom = "point", size = 3, aes(group = block)) +
  stat_summary(fun = mean, geom = "line", aes(group = block)) +
  #stat_summary(fun.data = "mean_sdl", geom = "errorbar", colour = "black", width = 0.1) +
  theme_classic(base_size = 14)
#visually seems to be an interaction effect. Lets test it!

# set aov
afex_options(emmeans_model = "multivariate")

mod <- aov_ez(
  "sub",
  "mean_rt",
  switch_outless_sqrt,
  within = c("block", "resp_type")
)
#the overall ANOVA shows sig effects at main effect response type and at
#interaction of response type and block :)

#define contrasts

conts <- list(
  "mt-st" = c(-1, 1, -1, 1),
  "fst-scnd" = c(1, 1, -1, -1),
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

#sig with outliers

#ns with n_nc and sens taken out


# what about switch v stay first correct responses -----------------------------


#tidy

switch_stay_outless <- switch_stay_avg |>
  mutate(
    block = fct_relevel(block, "st", "mt"),
    switch = factor(switch)
  ) |>
  filter(!sub %in% just_sens)

switch_stay_outless_log <- switch_stay_avg |>
  mutate(
    block = fct_relevel(block, "st", "mt"),
    switch = factor(switch),
    mean_rt = log(mean_rt)
  ) |>
  filter(!sub %in% just_sens)

#vis norms - here a log transform suits best
switch_avg |>
  ggplot(aes(sample = log(mean_rt))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch)

#and vis
switch_stay_outless_log |>
  ggplot(aes(x = switch, y = mean_rt, colour = block, shape = block)) +
  stat_summary(fun = mean, geom = "point", size = 3, aes(group = block)) +
  stat_summary(fun = mean, geom = "line", aes(group = block)) +
  theme_classic(base_size = 14)

switch_stay_outless_log |>
  ggplot(aes(x = block, y = mean_rt, colour = switch, shape = switch)) +
  stat_summary(fun = mean, geom = "point", size = 3, aes(group = switch)) +
  stat_summary(fun = mean, geom = "line", aes(group = switch)) +
  theme_classic(base_size = 14)
#it appears that switch does not vary by block
#and stay does vary by block


switch_stay_outless_log |>
  ggplot(aes(x = block, y = mean_rt, fill = switch)) +
  geom_boxplot() +
  theme_classic(base_size = 14)

#test

afex_options(emmeans_model = "multivariate")

mod_two <- aov_ez(
  "sub",
  "mean_rt",
  switch_stay_outless_log,
  within = c("block", "switch")
)
                            #st    mt    st   mt
                            #0     0     1    1
wth_conts <- list(
  "mt-st"               = c(-0.5, 0.5, -0.5, 0.5),
  "switch-stay"         = c(-0.5, -0.5, 0.5, 0.5),
  "int"                 = c(0.5, -0,5, -0.5, 0.5),
)

emms_two <- emmeans(mod_two, c("block", "switch"))

cnt_res_two <- contrast(emms_two, conts_two)

#produce simplefx
switch_by_block <- emmeans(mod_two, ~ switch | block)
simp_fx_contrasts <- list("block" = c(-1, 1))
switch_simp_fx <- contrast (switch_by_block, method = simp_fx_contrasts)


block_by_switch <- emmeans(mod_two, ~ block | switch)
block_simp_fx <- contrast(block_by_switch, method = simp_fx_contrasts)

all_fx <- c(cnt_res_two, switch_simp_fx, block_simp_fx)
family_list = as.list(rep("w", length(all_fx)))
alphas = 0.05 # because we adjust by contrast


p <- 2
q <- 2
smr_params <- list(
  p = p,
  q = q,
  n_sim = 100000, # this is also the default
  seed = 42 # set seed if you want to replicate simulation of smr distribution
)

output_two <- psyci(
  model = mod_two,
  contrast_tables = all_fx,
  method = "smr",
  family_list = family_list,
  within_factors = list("block", "switch"),
  alpha = alphas,
  smr_params = smr_params
)
