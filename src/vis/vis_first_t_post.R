################################################################################
########    Investigating impact on RT, TJ first trial post switch    ##########
########                    Sadie Lane, 2026                          ##########
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggdist)
library(ggpp)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

switch_avg <- read_csv(
  "routine_vs_habit_sw_frst-scnd_stvmt_avg.csv",
  na = c("", "NA")
)

# quick tidy -------------------------------------------------

remove <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

just_sens <- c(13, 22, 25, 28, 51, 61, 73, 76, 85)

switch_outlesser <- switch_avg |>
  mutate(
    mean_rt = mean_rt * 1000
  ) |>
  unite("dvs", block:resp_type, sep = "_") |>
  filter(!sub %in% remove)

# vis ---------------------------------------------------------------------

switch_outlesser |>
  ggplot(aes(y = mean_rt, x = dvs)) +
  stat_slab(width = 0.3, colour = "black",
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
  ) +
  geom_boxplot(
    alpha = 0.5, width = 0.05, colour = "black",
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_point(
    aes(stroke = 1.1, group = sub),
    position = position_dodgenudge(width = 0.2, x = -0.25),
    shape = 21, size = 3.5
  ) +
  geom_line(
    aes(group = sub), alpha = 0.4, colour = "grey",
    position = position_dodgenudge(width = 0.2, x = -0.25)
  ) +
  scale_y_continuous(limits = c(0, 1500)) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean RT (ms)",
    x = "Condition"
  )

#for tomorrow
#make median more salient
#add more ticks on y axis
#colours :)
#and finally change rt on piplup plot and on fig 6 to be * 1000

