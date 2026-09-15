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


just_sens <- c(13, 22, 25, 28, 51, 61, 73, 76, 85)

switch_outlesser <- switch_avg |>
  mutate(
    mean_rt = mean_rt * 1000
  ) |>
  unite("dvs", block:switch, sep = "_") |>
  mutate(
    dvs = factor(dvs),
    dvs = fct_recode(
      dvs,
      "MT Stay" = "mt_0", "MT Switch" = "mt_1",
      "ST Stay" = "st_0", "ST Switch" = "st_1",
      ),
    dvs = fct_relevel(dvs, "ST Stay", "ST Switch", "MT Stay", "MT Switch")
  ) |>
  filter(!sub %in% just_sens)

switch_stay_outless <- switch_avg |>
  mutate(
    mean_rt = mean_rt * 1000,
    switch = factor(switch),
    switch = fct_recode(switch, "Stay" = "0", "Switch" = "1"),
    switch = fct_relevel(switch, "Stay", "Switch"),
    block = factor(block),
    block = fct_recode(block, "ST" = "st", "MT" = "mt"),
    block = fct_relevel(block, "ST", "MT"),
  ) |>
  rename(
    Block = "block",
    Switch = "switch"
  ) |>
  filter(!sub %in% just_sens)

#palettes
pal_fill <- c("#E394BB90","#D4419E90", "#8785B290", "#3A488A90")
pal_colour <- c("#E394BBFF", "#D4419EFF" , "#8785B2FF", "#3A488AFF")

pal_fill <- c("#D4419E90", "#3A488A90")
pal_colour <- c( "#D4419EFF" , "#3A488AFF")

stay_pal_fill <- pal_fill[c(1, 2)]
stay_pal_colour <- pal_colour[c(1, 2)]

switch_pal_fill <- pal_fill[c(3, 4)]
switch_pal_colour <- pal_colour[c(3, 4)]

# vis ---------------------------------------------------------------------

switch_stay_outless |>
  ggplot(aes(x = Block, y = mean_rt)) +
  stat_summary(
    fun = mean, geom = "line", linewidth = 1.2,
    aes(group = Switch, colour = Switch)
  ) +
  stat_summary(
    fun = mean, geom = "point", size = 4, stroke = 1.1,
    aes(group = Switch, colour = Switch, fill = Switch, shape = Switch)
    ) +
  stat_summary(
    geom = "errorbar", fun.data = mean_cl_boot, width = 0.05, linewidth = 1.3,
    alpha = 0.7, aes(group = Switch, colour = Switch)
    ) +
  scale_fill_manual(values = stay_pal_fill) +
  scale_colour_manual(values = stay_pal_colour) +
  scale_shape_manual(values = c("Stay" = 21, "Switch" = 24)) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks.x = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.2, 0.55),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean RT (ms)"
  )


ggsave(
  "interaction_block_mt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 4,
)

ggsave(
  "interaction_block_mt.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 4,
)




########## abandoned alternative
switch_outlesser |>
  ggplot(aes(y = mean_rt, x = dvs, fill = dvs, colour = dvs)) +
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
    #legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean RT (ms)",
    x = "Condition"
  )

