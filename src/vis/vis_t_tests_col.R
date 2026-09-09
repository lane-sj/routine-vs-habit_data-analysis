################################################################################
#####         script to make pretty plots from t tests comparing        ########
#####                 rt, task jumps and gen errors                     ########
#####                      Sadie Lane, 2026                             ########
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggsignif)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_no_n_nc_no_nback_outs.csv",
  na = c("", "NA")
)
# tidy --------------------------------------------------------------------

graph_ts <- averages |>
  filter(ses == 4, switch == 0) |>
  group_by(sub, block) |>
  mutate(
    block = factor(block, c("st", "mt"), c("ST", "MT"))
  ) |>
  summarise(
    rt_mean = mean(rt_mean) * 1000,
    tj_mean = mean(task_jumps_mean),
    ges_mean = mean(general_errors_mean),
  )

# plot --------------------------------------------------------------------
pal_fill <- c("#A6CEE390", "#1F78B490", "#B2DF8A90", "#33A02C90", "#FDBF6F90", "#FF7F0090")
pal_colour <- paletteer_d("RColorBrewer::Paired")

rt_pal_fill <- pal_fill[c(1, 2)]
rt_pal_colour <- pal_colour[c(1, 2)]

tj_pal_fill <- pal_fill[c(3, 4)]
tj_pal_colour <- pal_colour[c(3, 4)]

ge_pal_fill <- pal_fill[c(5, 6)]
ge_pal_colour <- pal_colour[c(7, 8)]

#response time
#palette

graph_ts |>
  ggplot(aes(x = block, y = rt_mean)) +
  stat_summary(fun = "mean", geom = "col", fill = "grey", colour = "grey") +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    position = position_dodge(width = 0.5), shape = 21, size = 3.5
  ) +
  stat_summary(fun = "mean", geom = "point", fill = "black", colour = "black", size = 2.5) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0, size = 1.3) +
  geom_line(aes(group = sub), alpha = 0.4, colour = "grey", position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = rt_pal_fill) +
  scale_colour_manual(values = rt_pal_colour) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean RT (ms)"
  )

ggsave(
  "rt_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

ggsave(
  "rt_dif_ttest.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

 ###################
#task jumps

graph_ts |>
  ggplot(aes(x = block, y = tj_mean)) +
  stat_summary(fun = "mean", geom = "col", fill = "grey", colour = "grey") +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    position = position_dodge(width = 0.5), shape = 21, size = 3.5
  ) +
  stat_summary(fun = "mean", geom = "point", fill = "black", colour = "black", size = 2.5) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0, size = 1.3) +
  geom_signif(
    data = graph_ts, comparisons = list(c("ST","MT")),
    annotation = "***", margin_top = 0.1, size = 1.2, textsize = 10, vjust = 0.5
  ) +
  scale_fill_manual(values = tj_pal_fill) +
  scale_colour_manual(values = tj_pal_colour) +
  geom_line(aes(group = sub), alpha = 0.4, colour = "grey", position = position_dodge(width = 0.5)) +
  ylim(c(0, 3)) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean Task Jumps"
  )

ggsave(
  "tj_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

ggsave(
  "tj_dif_ttest.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

 ####################

#finally gen errors
#also ns
#and same outliers excluded


graph_ts |>
  ggplot(aes(x = block, y = ges_mean)) +
  stat_summary(fun = "mean", geom = "col", fill = "grey", colour = "grey") +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    position = position_dodge(width = 0.5), shape = 21, size = 3.5
  ) +
  stat_summary(fun = "mean", geom = "point", fill = "black", colour = "black", size = 2.5) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0, size = 1.3) +
  geom_signif(
    data = graph_ts, comparisons = list(c("ST","MT")),
    annotation = "*", margin_top = 1.2, size = 1.5, textsize = 10, vjust = 0.1
  ) +
  scale_fill_manual(values = ge_pal_fill) +
  scale_colour_manual(values = ge_pal_colour) +
  geom_line(aes(group = sub), alpha = 0.4, colour = "grey", position = position_dodge(width = 0.5)) +
  ylim(c(0, 0.3)) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean General Errors"
  )

ggsave(
  "ge_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

ggsave(
  "ge_dif_ttest.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

