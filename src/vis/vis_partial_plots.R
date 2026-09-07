################################################################################
#####      Partial regression plots, TE, reclicks and all_errors_stay     ######
#####                           Sadie Lane 2026                           ######
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggtext)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data

trsf_partial <- read_csv(
  "trsf_errors_partialled_reclicks_TE.csv",
  na = c("", "NA")
)

reclicks_x_errors <-read_csv(
  "trsf_TE_partialled_reclicks_errors.csv",
  na = c("", "NA")
)

res_re_te_cor <- -0.4152888

res_re_err_cor <- 0.05300969

#relation of reclicks and te, partialling out all_errors_stay

#with sqrt transform for reclicks, log + 0.001 for all_errors
trsf_partial |>
  ggplot(aes(x = trsf_rTE, y = trsf_rReclicks)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = T, colour = "#C93312FF", fill = "#C93312FF", fullrange = TRUE) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.5,
    x = -0.3,
    y = -1.2,
    fontface = "italic",
    label = "r = -.415, p < .001"
  ) +
  labs(
    x = "Transition Entropy | All Errors",
    y = "Reclicks | All Errors"
  )

ggsave(
  "trsf_partial.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)

# relationship of reclicks and errors, partialling out TE ----------------------


reclicks_x_errors |>
  ggplot(aes(x = trsf_r_err_by_te, y = trsf_r_re_by_te)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#C93312FF") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
  ) +
  annotate(
    geom = "text",
    size = 4.5,
    x = -2.2,
    y = 1.4,
    fontface = "italic",
    label = "r = .053"
  ) +
  labs(
    y = "Reclicks | TE",
    x = "All Errors | TE"
  )

ggsave(
  "reclicks_x_errors_partial.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)
