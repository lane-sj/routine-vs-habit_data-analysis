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

partial <- read_csv(
  "errors_partialled_reclicks_TE.csv",
  na = c("", "NA")
)

reclicks_x_errors <- read_csv(
  "TE_partialled_reclicks_errors.csv",
  na = c("", "NA")
)

#relation of reclicks and te, partialling out all_errors_stay
#no transforms

partial |>
  ggplot(aes(x = rTE, y = rReclicks)) +
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
    x = 0.3,
    y = 5,
    fontface = "italic",
    label = "r = -.289, p = .008"
  ) +
  labs(
    x = "TE | Performance Errors",
    y = "Reclicks | Performance Errors"
  )

ggsave(
  "trsf_partial.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)

# relationship of reclicks and errors, partialling out TE ----------------------


reclicks_x_errors |>
  ggplot(aes(x = r_err_by_te, y = r_re_by_te)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#C93312FF") +
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
    x = 0.25,
    y = 5,
    fontface = "italic",
    label = "r = -.143, p = .195"
  ) +
  labs(
    y = "Reclicks | TE",
    x = "Performance Errors | TE"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "reclicks_x_errors_partial.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)
