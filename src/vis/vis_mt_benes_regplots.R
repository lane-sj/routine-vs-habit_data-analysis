################################################################################
############              SL vis  benes of reclicks, TE           ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
perform_dat <- read_csv(
  "perform_dat_errors.csv",
  na = c("", "NA")
)

# rt ----------------------------------------------------------------------

#reclicks
perform_dat |>
  mutate(
    RT_cost = RT_cost * 1000
  ) |>
  ggplot(aes(x = reclicks_mean, y = RT_cost)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#1F78B4FF") +
  #scale_x_continuous(limits = c(0, 3)) +
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
    size = 4.3,
    x = 2.5,
    y = -180,
    fontface = "italic",
    label = "r = -.171, p = .201"
  ) +
  labs(
    y = "RT Cost (ms; MT - ST)",
    x = "Reclicks"
  )

ggsave(
  "RTcost_x_reclicks.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#TE
perform_dat |>
  mutate(
    RT_cost = RT_cost * 1000
  ) |>
  ggplot(aes(x = TE, y = RT_cost)) +
  geom_point(shape = 23, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#1F78B4FF") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.25,
    y = -180,
    fontface = "italic",
    label = "r = .083, p = .802"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "TE"
  )

ggsave(
  "RTcost_x_TE.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#ges
perform_dat |>
  mutate(
    RT_cost = RT_cost * 1000
  ) |>
  ggplot(aes(x = ge_stay, y = RT_cost)) +
  geom_point(shape = 24, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.04,
    y = -180,
    fontface = "italic",
    label = "r = -.016, p = .757"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "General Errors"
  )

ggsave(
  "RTcost_x_errors.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

# task jumps --------------------------------------------------------------
#it will give an error that 3 rows have been dropped
#these are subs 8 9 and 11 who had too few trials from which to calculate a task jump cost
#and are na-ed out


#reclicks
perform_dat |>
  ggplot(aes(x = reclicks_mean, y = tj_cost)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#33A02CFF") +
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
    size = 4.3,
    x = 2.5,
    y = -1.6,
    fontface = "italic",
    label = "r = -.010, p = .799"
  ) +
  labs(
    y = "Task Jump Cost (MT - ST)",
    x = "Reclicks"
  )

ggsave(
  "TJcost_x_reclicks.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#TE
perform_dat |>
  ggplot(aes(x = TE, y = tj_cost)) +
  geom_point(shape = 23, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#33A02CFF") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.25,
    y = -1.6,
    fontface = "italic",
    label = "r = .186, p = .285"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "TE"
  )

ggsave(
  "TJcost_x_TE.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)


perform_dat |>
  ggplot(aes(x = ge_stay, y = tj_cost)) +
  geom_point(shape = 24, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.04,
    y = -1.6,
    fontface = "italic",
    label = "r = .259, p = .051"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "General Errors"
  )

ggsave(
  "TJcost_x_errors.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

