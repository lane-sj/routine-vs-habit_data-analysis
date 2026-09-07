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
  filter(sub != 30) |>
  ggplot(aes(x = sqrt(reclicks_mean), y = RT_cost)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = T, colour = "#1F78B4FF", fill = "#1F78B4FF",  fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 2.5)) +
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
    size = 4.3,
    x = 0.55,
    y = -0.18,
    fontface = "italic",
    label = "r = -.246, p = .049"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "Mean Reclicks"
  )

ggsave(
  "RTcost_x_reclicks.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#TE
perform_dat |>
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
    axis.text.y = element_blank()
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.15,
    y = -0.18,
    fontface = "italic",
    label = "r = .083"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "Mean TE"
  )

ggsave(
  "RTcost_x_TE.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)


perform_dat |>
  ggplot(aes(x = ge_stay, y = RT_cost)) +
  geom_point(shape = 24, size = 3.2, stroke = 1.1, fill = "#899DA495", colour = "black") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.04,
    y = -0.18,
    fontface = "italic",
    label = "r = -.016"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "Mean General Errors"
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
  filter(sub != 30) |>
  ggplot(aes(x = sqrt(reclicks_mean), y = tj_cost)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#33A02CFF") +
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
    size = 4.3,
    x = 0.4,
    y = -1.6,
    fontface = "italic",
    label = "r = -.087"
  ) +
  labs(
    y = "Task Jump Cost (MT - ST)",
    x = "Mean Reclicks"
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
    axis.text.y = element_blank()
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.15,
    y = -1.6,
    fontface = "italic",
    label = "r = .186"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "Mean TE"
  )

ggsave(
  "TJcost_x_TE.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)


perform_dat |>
  ggplot(aes(x = ge_stay, y = tj_cost)) +
  geom_point(shape = 24, size = 3.2, stroke = 1.1, fill = "#899DA495", colour = "black") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  annotate(
    geom = "text",
    size = 4.3,
    x = 0.04,
    y = -1.6,
    fontface = "italic",
    label = "r = .259"
  ) +
  labs(
    y = "RT Cost (MT - ST)",
    x = "Mean General Errors"
  )

ggsave(
  "TJcost_x_errors.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4.3
)

