################################################################################
#####         script for pretty plots from t tests comparing            ########
#####              reclicks at mt vs st, TE at mt v st                  ########
#####                      Sadie Lane, 2026                             ########
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggsignif)
library(ggdist)
library(ggpp)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")
source("function_cousineau_morrey.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

# tidy --------------------------------------------------------------------
remove <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

# #raincloud
# graph_reclicks <- averages |>
#   filter(ses == 4, switch == 1) |>
#   group_by(sub, block) |>
#   mutate(
#     block = factor(block, c("st", "mt"), c("ST", "MT")),
#     reclicks_mean = mean(reclicks_mean),
#     TE_mean = mean(TE)
#   ) |>
#   filter(!sub %in% remove)
#
# graph_TE <- averages |>
#   filter(ses == 4, switch == 0) |>
#   group_by(sub, block) |>
#   mutate(
#     block = factor(block, c("st", "mt"), c("ST", "MT")),
#     reclicks_mean = mean(reclicks_mean),
#     TE_mean = mean(TE)
#   ) |>
#   filter(!sub %in% remove)


#cols
col_reclicks <- averages |>
  filter(ses == 4, switch == 1 ) |>
  group_by(sub, block) |>
  mutate(
    block = factor(block, c("st", "mt"), c("ST", "MT"))
  ) |>
  summarise(
    reclicks_mean = mean(reclicks_mean)
  ) |>
  filter(!sub %in% remove)

col_TE <- averages |>
  filter(ses == 4, switch == 0) |>
  group_by(sub, block) |>
  mutate(
    block = factor(block, c("st", "mt"), c("ST", "MT"))
  ) |>
  summarise(
    TE_mean = mean(TE)
  ) |>
  filter(!sub %in% remove)

###alternative colours:
#fill: "#42439590", "#2C176990"
#colour "#424395FF", "#2C1769FF"


pal_fill <- c("#FD646790", "#C9331290", "#90D4CC90","#0A9F9D90" )
pal_colour <- c("#FD6467FF", "#C93312FF", "#90D4CCFF", "#0A9F9DFF")

reclicks_pal_fill <- pal_fill[c(1, 2)]
reclicks_pal_colour <- pal_colour[c(1, 2)]

TE_pal_fill <- pal_fill [c(3, 4)]
TE_pal_colour <- pal_colour [c(3, 4)]


# cousineau-morrey CIs ----------------------------------------------------

#CI = Mean +/- t(1-alpha/2, n-1) * (SD / sqrt(n)) * sqrt(c / (c-1))


#reclicks

#st
reclicks_ST_CIs <- col_reclicks |>
  filter(block == "ST")

upper_reclicks_st_CIs <- cm_upper(
  mean(reclicks_ST_CIs$reclicks_mean),
  n = 73,
  sd(reclicks_ST_CIs$reclicks_mean),
  c = 2
  )
lower_reclicks_st_CIs <- cm_lower(
  mean(reclicks_ST_CIs$reclicks_mean),
  n = 73,
  sd(reclicks_ST_CIs$reclicks_mean),
  c = 2
)

#mt
reclicks_MT_CIs <- col_reclicks |>
  filter(block == "MT")

upper_reclicks_mt_CIs <- cm_upper(
  mean(reclicks_MT_CIs$reclicks_mean),
  n = 73,
  sd(reclicks_MT_CIs$reclicks_mean),
  c = 2
)
lower_reclicks_mt_CIs <- cm_lower(
  mean(reclicks_MT_CIs$reclicks_mean),
  n = 73,
  sd(reclicks_MT_CIs$reclicks_mean),
  c = 2
)

#now make a df to store that in
reclicks_cis <- data.frame(
  block = c("ST", "MT"),
  cis_lower = c(lower_reclicks_st_CIs, lower_reclicks_mt_CIs),
  cis_upper = c(upper_reclicks_st_CIs, upper_reclicks_mt_CIs)
)

#TE
TE_ST_CIs <- col_TE |>
  filter(block == "ST")

upper_TE_st_CIs <- cm_upper(
  mean(TE_ST_CIs$TE_mean),
  n = 73,
  sd(TE_ST_CIs$TE_mean),
  c = 2
)
lower_TE_st_CIs <- cm_lower(
  mean(TE_ST_CIs$TE_mean),
  n = 73,
  sd(TE_ST_CIs$TE_mean),
  c = 2
)

TE_MT_CIs <- col_TE |>
  filter(block == "MT")

upper_TE_mt_CIs <- cm_upper(
  mean(TE_MT_CIs$TE_mean),
  n = 73,
  sd(TE_MT_CIs$TE_mean),
  c = 2
)
lower_TE_mt_CIs <- cm_lower(
  mean(TE_MT_CIs$TE_mean),
  n = 73,
  sd(TE_MT_CIs$TE_mean),
  c = 2
)

#now make a df to store that in
TE_cis <- data.frame(
  block = c("ST", "MT"),
  cis_lower = c(lower_TE_st_CIs, lower_TE_mt_CIs),
  cis_upper = c(upper_TE_st_CIs, upper_TE_mt_CIs)
)

# graph -------------------------------------------------------------------

#this is a sig result - add in post bc this format doesn't work with it
# graph_reclicks |>
#   filter(sub != 30) |> #30 is outlier on both mt and st at mean + 2.5*sd
#   ggplot(aes(x = switch, y = reclicks_mean)) +
#   stat_slab(
#     aes(fill = block), width = 0.3, colour = "black",
#     data = ~ filter(.x, block == "ST"),
#     side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
#   ) +
#   stat_slab(
#     aes(fill = block), width = 0.3, colour = "black",
#     data = ~ filter(.x, block == "MT"),
#     side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
#   ) +
#   geom_boxplot(
#     aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
#     data = ~ filter(.x, block == "ST"),
#     position = position_nudge(x = -0.45), outlier.color = NA
#   ) +
#   geom_boxplot(
#     aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
#     data = ~ filter(.x, block == "MT"),
#     position = position_nudge(x = 0.45), outlier.color = NA
#   ) +
#   geom_point(
#     aes(stroke = 1.1, colour = block, fill = block, group = sub),
#     data = ~ filter(.x, block == "ST"),
#     position = position_dodgenudge(width = 0.2, x = -0.25), shape = 21, size = 3.5
#   ) +
#   geom_point(
#     aes(stroke = 1.1, colour = block, fill = block, group = sub),
#     data = ~ filter(.x, block == "MT"),
#     position = position_dodgenudge(width = 0.2, x = 0.25), shape = 21, size = 3.5
#   ) +
#   scale_fill_manual(values = reclicks_pal_fill) +
#   scale_colour_manual(values = reclicks_pal_colour) +
#   ylim(0, 10) +
#   plot_style() +
#   theme(
#     axis.title = element_text(face = "bold"),
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title.y = element_text(margin = margin (r = 15)),
#     axis.line = element_line(colour = "grey"),
#     axis.ticks.x = element_blank(),
#     legend.position = "none",
#     strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
#   ) +
#   labs(
#     y = "Mean Reclicks"
#   )
#
# ggsave(
#   "reclicks_dif_ttest_raincloud.png",
#   path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
#   width = 4,
#   height = 6,
# )
#
# ggsave(
#   "reclicks_dif_ttest_raincloud.svg",
#   path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
#   width = 4,
#   height = 6,
# )
#
# #te time
# #te has no outliers.
# #add sig in post
#
# graph_TE |>
#   ggplot(aes(x = switch, y = TE_mean)) +
#   stat_slab(
#     aes(fill = block), width = 0.3, colour = "black",
#     data = ~ filter(.x, block == "ST"),
#     side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
#   ) +
#   stat_slab(
#     aes(fill = block), width = 0.3, colour = "black",
#     data = ~ filter(.x, block == "MT"),
#     side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
#   ) +
#   geom_boxplot(
#     aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
#     data = ~ filter(.x, block == "ST"),
#     position = position_nudge(x = -0.45), outlier.color = NA
#   ) +
#   geom_boxplot(
#     aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
#     data = ~ filter(.x, block == "MT"),
#     position = position_nudge(x = 0.45), outlier.color = NA
#   ) +
#   geom_point(
#     aes(stroke = 1.1, colour = block, fill = block, group = sub),
#     data = ~ filter(.x, block == "ST"),
#     position = position_dodgenudge(width = 0.2, x = -0.25), shape = 21, size = 3.5
#   ) +
#   geom_point(
#     aes(stroke = 1.1, colour = block, fill = block, group = sub),
#     data = ~ filter(.x, block == "MT"),
#     position = position_dodgenudge(width = 0.2, x = 0.25), shape = 21, size = 3.5
#   ) +
#   scale_fill_manual(values = TE_pal_fill) +
#   scale_colour_manual(values = TE_pal_colour) +
#   ylim(0, 1) +
#   plot_style() +
#   theme(
#     axis.title = element_text(face = "bold"),
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title.y = element_text(margin = margin (r = 15)),
#     axis.line = element_line(colour = "grey"),
#     axis.ticks.x = element_blank(),
#     legend.position = "none",
#     strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
#   ) +
#   labs(
#     y = "Mean TE"
#   )
#
# ggsave(
#   "TE_dif_ttest_raincloud.png",
#   path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
#   width = 4,
#   height = 6,
# )
#
# ggsave(
#   "TE_dif_ttest_raincloud.svg",
#   path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
#   width = 4,
#   height = 6,
# )



# col graphs --------------------------------------------------------------

col_reclicks |>
  #filter(sub != 30) |>
  ggplot() +
  stat_summary(
    aes(x = block, y = reclicks_mean),
    fun = "mean", geom = "col", fill = "grey", colour = "grey"
    ) +
  geom_point(
    aes(x = block, y = reclicks_mean, stroke = 1.1, colour = block, fill = block, group = sub),
    position = position_dodge(width = 0.5), shape = 21, size = 3.5
  ) +
  stat_summary(
    aes(x = block, y = reclicks_mean),
    fun = "mean", geom = "point", fill = "black", colour = "black", size = 2.5
    ) +
  geom_errorbar(data = reclicks_cis, aes(x = block, ymin = cis_lower, ymax = cis_upper), color = "black", width = 0, size = 1.3)  +
  #stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0, size = 1.3) +
  geom_line(aes(group = sub, x = block, y = reclicks_mean), alpha = 0.4, colour = "grey", position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = reclicks_pal_fill) +
  scale_colour_manual(values = reclicks_pal_colour) +
  ylim(0, 18) +
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
    y = "Reclicks"
  )

ggsave(
  "reclicks_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

ggsave(
  "reclcicks_dif_ttest.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)


col_TE |>
  ggplot() +
  stat_summary(
    aes(x = block, y = TE_mean),
    fun = "mean", geom = "col", fill = "grey", colour = "grey"
    ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub, x = block, y = TE_mean),
    position = position_dodge(width = 0.5), shape = 21, size = 3.5
  ) +
  stat_summary(
    aes(x = block, y = TE_mean),
    fun = "mean", geom = "point", fill = "black", colour = "black", size = 2.5
    ) +
  geom_errorbar(data = TE_cis, aes(x = block, ymin = cis_lower, ymax = cis_upper), color = "black", width = 0, size = 1.3)  +
  #stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0, size = 1.3) +
  geom_line(aes(group = sub, x = block, y = TE_mean), alpha = 0.4, colour = "grey", position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = TE_pal_fill) +
  scale_colour_manual(values = TE_pal_colour) +
  ylim(0, 1) +
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
    y = "TE"
  )

ggsave(
  "TE_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)

ggsave(
  "TE_dif_ttest.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)
