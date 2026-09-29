#CI = Mean +/- t(1-alpha/2, n-1) * (SD / sqrt(n)) * sqrt(c / (c-1))
#for alpha = 0.05
#where c = number of within participants conditions

cm_upper <- function(mean, n, SD, c) {
  mean + qt(1 - 0.05/2, n - 1) * ((SD/sqrt(n) * sqrt(c/(c-1))))
}

cm_lower <- function(mean, n, SD, c) {
  mean - qt(1 - 0.05/2, n - 1) * ((SD/sqrt(n) * sqrt(c/(c-1))))
}
