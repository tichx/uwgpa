library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("data/uw_courses.csv", stringsAsFactors = F)

A_classes <- df %>%
  group_by(dept)