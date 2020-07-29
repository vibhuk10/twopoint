library(tidyverse)
data <- read_csv("data-raw/NFL_pbp_2009-2019.csv")
twopoint <- read_csv("data-clean/twopoint_2009-2019.csv")
extrapoint <- read_csv("data-clean/extrapoint_2009-2019.csv")