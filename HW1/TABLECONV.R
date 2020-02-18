library(tidyverse)
setwd("/Users/angelsarmiento/Documents/Graduate/First Year/Time Series/STATA/HW1")



fl_nonfarm <- read_csv("fl_nonfarm.csv") %>% replace_na(.)
fl_lf <- read_csv("fl_lf.csv") %>% replace_na(.)
fl_bp <- read_csv("fl_bp.csv") %>% replace_na(.)
us_epr_25to54 <- read_csv("us_epr_25to54.csv") %>% replace_na(.)

mergecol <- c('DATE')

dataset <- merge(fl_nonfarm, fl_lf, by = mergecol, all.x = TRUE) %>% replace_na(.)
dataset2 <-  merge(us_epr_25to54, fl_bp, by = mergecol, all.x = TRUE) %>% replace_na(.)

assembly <- merge(dataset, dataset2, by = mergecol, all.x = TRUE) %>% replace_na(.)

names <- c("DATE","fl_nonfarm", "fl_lf", "us_epr_25to54", "fl_bp")
colnames(assembly) <- names

write.csv(assembly, file = "data.csv")