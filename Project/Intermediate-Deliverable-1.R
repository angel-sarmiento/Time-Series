## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, comments = NA, message = FALSE)


## ---- echo = FALSE, warning = FALSE, comments = NA----------------------------------------------------------------------------------------
#library Import
library(caret)
library(tidyverse)

library(fable)
library(feasts)
library(fredr)
library(tsibble)
library(patchwork)
library(kableExtra)
library(ggfortify)

set.seed(23)
data <-  read_csv("dataset.csv")  %>% na.pass()

colnames(data)[2:7] <- c("nonfarm", "eci", "avg_week_hrs", "avg_hr_earnings",
                         "avg_week_earnings", "all_goods")
data["tot_week_earnings"] <- data[5]*data[2]


## ---- warning = FALSE, comments = NA, message = FALSE-------------------------------------------------------------------------------------
summary(data[2:5]) %>%
  kable(format = "latex") %>% 
  row_spec(0,bold=TRUE) %>% 
  kable_styling(full_width = FALSE, position = "center", latex_options = c("striped", "HOLD_position"))

summary(data[6:7]) %>%
  kable(format = "latex") %>% 
  row_spec(0,bold=TRUE) %>% 
  kable_styling(full_width = FALSE, position = "center", latex_options = c("striped", "HOLD_position"))


## -----------------------------------------------------------------------------------------------------------------------------------------
dataset <- data %>% as_tsibble()
#creating the months variable
dataset <- dataset %>%
  mutate(Month = lubridate::month(DATE)) %>%
  index_by(Month) 


## ---- fig.height = 8, fig.width = 9-------------------------------------------------------------------------------------------------------
t1 <- dataset %>% autoplot(nonfarm, ts.colour = "red") +
  xlab("Date") +
  ylab("Nonfarm")
t2 <- dataset %>% autoplot(eci) +
  xlab("Date") +
  ylab("ECI")
t3 <- dataset %>% autoplot(avg_week_hrs) +
  xlab("Date") +
  ylab("Avg Weekly Hours")
t4 <- dataset %>% autoplot(avg_hr_earnings) +
  xlab("Date") +
  ylab("Avg Hourly Earnings")
t5 <- dataset %>% autoplot(avg_week_earnings)+
  xlab("Date") +
  ylab("Avg Weekly Earnings")
t6 <- dataset %>% autoplot(all_goods)+
  xlab("Date") +
  ylab("All Goods Produced")
t7 <- dataset %>% autoplot(tot_week_earnings)+
  xlab("Date") +
  ylab("Total Weekly Earnings")

(t1 + t2) /
(t3 + t4) /
(t5 + t6) /
(t7)


## ---- fig.height = 6, fig.width = 10------------------------------------------------------------------------------------------------------

ac_data <- dataset %>% na.omit()

a1 <- autoplot(acf(ac_data$nonfarm, plot = FALSE), main = "Nonfarm") 
a2 <- autoplot(acf(ac_data$eci, plot = FALSE), main = "ECI") + theme(axis.title.y = element_blank())
a3 <- autoplot(acf(ac_data$avg_week_hrs, plot = FALSE), main = "Weekly Hours")  + theme(axis.title.y = element_blank())
a4 <- autoplot(acf(ac_data$avg_hr_earnings, plot = FALSE), main = "Hourly Earnings")
a5 <- autoplot(acf(ac_data$avg_week_earnings, plot = FALSE), main = "Weekly Earnings")  + theme(axis.title.y = element_blank())
a6 <- autoplot(acf(ac_data$all_goods, plot = FALSE), main = "All Goods Produced ")  + theme(axis.title.y = element_blank())
a7 <- autoplot(acf(ac_data$tot_week_earnings, plot = FALSE), main = "Total Weekly Earnings")


wrap_plots(a1, a2, a3 , a4, a5, a6, a7, ncol = 3, byrow = TRUE)


## ---- fig.height = 7, fig.width = 10------------------------------------------------------------------------------------------------------
p1 <- autoplot(pacf(ac_data$nonfarm, plot = FALSE), main = "Nonfarm") 
p2 <- autoplot(pacf(ac_data$eci, plot = FALSE), main = "ECI") + theme(axis.title.y = element_blank())
p3 <- autoplot(pacf(ac_data$avg_week_hrs, plot = FALSE), main = "Weekly Hours")  + theme(axis.title.y = element_blank())
p4 <- autoplot(pacf(ac_data$avg_hr_earnings, plot = FALSE), main = "Hourly Earnings")
p5 <- autoplot(pacf(ac_data$avg_week_earnings, plot = FALSE), main = "Weekly Earnings")  + theme(axis.title.y = element_blank())
p6 <- autoplot(pacf(ac_data$all_goods, plot = FALSE), main = "All Goods Produced ")  + theme(axis.title.y = element_blank())
p7 <- autoplot(pacf(ac_data$tot_week_earnings, plot = FALSE), main = "Total Weekly Earnings")


wrap_plots(p1, p2, p3 , p4, p5, p6, p7, ncol = 3, byrow = TRUE)

