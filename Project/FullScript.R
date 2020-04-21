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
library(lubridate)


source("Fredr_Script.R")

 {r Wrangling the data and changing names}
set.seed(23)

colnames(data)[2:6] <- c("nonfarm", "avg_week_hrs", "avg_hr_earnings",
                         "avg_week_earnings", "all_goods")
data["tot_week_earnings"] <- data[2]*data[5]
 

 {r Differencing the variables}
data['diff.nonfarm'] <-  difference(data$nonfarm, differences = 2)
data['diff.avg_week_hrs'] <- difference(data$avg_week_hrs,  differences = 1) 
data['diff.avg_hr_earn'] <-  difference(data$avg_hr_earnings, differences = 1)
data['diff.avg_w_earn'] <- difference(data$avg_week_earnings, differences = 1) 
data['diff.all_goods'] <- difference(data$all_goods, differences = 1) 
data['diff.tot_w_earn'] <- difference(data$tot_week_earnings, differences = 1) 
 


 {r Monthly dummy variables}
# Make Dummy Vars for Month 
data_ts <- data %>% 
  mutate(YearMonth = yearmonth(as.character(data$date))) %>%
  as_tsibble(index = YearMonth)

data_ts <- data_ts %>% 
  mutate(month = month(date)) %>% 
  mutate(jan = (month == 1),
         feb = (month == 2),
         mar = (month == 3),
         apr = (month == 4),
         may = (month == 5),
         jun = (month == 6),
         jul = (month == 7),
         aug = (month == 8),
         sep = (month == 9),
         oct = (month == 10),
         nov = (month == 11),
         dec = (month == 12))
 


  
summary(data[2:5]) %>%
  kable(format = "latex") %>% 
  row_spec(0,bold=TRUE) %>% 
  kable_styling(full_width = FALSE, position = "center", latex_options = c("striped", "HOLD_position"))

summary(data[6:7]) %>%
  kable(format = "latex") %>% 
  row_spec(0,bold=TRUE) %>% 
  kable_styling(full_width = FALSE, position = "center", latex_options = c("striped", "HOLD_position"))
 



 {r, fig.height = 8, fig.width = 9}
t1 <- data_ts %>% autoplot(nonfarm, ts.colour = "red") +
  xlab("Date") +
  ylab("Nonfarm")
t3 <- data_ts %>% autoplot(avg_week_hrs) +
  xlab("Date") +
  ylab("Avg Weekly Hours")
t4 <- data_ts %>% autoplot(avg_hr_earnings) +
  xlab("Date") +
  ylab("Avg Hourly Earnings")
t5 <- data_ts %>% autoplot(avg_week_earnings)+
  xlab("Date") +
  ylab("Avg Weekly Earnings")
t6 <- data_ts %>% autoplot(all_goods)+
  xlab("Date") +
  ylab("All Goods Produced")
t7 <- data_ts %>% autoplot(tot_week_earnings)+
  xlab("Date") +
  ylab("Total Weekly Earnings")

(t1 + t3) /
  (t4 + t5) /
  (t6 + t7) 
 


 {r, fig.height = 6, fig.width = 10}

ac_data <- data_ts %>% na.omit()

a1 <- autoplot(acf(ac_data$nonfarm, plot = FALSE), main = "Nonfarm") + theme(axis.title.x = element_blank())
a3 <- autoplot(acf(ac_data$avg_week_hrs, plot = FALSE), main = "Weekly Hours")  + theme(axis.title.y = element_blank(),
                                                                                        axis.title.x = element_blank())
a4 <- autoplot(acf(ac_data$avg_hr_earnings, plot = FALSE), main = "Hourly Earnings") + theme(axis.title.y = element_blank(),
                                                                                             axis.title.x = element_blank())
a5 <- autoplot(acf(ac_data$avg_week_earnings, plot = FALSE), main = "Weekly Earnings")  
a6 <- autoplot(acf(ac_data$all_goods, plot = FALSE), main = "All Goods Produced ")  + theme(axis.title.y = element_blank())
a7 <- autoplot(acf(ac_data$tot_week_earnings, plot = FALSE), main = "Total Weekly Earnings")+ theme(axis.title.y = element_blank())


wrap_plots(a1, a3 , a4, a5, a6, a7, ncol = 3, byrow = TRUE)
 


 {r, fig.height = 7, fig.width = 10}
p1 <- autoplot(pacf(ac_data$nonfarm, plot = FALSE), main = "Nonfarm") + theme(axis.title.x = element_blank())
p3 <- autoplot(pacf(ac_data$avg_week_hrs, plot = FALSE), main = "Weekly Hours")  + theme(axis.title.y = element_blank(),
                                                                                         axis.title.x = element_blank())
p4 <- autoplot(pacf(ac_data$avg_hr_earnings, plot = FALSE), main = "Hourly Earnings")+ theme(axis.title.y = element_blank(),
                                                                                             axis.title.x = element_blank())
p5 <- autoplot(pacf(ac_data$avg_week_earnings, plot = FALSE), main = "Weekly Earnings") 
p6 <- autoplot(pacf(ac_data$all_goods, plot = FALSE), main = "All Goods Produced ")  + theme(axis.title.y = element_blank())
p7 <- autoplot(pacf(ac_data$tot_week_earnings, plot = FALSE), main = "Total Weekly Earnings") + theme(axis.title.y = element_blank())


wrap_plots(p1, p3 , p4, p5, p6, p7, ncol = 3, byrow = TRUE)
 


 {r AC and PAC of the differenced variables, fig.height = 6, fig.width = 10}
# Autoplots for AC
da1 <- autoplot(acf(ac_data$diff.nonfarm, plot = FALSE), main = "Nonfarm") + theme(axis.title.x = element_blank())
da3 <- autoplot(acf(ac_data$diff.avg_week_hrs, plot = FALSE), main = "Weekly Hours")  + theme(axis.title.y = element_blank(),
                                                                                              axis.title.x = element_blank())
da4 <- autoplot(acf(ac_data$diff.avg_hr_earn, plot = FALSE), main = "Hourly Earnings") + theme(axis.title.y = element_blank(),
                                                                                               axis.title.x = element_blank())
da5 <- autoplot(acf(ac_data$diff.avg_w_earn, plot = FALSE), main = "Weekly Earnings")  
da6 <- autoplot(acf(ac_data$diff.all_goods, plot = FALSE), main = "All Goods Produced ")  + theme(axis.title.y = element_blank())
da7 <- autoplot(acf(ac_data$diff.tot_w_earn, plot = FALSE), main = "Total Weekly Earnings")+ theme(axis.title.y = element_blank())


wrap_plots(da1, da3, da4, da5, da6, da7, ncol = 3, byrow = TRUE)
 

 {r, fig.height = 7, fig.width = 10}
#Autoplots for PAC
dp1 <- autoplot(pacf(ac_data$diff.nonfarm, plot = FALSE), main = "Nonfarm") + theme(axis.title.x = element_blank())
dp3 <- autoplot(pacf(ac_data$diff.avg_week_hrs, plot = FALSE), main = "Weekly Hours")  + theme(axis.title.y = element_blank(),
                                                                                               axis.title.x = element_blank())
dp4 <- autoplot(pacf(ac_data$diff.avg_hr_earn, plot = FALSE), main = "Hourly Earnings") + theme(axis.title.y = element_blank(),
                                                                                                axis.title.x = element_blank())
dp5 <- autoplot(pacf(ac_data$diff.avg_w_earn, plot = FALSE), main = "Weekly Earnings")  
dp6 <- autoplot(pacf(ac_data$diff.all_goods, plot = FALSE), main = "All Goods Produced ")  + theme(axis.title.y = element_blank())
dp7 <- autoplot(pacf(ac_data$diff.tot_w_earn, plot = FALSE), main = "Total Weekly Earnings")+ theme(axis.title.y = element_blank())


wrap_plots(dp1, dp3, dp4, dp5, dp6, dp7, ncol = 3, byrow = TRUE)
 


 {r Augmented Dickey Fuller Test, results=FALSE}
tseries::adf.test(c(ac_data$diff.nonfarm, ac_data$diff.avg_week_hrs, 
                    ac_data$diff.avg_hr_earn, ac_data$diff.avg_w_earn, 
                    ac_data$diff.all_goods, ac_data$diff.tot_w_earn,
                    ac_data$YearMonth),
                  alternative = "stationary",
                  k = 13)
 



  
#FOR THE GENETIC ALGORITHM
## not very robust but it gets the job done.
## num_of_lags is set up, requires your main dataframe be a time series object labelled "data_ts" 
## remove lag_df after each chunk run.

# num_of_lags <- 12
# source("GA_script.R")
# 
# rm(lag_df)

 


 {r Train Test Set split}
#training test set split
train_set <- data_ts[1:348,] # only up to 2018

test_set_2019 <- data_ts[349:360,] #Includes only the year 2019
 

 {r glmnet grid}
#GLMNET grid
#grid for glmnet
fit_grid2 <- expand.grid(alpha = 0:1, 
                         lambda = seq(-5, 5, length = 1000))
 

 {r Control for glmnet}
#Control with cross validation 
control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
 


 {r Training Glmnet}
#porque no los dos? 
#training
glmnet_fit <- train(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 3) + lag(diff.nonfarm, 4) + 
                      lag(diff.nonfarm, 5) +
                      lag(diff.nonfarm, 6) + lag(diff.nonfarm, 7) + lag(diff.nonfarm, 8) + lag(diff.nonfarm, 9) + lag(diff.nonfarm, 10) +
                      lag(diff.nonfarm, 11) + lag(diff.nonfarm, 12) + 
                      
                      lag(diff.avg_week_hrs, 1) + lag(diff.avg_week_hrs, 2) + lag(diff.avg_week_hrs, 3) + lag(diff.avg_week_hrs, 4) + 
                      lag(diff.avg_week_hrs, 5) + lag(diff.avg_week_hrs, 6) + lag(diff.avg_week_hrs, 7) + lag(diff.avg_week_hrs, 8) +
                      lag(diff.avg_week_hrs, 9) + lag(diff.avg_week_hrs, 10) + lag(diff.avg_week_hrs, 11) + lag(diff.avg_week_hrs, 12) +
                      
                      lag(diff.avg_hr_earn, 1) + lag(diff.avg_hr_earn, 2) + lag(diff.avg_hr_earn, 3) + lag(diff.avg_hr_earn, 4) + 
                      lag(diff.avg_hr_earn, 5) + lag(diff.avg_hr_earn, 6) + lag(diff.avg_hr_earn, 7) + lag(diff.avg_hr_earn, 8)+
                      lag(diff.avg_hr_earn, 9) + lag(diff.avg_hr_earn, 10) + lag(diff.avg_hr_earn, 11) + lag(diff.avg_hr_earn, 12)+
                      
                      lag(diff.avg_w_earn, 1)+ lag(diff.avg_w_earn, 12) + lag(diff.avg_w_earn, 3) + lag(diff.avg_w_earn, 4) +
                      lag(diff.avg_w_earn, 5) + lag(diff.avg_w_earn, 6) + lag(diff.avg_w_earn, 7) + lag(diff.avg_w_earn, 8) +
                      lag(diff.avg_w_earn, 9) + lag(diff.avg_w_earn, 10) + lag(diff.avg_w_earn, 11) + lag(diff.avg_w_earn, 12)+
                      
                      lag(diff.all_goods, 1)+ lag(diff.all_goods, 12) + lag(diff.all_goods, 3) + lag(diff.all_goods, 4) +
                      lag(diff.all_goods, 5) + lag(diff.all_goods, 6) + lag(diff.all_goods, 7) + lag(diff.all_goods, 8) +
                      lag(diff.all_goods, 9) + lag(diff.all_goods, 10) + lag(diff.all_goods, 11) + lag(diff.all_goods, 12) +
                      
                      lag(diff.tot_w_earn, 1)+ lag(diff.tot_w_earn, 12) + lag(diff.tot_w_earn, 3) + lag(diff.tot_w_earn, 4) +
                      lag(diff.tot_w_earn, 5) + lag(diff.tot_w_earn, 6) + lag(diff.tot_w_earn, 7) + lag(diff.tot_w_earn, 8) +
                      lag(diff.tot_w_earn, 9) + lag(diff.tot_w_earn, 10) + lag(diff.tot_w_earn, 11) + lag(diff.tot_w_earn, 12) + YearMonth +
                      jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec, 
                    data = train_set,
                    method = "glmnet", 
                    tuneGrid = fit_grid2,
                    trControl = control,
                    na.action = na.exclude,
                    preProcess = c("center", "scale"))
 


  
best_lam <- glmnet_fit$bestTune$lambda
 


 {r, comment = NA}
net_frame <- as.data.frame(as.matrix(predict(glmnet_fit$finalModel, s = best_lam, type = "coefficients")))
net_frame[,1][net_frame[,1] == 0] <- "-"

net_frame %>% 
  kable(format = "latex", longtable = TRUE) %>% 
  kable_styling(position = "center", latex_options = "striped")
 



 {r Predicting 2019 with the models}
prelim_model <- train_set %>% model(
  intuition = TSLM(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 12) +
                     lag(diff.avg_week_hrs, 1) + lag(diff.avg_week_hrs, 2) +
                     lag(diff.avg_hr_earn, 1) + lag(diff.avg_hr_earn, 2) + 
                     lag(diff.avg_w_earn, 1) + lag(diff.avg_w_earn, 2) +
                     lag(diff.all_goods, 1) + lag(diff.all_goods, 2) +
                     lag(diff.tot_w_earn, 1) + lag(diff.tot_w_earn, 2) + 
                     jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + YearMonth),
  
  GA_model = TSLM(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 3) + lag(diff.nonfarm, 4) +
                    lag(diff.nonfarm, 5) + lag(diff.nonfarm, 6) + lag(diff.nonfarm, 7) + lag(diff.nonfarm, 8) + 
                    lag(diff.nonfarm, 10) + lag(diff.nonfarm, 11) + lag(diff.nonfarm, 12) + 
                    lag(diff.avg_week_hrs, 2) + lag(diff.avg_week_hrs, 7) + lag(diff.avg_week_hrs, 8) +
                    lag(diff.avg_week_hrs, 12) + 
                    lag(diff.avg_w_earn, 2) + lag(diff.avg_w_earn, 3) + lag(diff.avg_w_earn, 4) +
                    lag(diff.avg_w_earn, 5) + lag(diff.avg_w_earn, 6) + lag(diff.avg_w_earn, 7) + 
                    lag(diff.tot_w_earn, 3) + lag(diff.tot_w_earn, 5) + lag(diff.tot_w_earn, 3) +
                    lag(diff.tot_w_earn, 8) + 
                    lag(diff.avg_hr_earn, 6) + lag(diff.tot_w_earn, 7) + lag(diff.tot_w_earn, 12) +
                    lag(diff.all_goods, 3) + lag(diff.all_goods, 4) + lag(diff.all_goods, 5) + lag(diff.all_goods, 6) +
                    lag(diff.all_goods, 12) + YearMonth +
                    jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec),
  
  glmnet_mod = TSLM(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 16) + lag(diff.nonfarm, 10) +
                      lag(diff.nonfarm, 12) +
                      lag(diff.avg_week_hrs, 2) + lag(diff.avg_week_hrs, 4) + lag(diff.avg_week_hrs, 6) +
                      lag(diff.avg_week_hrs, 12) +
                      lag(diff.avg_hr_earn, 1) + lag(diff.avg_hr_earn, 7) + lag(diff.avg_hr_earn, 8) + 
                      lag(diff.avg_hr_earn, 11) + lag(diff.avg_hr_earn, 12) +
                      lag(diff.avg_w_earn, 5) +
                      lag(diff.all_goods, 1) + lag(diff.all_goods, 4) + lag(diff.all_goods, 5) + lag(diff.all_goods, 6) +
                      lag(diff.all_goods, 9) +
                      lag(diff.tot_w_earn, 1) + YearMonth + 
                      jan + feb + may + jun + aug + sep + oct + nov)
)

init_fc <- forecast(prelim_model, new_data = test_set_2019)
 

  
fc_accuracy <- accuracy(init_fc, test_set_2019,
                        measures = list(
                          point_accuracy_measures,
                          interval_accuracy_measures,
                          distribution_accuracy_measures
                        )
)

#creating an accuracy table
fc_acc_table <- fc_accuracy %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE),
    MAE = mean(MAE),
    Winkler = mean(winkler),
    CRPS = mean(CRPS)
  ) %>%
  arrange(RMSE) 

fc_acc_table %>% kable()

 


  
#Plotting Predictions
autoplot(init_fc, data = data_ts[300:360,], level = NULL) +
  geom_point() +
  ggtitle("Model selection ") +
  xlab("Months") +
  guides(colour = guide_legend(title = "Forecast"))
 




# Baseline Rolling window model

  
Bcontrol <- trainControl(method = "timeslice",
                         initialWindow = 60,
                         horizon = 1,
                         fixedWindow = FALSE,
                         savePredictions = TRUE)

baseline <- train(diff.nonfarm ~lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 3) + lag(diff.nonfarm, 4) + 
                    lag(diff.nonfarm, 5) +
                    lag(diff.nonfarm, 6) + lag(diff.nonfarm, 7) + lag(diff.nonfarm, 8) + lag(diff.nonfarm, 9) + lag(diff.nonfarm, 10) +
                    lag(diff.nonfarm, 11) + lag(diff.nonfarm, 12) + 
                    lag(diff.avg_w_earn, 1)+ lag(diff.avg_w_earn, 12) + lag(diff.avg_w_earn, 3) + lag(diff.avg_w_earn, 4) +
                    lag(diff.avg_w_earn, 5) + lag(diff.avg_w_earn, 6) + lag(diff.avg_w_earn, 7) + lag(diff.avg_w_earn, 8) +
                    lag(diff.avg_w_earn, 9) + lag(diff.avg_w_earn, 10) + lag(diff.avg_w_earn, 11) + lag(diff.avg_w_earn, 12) +
                    feb + mar + apr + may + jun + jul + aug + sep+ oct + nov + dec,
                  method = "lm",
                  data = data_ts,
                  trControl = Bcontrol,
                  na.action = na.exclude)


 

  
predict_baseline <- data.frame(baseline['pred'])
sqrt(mean(predict_baseline[85, 1] - predict_baseline[85, 2])^2)
 




  
Tcontrol <- trainControl(method = "timeslice",
                         initialWindow = 60,
                         horizon = 1,
                         fixedWindow = FALSE,
                         savePredictions = TRUE)
 


 {r Rolling window 1 Step GA}
ga_roll <- train(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 12) +
                   lag(diff.avg_week_hrs, 1) + lag(diff.avg_week_hrs, 2) +
                   lag(diff.avg_hr_earn, 1) + lag(diff.avg_hr_earn, 2) + 
                   lag(diff.avg_w_earn, 1) + lag(diff.avg_w_earn, 2) +
                   lag(diff.all_goods, 1) + lag(diff.all_goods, 2) +
                   lag(diff.tot_w_earn, 1) + lag(diff.tot_w_earn, 2) + 
                   jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + YearMonth,
                 method = 'lm',
                 trControl = Tcontrol,
                 data = data_ts,
                 na.action = na.exclude)

 

  
predict_ga <- data.frame(ga_roll['pred'])
ga_feb <- sqrt(mean(predict_ga[95, 1] - predict_ga[95, 2])^2)
 


 {r Rolling window 1 Step GLMNET}
glmnet_roll <- train(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 16) + lag(diff.nonfarm, 10) +
                       lag(diff.nonfarm, 12) +
                       lag(diff.avg_week_hrs, 2) + lag(diff.avg_week_hrs, 4) + lag(diff.avg_week_hrs, 6) +
                       lag(diff.avg_week_hrs, 12) +
                       lag(diff.avg_hr_earn, 1) + lag(diff.avg_hr_earn, 7) + lag(diff.avg_hr_earn, 8) + 
                       lag(diff.avg_hr_earn, 11) + lag(diff.avg_hr_earn, 12) +
                       lag(diff.avg_w_earn, 5) +
                       lag(diff.all_goods, 1) + lag(diff.all_goods, 4) + lag(diff.all_goods, 5) + lag(diff.all_goods, 6) +
                       lag(diff.all_goods, 9) +
                       lag(diff.tot_w_earn, 1) + YearMonth + 
                       jan + feb + may + jun + aug + sep + oct + nov,
                     method = "lm",
                     trControl = Tcontrol,
                     data = data_ts,
                     na.action = na.exclude)
 

  
predict_glmnet <- data.frame(glmnet_roll['pred'])
glmnet_feb <- sqrt(mean(predict_glmnet[85, 1] - predict_glmnet[85, 2])^2)
 

 {r Rolling window 1 Step Intuition Model}
intuit_roll <- train(diff.nonfarm ~ lag(diff.nonfarm, 1) + lag(diff.nonfarm, 2) + lag(diff.nonfarm, 12) +
                       lag(diff.avg_week_hrs, 1) + lag(diff.avg_week_hrs, 2) +
                       lag(diff.avg_hr_earn, 1) + lag(diff.avg_hr_earn, 2) + 
                       lag(diff.avg_w_earn, 1) + lag(diff.avg_w_earn, 2) +
                       lag(diff.all_goods, 1) + lag(diff.all_goods, 2) +
                       lag(diff.tot_w_earn, 1) + lag(diff.tot_w_earn, 2) + 
                       jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec,
                     method = "lm",
                     trControl = Tcontrol,
                     data = data_ts,
                     na.action = na.exclude)


 

  
predict_intuit <- data.frame(intuit_roll['pred'])
intuit_feb <- sqrt(mean(predict_intuit[95, 1] - predict_intuit[95, 2])^2)
 

### Results 

  
fc_acc_table['Feb RMSE'] <- c(glmnet_feb, ga_feb, intuit_feb)
fc_acc_table 
 