library(caret)
library(tidyverse)

library(patchwork)
library(kableExtra)
library(tsibble)
library(lubridate)
library(fable)
library(fredr)
set.seed(33455)



fredr_set_key("91de61ed607a6478d01c35d9a903017c")


test <- if (requireNamespace("purrr", quietly = TRUE)) {
  
  library(purrr)
  purrr::map_dfr(c('FLNAN', 'FLLFN', 'LREM25TTUSM156N', 'FLBPPRIV'), fredr)
  
  # Using purrr::pmap_dfr() allows you to use varying optional parameters
  params <- list(
    series_id = c('FLNAN', 'FLLFN', 'LREM25TTUSM156N', 'FLBPPRIV')
  )
  
  purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x)
  )
  
} 
data <- pivot_wider(test, 
                    names_from = series_id,
                    values_from = value)

#Reshaping and reformatting
data[6:9] <- log(data[2:5]) 

colnames(data)[2:5] <- c("fl_nonfarm", "fl_lf", "us_epr", "fl_bp")
colnames(data)[6:9] <- c("ln_fl_nonfarm", "ln_fl_lf", "ln_us_epr", "ln_fl_bp")
data <- data[613:974,]

#now to difference the data
data['d.fl_nonfarm'] <- difference(data$ln_fl_nonfarm, differences = 1)
data['d.fl_lf'] <- difference(data$ln_fl_lf,  differences = 1) 
data['d.fl_bp'] <- difference(data$ln_fl_bp, differences = 1)
data['d.us_epr'] <- difference(data$ln_us_epr, differences = 1) 

#variables for just the difference, for the end forecasts of the actual nonfarm employment
data['diff.fl_nonfarm'] <-  difference(data$fl_nonfarm, differences = 1)
data['diff.fl_lf'] <- difference(data$fl_lf,  differences = 1) 
data['diff.fl_bp'] <-  difference(data$fl_bp, differences = 1)
data['diff.us_epr'] <- difference(data$us_epr, differences = 1) 

# Make Dummy Vars for Month
data_ts <- data %>% 
  mutate(YearMonth = yearmonth(as.character(data$date))) %>%
  as_tsibble(index = YearMonth)

# data_ts <- data  %>% 
#   as_tsibble(index = month)


train_set <- data_ts[1:348,] 

test_set <- data_ts[349:360,]


#Defining predictors for Genetic algorithm 
#getting a bunch of lags
lag_predicts <-  seq(12)
lag_names <- paste("lag", formatC(lag_predicts, width = nchar(max(lag_predicts)), flag = "0"), sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lag_predicts, ")"), lag_names)

lag_df <- data_ts %>% mutate_at(vars(d.fl_nonfarm, d.fl_lf, d.fl_bp, d.us_epr), funs_(lag_functions))

b_df <- lag_df[13:362,]
b_df[,1:5] <- NULL
b_df[,2:4] <- NULL

# ctrl <- gafsControl(functions = caretGA, 
#                     method = "cv", 
#                     number = 5, 
#                     metric = c(internal = "RMSE", external = "RMSE"),
#                     maximize = FALSE,
#                     allowParallel = TRUE,
#                     genParallel = TRUE,
#                     verbose = TRUE)


# #setting up parallel processing
# library(doParallel)
# cl <- makePSOCKcluster(11)
# registerDoParallel(cl)
# 
# library(recipes)
# #setting up a recipe to get the variables needed for gafs
# 
# # not_inc <- list(date, ln_fl_nonfarm, ln_fl_lf, ln_fl_bp, ln_us_epr, d.fl_nonfarm, d.fl_lf, d.fl_bp, d.us_epr)
# lag_rec <- b_df %>% 
#   recipe(d.fl_nonfarm~.) 
# 
# 
# 
# obj <- gafs(lag_rec,
#             iters = 100,
#             popSize = 20,
#             gafsControl = ctrl,
#             data = b_df,
#             elite = 3,
#             ## Now pass options to `train`
#             
#             method = "lm",
#             na.action = na.omit,
#             trControl = trainControl(method = "cv", number = 5,
#                                      allowParallel = TRUE)); beepr::beep("coin")
# 
# stopCluster(cl)



knitr::include_graphics("iterations_ga.png") 



RMSE <- c(0.00374, 0.00396, 0.00364, 0.004840)
OOS_RMSE <- c(0.00538, 0.003969, 0.002206, 0.0023330)
Rsquared <- c(0.862, 0.8411, 0.869, 0.7704)
N_of_Vars <- c(22, 32, 34, 26)

model_select <- data.frame(RMSE, OOS_RMSE, Rsquared, N_of_Vars)

rows <- c("Model 1", "Model 2", "Model 3", "Prev_Best Model")
rownames(model_select)[1:4] <- rows

model_select %>% kable(format = "latex") %>% 
  kable_styling(position = "center", latex_options = "striped")



model_ardls <- train_set %>% model(
  prev_ardl = TSLM(d.fl_nonfarm ~ lag(d.fl_nonfarm,1) + lag(d.fl_nonfarm,2) + lag(d.fl_nonfarm,3) + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) +
                     lag(d.fl_nonfarm,6) + lag(d.fl_nonfarm,7) + lag(d.fl_nonfarm,8) + lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11)+
                     lag(d.fl_nonfarm,12) + lag(d.fl_lf,1) + lag(d.fl_lf,2) +
                     lag(d.us_epr,1) + lag(d.us_epr,2) +
                     lag(d.fl_bp,1) + lag(d.fl_bp,2)  +
                     YearMonth),
  ga_ardl = TSLM(d.fl_nonfarm ~ lag(d.fl_nonfarm,1)  + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) + lag(d.fl_nonfarm,8) + 
                   lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11) + lag(d.fl_nonfarm,12) + 
                   lag(d.fl_lf,1) + lag(d.fl_lf,3) + lag(d.fl_lf,4) + lag(d.fl_lf,7) + lag(d.fl_lf,8) + lag(d.fl_lf,11) +
                   lag(d.us_epr,1) + lag(d.us_epr,2) + lag(d.us_epr,3) + lag(d.us_epr,4) + lag(d.us_epr,5) + 
                   lag(d.us_epr,6) + lag(d.us_epr,7) + lag(d.us_epr,8) + lag(d.us_epr,9) + lag(d.us_epr,10) +
                   lag(d.fl_bp,1) + lag(d.fl_bp,2)  + lag(d.fl_bp,3) + lag(d.fl_bp,4)  + lag(d.fl_bp,5) +
                   lag(d.fl_bp,6) + lag(d.fl_bp,7)  + lag(d.fl_bp,8)  + lag(d.fl_bp,9)  + lag(d.fl_bp,12) +
                   YearMonth)
)
init_fc <- forecast(model_ardls, new_data = test_set)


#Plotting Predictions
autoplot(init_fc, data = data_ts[300:360,], level = NULL) +
  ggtitle("Current vs. Previous Best models") +
  xlab("Months") +
  guides(colour = guide_legend(title = "Forecast"))


tscontrol <- trainControl(method = "timeslice", 
                          initialWindow = 36,
                          horizon = 12,
                          fixedWindow = FALSE,
                          savePredictions = TRUE)


rw_model <- train(d.fl_nonfarm ~ lag(d.fl_nonfarm,1)  + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) + lag(d.fl_nonfarm,8) + 
                    lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11) + lag(d.fl_nonfarm,12) + 
                    lag(d.fl_lf,1) + lag(d.fl_lf,3) + lag(d.fl_lf,4) + lag(d.fl_lf,7) + lag(d.fl_lf,8) + lag(d.fl_lf,11) +
                    lag(d.us_epr,1) + lag(d.us_epr,2) + lag(d.us_epr,3) + lag(d.us_epr,4) + lag(d.us_epr,5) + 
                    lag(d.us_epr,6) + lag(d.us_epr,7) + lag(d.us_epr,8) + lag(d.us_epr,9) + lag(d.us_epr,10) +
                    lag(d.fl_bp,1) + lag(d.fl_bp,2)  + lag(d.fl_bp,3) + lag(d.fl_bp,4)  + lag(d.fl_bp,5) +
                    lag(d.fl_bp,6) + lag(d.fl_bp,7)  + lag(d.fl_bp,8)  + lag(d.fl_bp,9)  + lag(d.fl_bp,12) +
                    YearMonth, 
                  data = data_ts[1:360,],
                  method = "lm",
                  trControl = tscontrol,
                  na.action = na.exclude)

rw_model


ggplot(rw_model$pred, aes(x = obs, y = pred)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", color = "black") +
  xlab("Observations") +
  ylab("Predictions") +
  ggtitle("Predictions vs Observations of model trained with Rolling Window")


rw_model_2 <- train_set %>% model(
  ga_ardl = TSLM(d.fl_nonfarm ~ lag(d.fl_nonfarm,1)  + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) + lag(d.fl_nonfarm,8) + 
                   lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11) + lag(d.fl_nonfarm,12) + 
                   lag(d.fl_lf,1) + lag(d.fl_lf,3) + lag(d.fl_lf,4) + lag(d.fl_lf,7) + lag(d.fl_lf,8) + lag(d.fl_lf,11) +
                   lag(d.us_epr,1) + lag(d.us_epr,2) + lag(d.us_epr,3) + lag(d.us_epr,4) + lag(d.us_epr,5) + 
                   lag(d.us_epr,6) + lag(d.us_epr,7) + lag(d.us_epr,8) + lag(d.us_epr,9) + lag(d.us_epr,10) +
                   lag(d.fl_bp,1) + lag(d.fl_bp,2)  + lag(d.fl_bp,3) + lag(d.fl_bp,4)  + lag(d.fl_bp,5) +
                   lag(d.fl_bp,6) + lag(d.fl_bp,7)  + lag(d.fl_bp,8)  + lag(d.fl_bp,9)  + lag(d.fl_bp,12) +
                   YearMonth)
)

ga_fc <- forecast(rw_model_2, new_data = test_set)

#Plotting Predictions
autoplot(ga_fc, data = test_set, level = c(95, 97.5, 99.8), colour = "darkblue") +
  ggtitle("Forecasts for Nonfarm Employment from GA model: 2019") +
  xlab("Months") +
  guides(colour = guide_legend(title = "Forecast"))



train_set_2019 <- data_ts[1:360,]
test_set_2020 <- data_ts[361,]


rw_model_2020 <- train_set_2019 %>% model(
  ga_ardl = TSLM(d.fl_nonfarm~ lag(d.fl_nonfarm,1)  + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) + lag(d.fl_nonfarm,8) + 
                   lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11) + lag(d.fl_nonfarm,12) + 
                   lag(d.fl_lf,1) + lag(d.fl_lf,3) + lag(d.fl_lf,4) + lag(d.fl_lf,7) + lag(d.fl_lf,8) + lag(d.fl_lf,11) +
                   lag(d.us_epr,1) + lag(d.us_epr,2) + lag(d.us_epr,3) + lag(d.us_epr,4) + lag(d.us_epr,5) + 
                   lag(d.us_epr,6) + lag(d.us_epr,7) + lag(d.us_epr,8) + lag(d.us_epr,9) + lag(d.us_epr,10) +
                   lag(d.fl_bp,1) + lag(d.fl_bp,2)  + lag(d.fl_bp,3) + lag(d.fl_bp,4)  + lag(d.fl_bp,5) +
                   lag(d.fl_bp,6) + lag(d.fl_bp,7)  + lag(d.fl_bp,8)  + lag(d.fl_bp,9)  + lag(d.fl_bp,12) +
                   YearMonth)
)

ga_fc_2020 <- forecast(rw_model_2020, new_data = test_set_2020)

#Plotting Predictions
autoplot(ga_fc_2020, data = train_set_2019[320:360,], level = c(68, 95, 99.7), colour = "darkblue") +
  ggtitle("Forecast: L.Differenced Nonfarm Employment from GA model: Jan 2020") +
  xlab("Months") +
  ylab("Nonfarm")



rw_model_2020_2 <- train_set_2019 %>% model(
  ga_ardl = TSLM(fl_nonfarm~ lag(diff.fl_nonfarm,1)  + lag(diff.fl_nonfarm,4) + lag(diff.fl_nonfarm,5) + lag(diff.fl_nonfarm,8) + 
                   lag(diff.fl_nonfarm,9) + lag(diff.fl_nonfarm,10) + lag(diff.fl_nonfarm,11) + lag(diff.fl_nonfarm,12) + 
                   lag(diff.fl_lf,1) + lag(diff.fl_lf,3) + lag(diff.fl_lf,4) + lag(diff.fl_lf,7) + lag(diff.fl_lf,8) + lag(diff.fl_lf,11) +
                   lag(diff.us_epr,1) + lag(diff.us_epr,2) + lag(diff.us_epr,3) + lag(diff.us_epr,4) + lag(diff.us_epr,5) + 
                   lag(diff.us_epr,6) + lag(diff.us_epr,7) + lag(diff.us_epr,8) + lag(diff.us_epr,9) + lag(diff.us_epr,10) +
                   lag(diff.fl_bp,1) + lag(diff.fl_bp,2)  + lag(diff.fl_bp,3) + lag(diff.fl_bp,4)  + lag(diff.fl_bp,5) +
                   lag(diff.fl_bp,6) + lag(diff.fl_bp,7)  + lag(diff.fl_bp,8)  + lag(diff.fl_bp,9)  + lag(diff.fl_bp,12) +
                   YearMonth)
)

ga_fc_2020_2 <- forecast(rw_model_2020_2, new_data = test_set_2020)

#Plotting Predictions
autoplot(ga_fc_2020_2, data = data_ts[200:361,], level = c(68, 95, 99.7), colour = "darkred") +
  ggtitle("Forecast for Nonfarm Employment from GA model: Jan 2020") +
  xlab("Months") +
  ylab("Nonfarm")



rw_model_2020_3 <- train_set_2019 %>% model(
  ga_ardl = TSLM(fl_nonfarm~ lag(diff.fl_nonfarm,1)  + lag(diff.fl_nonfarm,4) + lag(diff.fl_nonfarm,5) + lag(diff.fl_nonfarm,8) + 
                   lag(diff.fl_nonfarm,9) + lag(diff.fl_nonfarm,10) + lag(diff.fl_nonfarm,11) + lag(diff.fl_nonfarm,12) + 
                   lag(diff.fl_lf,1) + lag(diff.fl_lf,3) + lag(diff.fl_lf,4) + lag(diff.fl_lf,7) + lag(diff.fl_lf,8) + lag(diff.fl_lf,11) +
                   lag(diff.us_epr,1) + lag(diff.us_epr,2) + lag(diff.us_epr,3) + lag(diff.us_epr,4) + lag(diff.us_epr,5) + 
                   lag(diff.us_epr,6) + lag(diff.us_epr,7) + lag(diff.us_epr,8) + lag(diff.us_epr,9) + lag(diff.us_epr,10) +
                   lag(diff.fl_bp,1) + lag(diff.fl_bp,2)  + lag(diff.fl_bp,3) + lag(diff.fl_bp,4)  + lag(diff.fl_bp,5) +
                   lag(diff.fl_bp,6) + lag(diff.fl_bp,7)  + lag(diff.fl_bp,8)  + lag(diff.fl_bp,9)  + lag(diff.fl_bp,12) +
                   YearMonth)
)

ga_fc_2020_3 <- forecast(rw_model_2020_3, new_data = test_set_2020)

# #For loop to grab the upper bounds for empirical approach
# for(i in 1:nrow(hilo(ga_fc_2020_3, 97.5)['97.5%'])){
#   
#   emp_bounds[i,] <- hilo(ga_fc_2020_3, 97.5)['97.5%'][[1]][[i]]
# }
# 
# #setting the upper and lower bounds empirically
# upbd <- exp(data_ts[349:361,5] + emp_bounds[,2])
# lwbd <- exp(data_ts[349:361,5] + emp_bounds[,1])


#Plotting Predictions
autoplot(ga_fc_2020_2, data = data_ts[200:361,], level = c(97.5), colour = "orange") +
  ggtitle("Forecast for Nonfarm Employment (Empirical) from GA model: Jan 2020") +
  xlab("Months") +
  ylab("Nonfarm") +
  theme(legend.position = "none")



test_set_2020 <- data_ts[349:361,]




#building the models

model_1 <- train_set %>% model(
  prev_ardl = TSLM(d.fl_nonfarm ~ lag(d.fl_nonfarm,1) + lag(d.fl_nonfarm,2) + lag(d.fl_nonfarm,3) + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) +
                     lag(d.fl_nonfarm,6) + lag(d.fl_nonfarm,7) + lag(d.fl_nonfarm,8) + lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11)+
                     lag(d.fl_nonfarm,12) + lag(d.fl_lf,1) + lag(d.fl_lf,2) +
                     lag(d.us_epr,1) + lag(d.us_epr,2) +
                     lag(d.fl_bp,1) + lag(d.fl_bp,2)  +
                     YearMonth),
  ga_ardl = TSLM(d.fl_nonfarm ~ lag(d.fl_nonfarm,1)  + lag(d.fl_nonfarm,4) + lag(d.fl_nonfarm,5) + lag(d.fl_nonfarm,8) + 
                   lag(d.fl_nonfarm,9) + lag(d.fl_nonfarm,10) + lag(d.fl_nonfarm,11) + lag(d.fl_nonfarm,12) + 
                   lag(d.fl_lf,1) + lag(d.fl_lf,3) + lag(d.fl_lf,4) + lag(d.fl_lf,7) + lag(d.fl_lf,8) + lag(d.fl_lf,11) +
                   lag(d.us_epr,1) + lag(d.us_epr,2) + lag(d.us_epr,3) + lag(d.us_epr,4) + lag(d.us_epr,5) + 
                   lag(d.us_epr,6) + lag(d.us_epr,7) + lag(d.us_epr,8) + lag(d.us_epr,9) + lag(d.us_epr,10) +
                   lag(d.fl_bp,1) + lag(d.fl_bp,2)  + lag(d.fl_bp,3) + lag(d.fl_bp,4)  + lag(d.fl_bp,5) +
                   lag(d.fl_bp,6) + lag(d.fl_bp,7)  + lag(d.fl_bp,8)  + lag(d.fl_bp,9)  + lag(d.fl_bp,12) +
                   YearMonth),
  
  arima = ARIMA(d.fl_nonfarm),
  var = VAR(d.fl_nonfarm  ~ AR(1:12) , ic = "aic" )
)

# forecasting on the test set 
fc <- forecast(model_1, new_data = test_set_2020)

#Plotting Predictions
autoplot(fc, data = data_ts[300:361,], level = NULL) +
  ggtitle("Forecasts for Nonfarm Employment (4 Models)") +
  xlab("Months") +
  ylab("L.d. Nonfarm Employment")
guides(colour = guide_legend(title = "Forecast")) +
  theme(legend.position = "bottom")




fc_accuracy <- accuracy(fc, test_set,
                        measures = list(
                          point_accuracy_measures,
                          interval_accuracy_measures,
                          distribution_accuracy_measures
                        )
)

#creating an accuracy table
final_tab <- fc_accuracy %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE),
    MAE = mean(MAE),
    MASE = mean(MASE),
    Winkler = mean(winkler),
    CRPS = mean(CRPS)
  ) %>%
  arrange(RMSE) 


final_tab['MASE'] <- NULL


final_tab %>% kable(format = "latex") %>% kable_styling(position = "center", latex_options = "striped")










