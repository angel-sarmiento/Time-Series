#library Import
library(caret)
library(tidyverse)

library(fable)
library(feasts)
library(fredr)
library(tsibble)
library(kableExtra)
library(plyr)
set.seed(23)

#creating the tibble/dataframe with 30 observations
tsdf <- tibble(ts_index = c(1:30), r = rnorm(30))

tsdf$y <- 0

#logic that doesnt really work, but sets the y values according to a funciton with 3 lags
tsdf <- tsdf %>%
  mutate(y = ifelse(ts_index < 4, r,
                    y = 0.5 + 0.5*lag(y,1) - 0.1*lag(y, 2) + 0.25*lag(y, 3) + r))

#replacing rows 1 through 3 with the associated r values 
tsdf[1:3,3] <- tsdf[1:3,2]

#making 7 different possible Autoregressive models (1, 2, 3) using loocv in caret 
train_ct1 <- trainControl(method = "LOOCV")

# 1,2,3 lag structure
lm_model1 <- train(y ~ lag(y) + lag(y, 2) + lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")
# 1 2 lag structure
lm_model2 <- train(y ~ lag(y) +  lag(y, 2),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")
# 1 3 lag structure
lm_model3 <- train(y ~ lag(y) + lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model4 <- train(y ~ lag(y, 2) + lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model5 <- train(y ~ lag(y),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model6 <- train(y ~ lag(y, 2),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model7 <- train(y ~ lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

#binding all of the results by rows into one matrix 
results_matrix <- as_tibble(bind_rows(lm_model1$results, 
                                      lm_model2$results,
                                      lm_model3$results,
                                      lm_model4$results,
                                      lm_model5$results,
                                      lm_model6$results,
                                      lm_model7$results))
results_matrix[,1] <- NULL

#Matrix of the AIC values
AIC_matrix <- AIC(lm_model1$finalModel, lm_model2$finalModel, lm_model3$finalModel, 
                  lm_model4$finalModel, lm_model5$finalModel, lm_model6$finalModel, lm_model7$finalModel)
#Removing the first column
AIC_matrix[,1] <- NULL

BIC_matrix <- BIC(lm_model1$finalModel, lm_model2$finalModel, lm_model3$finalModel, 
                  lm_model4$finalModel, lm_model5$finalModel, lm_model6$finalModel, lm_model7$finalModel)

#removing the first column
BIC_matrix[,1] <- NULL

#appending them to the results matrix
results_matrix['AIC'] <- AIC_matrix
results_matrix['BIC'] <- BIC_matrix


#similar cross-validation to above, except using k-folds


#making 7 different possible Autoregressive models (1, 2, 3) using loocv in caret 
train_ct2 <- trainControl(method = "cv", number = 10)

# Creating a training set using 80% of the data
inTrain2 <- createDataPartition(y = tsdf$y, p = 0.8, list = FALSE)

#training data
train_set2 <- tsdf[inTrain2, ]
#test data (with the other 20 percent)
test_set2 <- tsdf[-inTrain2, ]




# 1,2,3 lag structure
lm_model_cv1 <- train(y ~ lag(y) + lag(y, 2) + lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")
# 1 2 lag structure
lm_model_cv2 <- train(y ~ lag(y) +  lag(y, 2),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")
# 1 3 lag structure
lm_model_cv3 <- train(y ~ lag(y) + lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv4 <- train(y ~ lag(y, 2) + lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv5 <- train(y ~ lag(y),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv6 <- train(y ~ lag(y, 2),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv7 <- train(y ~ lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

#Getting the results and appending them to the same matrix as before

models <- c(lm_model_cv1, lm_model_cv2, lm_model_cv3, 
            lm_model_cv4, lm_model_cv5, lm_model_cv6, lm_model_cv7)

predictions1 <- predict(lm_model_cv1, test_set2)
predictions2 <- predict(lm_model_cv2, test_set2)
predictions3 <- predict(lm_model_cv3, test_set2)
predictions4 <- predict(lm_model_cv4, test_set2)
predictions5 <- predict(lm_model_cv5, test_set2)
predictions6 <- predict(lm_model_cv6, test_set2)
predictions7 <- predict(lm_model_cv7, test_set2)

#post prediction resampling 
lag_123 <- postResample(predictions1, test_set2$y)
lag_12 <- postResample(predictions2, test_set2$y)
lag_13 <- postResample(predictions3, test_set2$y)
lag_23 <- postResample(predictions4, test_set2$y)
lag_1 <- postResample(predictions5, test_set2$y)
lag_2 <- postResample(predictions6, test_set2$y)
lag_3 <- postResample(predictions7, test_set2$y)


#creating a new matrix and binding the RMSE values
results2 <- rbind(lag_123, lag_12, lag_13, lag_23, lag_1, lag_2, lag_3 ) %>% 
  as.data.frame() %>% 
  subset(select = RMSE)


#Binding it to the original matrix
results_matrix <- cbind(results_matrix, results2)

#renaming the columns
colnames(results_matrix)[6] <- "k_RMSE"

kable(results_matrix, format = "latex") %>% 
  kable_styling(position = "center", latex_options = c("striped", "HOLD_position"))

require(ggplot2)

results_mods <- data.frame(cbind(predictions = lm_model1$pred[,1], observations = lm_model1$pred[,2]))

model_plot1 <- ggplot(results_mods, aes(predictions, observations)) +
  geom_point(color = "orange", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags Included: 1, 2, 3") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))


results_mods2 <- data.frame(cbind(predictions = lm_model2$pred[,1], observations = lm_model2$pred[,2]))

model_plot2 <- ggplot(results_mods2, aes(predictions, observations)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags included: 1, 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_mods6 <- data.frame(cbind(predictions = lm_model6$pred[,1], observations = lm_model6$pred[,2]))

model_plot6 <- ggplot(results_mods6, aes(predictions, observations)) +
  geom_point(color = "darkblue", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags included: 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_mods5 <- data.frame(cbind(predictions = lm_model5$pred[,1], observations = lm_model5$pred[,2]))

model_plot5 <- ggplot(results_mods5, aes(predictions, observations)) +
  geom_point(color = "darkgrey", alpha = 0.8) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 5') +
  ggtitle("Lags Included: 1") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black", size=12, hjust = 0.5))



require(patchwork)
patchwork = model_plot1 + model_plot2 + model_plot5 + model_plot6

patchwork[[1]] = patchwork[[1]] + theme(axis.title.x = element_blank())

patchwork[[2]] = patchwork[[2]] + theme(axis.title.x = element_blank())

patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

patchwork[[4]] = patchwork[[4]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

patchwork 


#creating the tibble/dataframe with 300 observations
tsdf <- tibble(ts_index = c(1:300), r = rnorm(300))

tsdf$y <- 0

#logic that doesnt really work, but sets the y values according to a funciton with 3 lags
tsdf <- tsdf %>%
  mutate(y = ifelse(ts_index < 4, r,
                    y = 0.5 + 0.5*lag(y,1) - 0.1*lag(y, 2) + 0.25*lag(y, 3) + r))

#replacing rows 1 through 3 with the associated r values 
tsdf[1:3,3] <- tsdf[1:3,2]

#making 7 different possible Autoregressive models (1, 2, 3) using loocv in caret 
train_ct1 <- trainControl(method = "LOOCV")

# 1,2,3 lag structure
lm_model1 <- train(y ~ lag(y) + lag(y, 2) + lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")
# 1 2 lag structure
lm_model2 <- train(y ~ lag(y) +  lag(y, 2),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")
# 1 3 lag structure
lm_model3 <- train(y ~ lag(y) + lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model4 <- train(y ~ lag(y, 2) + lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model5 <- train(y ~ lag(y),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model6 <- train(y ~ lag(y, 2),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")

lm_model7 <- train(y ~ lag(y, 3),  data = tsdf,
                   na.action = na.pass,
                   trControl = train_ct1,
                   method = "lm")



#binding all of the results by rows into one matrix 
results_matrix2 <- as_tibble(bind_rows(lm_model1$results, 
                                       lm_model2$results,
                                       lm_model3$results,
                                       lm_model4$results,
                                       lm_model5$results,
                                       lm_model6$results,
                                       lm_model7$results))
results_matrix2[,1] <- NULL

#Matrix of the AIC values
AIC_matrix <- AIC(lm_model1$finalModel, lm_model2$finalModel, lm_model3$finalModel, 
                  lm_model4$finalModel, lm_model5$finalModel, lm_model6$finalModel, lm_model7$finalModel)
#Removing the first column
AIC_matrix[,1] <- NULL

BIC_matrix <- BIC(lm_model1$finalModel, lm_model2$finalModel, lm_model3$finalModel, 
                  lm_model4$finalModel, lm_model5$finalModel, lm_model6$finalModel, lm_model7$finalModel)

#removing the first column
BIC_matrix[,1] <- NULL

#appending them to the results matrix
results_matrix2['AIC'] <- AIC_matrix
results_matrix2['BIC'] <- BIC_matrix


#similar cross-validation to above, except using k-folds


#making 7 different possible Autoregressive models (1, 2, 3) using loocv in caret 
train_ct2 <- trainControl(method = "cv", number = 10)

# Creating a training set using 80% of the data
inTrain2 <- createDataPartition(y = tsdf$y, p = 0.8, list = FALSE)

#training data
train_set2 <- tsdf[inTrain2, ]
#test data (with the other 20 percent)
test_set2 <- tsdf[-inTrain2, ]




# 1,2,3 lag structure
lm_model_cv1 <- train(y ~ lag(y) + lag(y, 2) + lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")
# 1 2 lag structure
lm_model_cv2 <- train(y ~ lag(y) +  lag(y, 2),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")
# 1 3 lag structure
lm_model_cv3 <- train(y ~ lag(y) + lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv4 <- train(y ~ lag(y, 2) + lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv5 <- train(y ~ lag(y),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv6 <- train(y ~ lag(y, 2),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

lm_model_cv7 <- train(y ~ lag(y, 3),  data = train_set2,
                      na.action = na.pass,
                      trControl = train_ct2,
                      method = "lm")

#Getting the results and appending them to the same matrix as before

models <- c(lm_model_cv1, lm_model_cv2, lm_model_cv3, 
            lm_model_cv4, lm_model_cv5, lm_model_cv6, lm_model_cv7)

predictions1 <- predict(lm_model_cv1, test_set2)
predictions2 <- predict(lm_model_cv2, test_set2)
predictions3 <- predict(lm_model_cv3, test_set2)
predictions4 <- predict(lm_model_cv4, test_set2)
predictions5 <- predict(lm_model_cv5, test_set2)
predictions6 <- predict(lm_model_cv6, test_set2)
predictions7 <- predict(lm_model_cv7, test_set2)

#post prediction resampling 
model123 <- postResample(predictions1, test_set2$y)
model12 <- postResample(predictions2, test_set2$y)
model13 <- postResample(predictions3, test_set2$y)
model23 <- postResample(predictions4, test_set2$y)
model1 <- postResample(predictions5, test_set2$y)
model2 <- postResample(predictions6, test_set2$y)
model3 <- postResample(predictions7, test_set2$y)


#creating a new matrix and binding the RMSE values
results3 <- rbind(model123, model12, model13, model23, model1, model2, model3 ) %>% 
  as.data.frame() %>% 
  subset(select = RMSE)


#Binding it to the original matrix
results_matrix2 <- cbind(results_matrix2, results3)

#renaming the columns
colnames(results_matrix2)[6] <- "k_RMSE"

kable(results_matrix2, format = "latex") %>% 
  kable_styling(position = "center", latex_options = c("striped", "HOLD_position"))


#PLOTTING LOOCV FOR 300 OBS
require(ggplot2)

results_mods <- data.frame(cbind(predictions = lm_model1$pred[,1], observations = lm_model1$pred[,2]))

model_plot1 <- ggplot(results_mods, aes(predictions, observations)) +
  geom_point(color = "orange", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags Included: 1, 2, 3") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))


results_mods2 <- data.frame(cbind(predictions = lm_model2$pred[,1], observations = lm_model2$pred[,2]))

model_plot2 <- ggplot(results_mods2, aes(predictions, observations)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags included: 1, 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_mods6 <- data.frame(cbind(predictions = lm_model6$pred[,1], observations = lm_model6$pred[,2]))

model_plot6 <- ggplot(results_mods6, aes(predictions, observations)) +
  geom_point(color = "darkblue", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags included: 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_mods5 <- data.frame(cbind(predictions = lm_model5$pred[,1], observations = lm_model5$pred[,2]))

model_plot5 <- ggplot(results_mods5, aes(predictions, observations)) +
  geom_point(color = "darkgrey", alpha = 0.8) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 5') +
  ggtitle("Lags Included: 1") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black", size=12, hjust = 0.5))


#setting up a grid of all of the plots 
require(patchwork)
patchwork = model_plot1 + model_plot2 + model_plot5 + model_plot6

patchwork[[1]] = patchwork[[1]] + theme(axis.title.x = element_blank())

patchwork[[2]] = patchwork[[2]] + theme(axis.title.x = element_blank())

patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

patchwork[[4]] = patchwork[[4]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

patchwork 


#PLOTTING KFOLDS FOR 300 OBS

results_modscv1 <- data.frame(cbind(predictions = predictions1, observations = test_set2$y))

model_plotcv1 <- ggplot(results_modscv1, aes(predictions, observations)) +
  geom_point(color = "orange", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags Included: 1, 2, 3") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))


results_modscv2 <- data.frame(cbind(predictions = predictions2, observations = test_set2$y))

model_plotcv2 <- ggplot(results_modscv2, aes(predictions, observations)) +
  geom_point(color = "darkred", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags included: 1, 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_modscv6 <- data.frame(cbind(predictions = predictions6, observations = test_set2$y))

model_plotcv6 <- ggplot(results_modscv6, aes(predictions, observations)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Lags included: 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_modscv5 <- data.frame(cbind(predictions = predictions5, observations = test_set2$y))

model_plotcv5 <- ggplot(results_modscv5, aes(predictions, observations)) +
  geom_point(color = "darkgrey", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 5') +
  ggtitle("Lags Included: 1") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))



#setting up a grid of all of the plots 
patchwork2 = model_plotcv1 + model_plotcv2 + model_plotcv6 + model_plotcv5

patchwork2[[1]] = patchwork2[[1]] + theme(axis.title.x = element_blank())

patchwork2[[2]] = patchwork2[[2]] + theme(axis.title.x = element_blank())

patchwork2[[2]] = patchwork2[[2]] + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() )

patchwork2[[4]] = patchwork2[[4]] + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() )

patchwork2



#importing the data
data <- read_csv("data.csv") %>% na.omit()

data[3:6] <- log(data[3:6])
data[,1] <- NULL

colnames(data)[2:5] <- c("ln_fl_nonfarm", "ln_fl_lf", "ln_us_epr", "ln_fl_bp")
kable(head(data), format = "latex") %>% 
  kable_styling(position = "center", latex_options = "striped")


#Making the four different models
#creating a new dataframe 
#FIRST MODEL
data['d.nonfarm'] <- difference(data$ln_fl_nonfarm, differences = 1)
data['d.nonfarm_lag'] <- difference(data$ln_fl_nonfarm, lag = 12, difference = 1)
data['d.lf_lag'] <- difference(data$ln_fl_lf, lag = 12, differences = 1) 
data['d.fl_bp_lag'] <- difference(data$ln_fl_bp, lag = 12, differences = 1)
data['d.usepr'] <- difference(data$ln_us_epr, lag = 12, differences = 1) 

months <- yearmonth(data$DATE) %>% 
  format(format = "%m") %>% 
  as.factor()
data['months'] <- months



#LOOCV
model_1 <- train(d.nonfarm ~ d.nonfarm_lag + d.lf_lag + d.fl_bp_lag + d.usepr + months + DATE, 
                 na.action = na.exclude, 
                 data = data,
                 trControl = trainControl(method = "LOOCV"),
                 method = "lm")

#writing results to final table
final_results <- rbind(model_1$results)



#SECOND MODEL
#changing the lag structure
data['d.lf_lag'] <- difference(data$ln_fl_lf, lag = 2, differences = 1) 
data['d.fl_bp_lag'] <- difference(data$ln_fl_bp, lag = 2, differences = 1)
data['d.usepr'] <- difference(data$ln_us_epr, lag = 2, differences = 1) 


model_2 <- train(d.nonfarm ~ d.nonfarm_lag + d.lf_lag + d.fl_bp_lag + d.usepr + months + DATE, 
                 na.action = na.exclude, 
                 data = data,
                 trControl = trainControl(method = "LOOCV"),
                 method = "lm")

final_results <- rbind(final_results, model_2$results)



#THIRD MODEL
data['d.lf_lag'] <- difference(data$ln_fl_lf, lag = 2, differences = 1) 
data['d.lf_lag_12'] <- difference(data$ln_fl_lf, lag = 12, differences = 1)
data['d.fl_bp_lag'] <- difference(data$ln_fl_bp, lag = 2, differences = 1)
data['d.fl_bp_12'] <- difference(data$ln_fl_bp, lag = 12, differences = 1)
data['d.usepr'] <- difference(data$ln_us_epr, lag = 2, differences = 1) 
data['d.usepr_12'] <- difference(data$ln_us_epr, lag = 12, differences = 1)

#LOOCV
model_3 <- train(d.nonfarm ~ d.nonfarm_lag + d.lf_lag + d.lf_lag_12 + d.fl_bp_lag + d.fl_bp_12 + d.usepr_12 + d.usepr + months + DATE, 
                 na.action = na.exclude, 
                 data = data,
                 trControl = trainControl(method = "LOOCV"),
                 method = "lm")

#writing results to final table

final_results <- rbind(final_results, model_3$results)


#FOURTH MODEL
data['d.nonfarm_lag_24'] <- difference(data$ln_fl_nonfarm, lag = 24, difference = 1)
data['d.lf_lag'] <- difference(data$ln_fl_lf, lag = 2, differences = 1) 
data['d.lf_lag_12'] <- difference(data$ln_fl_lf, lag = 12, differences = 1)
data['d.lf_lag_24'] <- difference(data$ln_fl_lf, lag = 24, differences = 1)
data['d.fl_bp_lag'] <- difference(data$ln_fl_bp, lag = 2, differences = 1)
data['d.fl_bp_12'] <- difference(data$ln_fl_bp, lag = 12, differences = 1)
data['d.fl_bp_24'] <- difference(data$ln_fl_bp, lag = 24, differences = 1)
data['d.usepr'] <- difference(data$ln_us_epr, lag = 2, differences = 1) 
data['d.usepr_12'] <- difference(data$ln_us_epr, lag = 12, differences = 1)
data['d.usepr_24'] <- difference(data$ln_us_epr, lag = 24, differences = 1)

#LOOCV
model_4 <- train(d.nonfarm ~ d.nonfarm_lag + d.nonfarm_lag_24 + d.lf_lag + d.lf_lag_12 + d.lf_lag_24 + 
                   d.fl_bp_lag + d.fl_bp_12 + d.fl_bp_24 + d.usepr + d.usepr_12 + d.usepr_24 + months, 
                 na.action = na.exclude, 
                 data = data,
                 trControl = trainControl(method = "LOOCV"),
                 method = "lm")

#writing results to final table

final_results <- rbind(final_results, model_4$results)


final_results[,1] <- NULL

#Matrix of the AIC values
AIC_final <- AIC(model_1$finalModel, model_2$finalModel, model_3$finalModel, 
                 model_4$finalModel)
#Removing the first column
AIC_final[,1] <- NULL

BIC_final <- BIC(model_1$finalModel, model_2$finalModel, model_3$finalModel, 
                 model_4$finalModel)

#removing the first column
BIC_final[,1] <- NULL

#appending them to the results matrix
final_results['AIC'] <- AIC_final
final_results['BIC'] <- BIC_final

#adding rownames
row.names(final_results) <- c("Model 1", "Model 2", "Model 3", "Model 4")

kable(final_results, format = "latex") %>% 
  kable_styling(position = "center", latex_options = "striped")


results_final <- data.frame(cbind(predictions = model_1$pred[,1], observations = model_1$pred[,2]))

model_final_1 <- ggplot(results_final, aes(predictions, observations)) +
  geom_point(color = "orange", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Model 1") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))


results_final_2 <- data.frame(cbind(predictions = model_2$pred[,1], observations = model_2$pred[,2]))

model_final_2 <- ggplot(results_final_2, aes(predictions, observations)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Model 2") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_final_3 <- data.frame(cbind(predictions = model_3$pred[,1], observations = model_3$pred[,2]))

model_final_3 <- ggplot(results_final_3, aes(predictions, observations)) +
  geom_point(color = "darkblue", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 1') +
  ggtitle("Model 3") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black",size=12,hjust = 0.5))

results_final_4 <- data.frame(cbind(predictions = model_4$pred[,1], observations = model_4$pred[,2]))

model_final_4 <- ggplot(results_final_4, aes(predictions, observations)) +
  geom_point(color = "darkgrey", alpha = 0.8) + 
  geom_smooth(method = "lm", colour = "black")+ ggtitle('Model 5') +
  ggtitle("Model 4") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(color="black", size=12, hjust = 0.5))



require(patchwork)
patchwork = model_final_1 + model_final_2 + model_final_3 + model_final_4

patchwork[[1]] = patchwork[[1]] + theme(axis.title.x = element_blank())

patchwork[[2]] = patchwork[[2]] + theme(axis.title.x = element_blank())

patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

patchwork[[4]] = patchwork[[4]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

patchwork 

beepr::beep("coin")