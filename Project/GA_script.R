#Defining predictors for Genetic algorithm 
#getting a bunch of lags
lag_predicts <-  seq(num_of_lags)
lag_names <- paste("lag", formatC(lag_predicts, width = nchar(max(lag_predicts)), flag = "0"), sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lag_predicts, ")"), lag_names)


#creating the lag variables in a dataframe
lag_df <- data_ts %>% mutate_at(vars(diff.nonfarm, diff.avg_week_hrs, 
                                     diff.avg_hr_earn, diff.avg_w_earn, 
                                     diff.all_goods, diff.tot_w_earn), funs_(lag_functions))

lag_df <- lag_df[11:nrow(lag_df),]

lag_df <- lag_df %>% 
  select(-c('nonfarm', "avg_week_hrs", "avg_hr_earnings",
            "avg_week_earnings", "all_goods", "tot_week_earnings", 
            'diff.avg_week_hrs', 'diff.avg_hr_earn', 'diff.avg_w_earn', 
            'diff.all_goods','diff.tot_w_earn', 'date'))


ctrl <- gafsControl(functions = caretGA,
                    method = "cv",
                    number = 10,
                    metric = c(internal = "RMSE", external = "RMSE"),
                    maximize = c(internal = FALSE, external = FALSE),
                    allowParallel = TRUE,
                    genParallel = TRUE,
                    verbose = TRUE)



#setting up parallel processing
library(doParallel)


#Function to stop the cluster after its done running 
autoStopCluster <- function(cl) {
  stopifnot(inherits(cl, "cluster"))
  env <- new.env()
  env$cluster <- cl
  attr(cl, "gcMe") <- env
  reg.finalizer(env, function(e) {
    message("Finalizing cluster ...")
    message(capture.output(print(e$cluster)))
    try(parallel::stopCluster(e$cluster), silent = FALSE)
    message("Finalizing cluster ... done")
  })
  cl
}

cl <- autoStopCluster(makeCluster(11))
cl

library(recipes)
#setting up a recipe to get the variables needed for gafs

# not_inc <- list(date, ln_fl_nonfarm, ln_fl_lf, ln_fl_bp, ln_us_epr, d.fl_nonfarm, d.fl_lf, d.fl_bp, d.us_epr)
lag_rec <- lag_df %>%
  recipe(diff.nonfarm~.)



obj <- gafs(lag_rec,
            iters = 100,
            popSize = 50,
            gafsControl = ctrl,
            data = lag_df,
            ## Now pass options to `train`

            method = "lm",
            na.action = na.omit,
            preProcess = c("center", "scale"), 
            trControl = trainControl(method = "cv", number = 10,
                                     allowParallel = TRUE)); beepr::beep("coin")


plot(obj) + theme_bw()