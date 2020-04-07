ctrl <- gafsControl(functions = caretGA,
                    method = "cv",
                    number = 5,
                    metric = c(internal = "RMSE", external = "RMSE"),
                    maximize = c(internal = FALSE, external = FALSE),
                    allowParallel = TRUE,
                    genParallel = TRUE,
                    verbose = TRUE)


#setting up parallel processing
library(doParallel)
cl <- makePSOCKcluster(11)
registerDoParallel(cl)

library(recipes)
#setting up a recipe to get the variables needed for gafs

# not_inc <- list(date, ln_fl_nonfarm, ln_fl_lf, ln_fl_bp, ln_us_epr, d.fl_nonfarm, d.fl_lf, d.fl_bp, d.us_epr)
lag_rec <- b_df %>%
  recipe(d.fl_nonfarm~.)



obj <- gafs(lag_rec,
            iters = 100,
            popSize = 20,
            gafsControl = ctrl,
            data = b_df,
            elite = 3,
            ## Now pass options to `train`

            method = "lm",
            na.action = na.omit,
            trControl = trainControl(method = "cv", number = 5,
                                     allowParallel = TRUE)); beepr::beep("coin")

stopCluster(cl)

autoplot(obj)