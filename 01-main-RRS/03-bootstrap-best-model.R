# clear the workspace except for key variables
rm(list = setdiff(ls(), do_not_rm))

# load the saved object that stores the best model
fit = readRDS(file.path(out_dir, paste0(y_variable, "-best-fit.rds")))

# A FUNCTION TO CREATE A DATA SET OF PREDICTOR VARIABLES
make_pred_data = function() {
  out = NULL
  
  # calculate mean and sd of length data on raw scale
  mean_length_raw = mean(dat$length_raw)
  sd_length_raw = sd(dat$length_raw)
  
  # loop through years, sexes, and origins to produce a prediction data set over the observed range
  # of all continuous variables for each combo
  # i.e., so that we do not extrapolate days/lengths not observed for each year-origin-sex combo
  # also, include a value for mean day and mean length for each year-origin-sex combo, to enable 
  # summarizing predictions for the average fish of a given year-origin-sex combo
  for (y in levels(dat$year)) {
    for (s in c("M", "F")) {
      for (o in c("H", "W")) {
        # extract data for this year, sex, and origin combo
        dat_sub = subset(dat, year == y & sex == s & origin == o)
        
        # calculate the mean length and day values observed for this combo
        mean_length = mean(dat_sub$length)
        mean_day = mean(dat_sub$day)
        
        # build 10-element, ordered and evenly spaced sequences from the smallest to largest values observed each year for this sex/origin
        day_seq = seq(min(dat_sub$day), max(dat_sub$day), length = 10)
        length_seq = seq(min(dat_sub$length), max(dat_sub$length), length = 10)
        
        # insert the mean in the sequence where it belongs
        day_seq = c(day_seq[day_seq < mean_day], mean_day, day_seq[day_seq > mean_day])
        length_seq = c(length_seq[length_seq < mean_length], mean_length, length_seq[length_seq > mean_length])
        
        # build all combinations
        tmp = expand.grid(year = y, sex = s, origin = o, day = day_seq, length = length_seq)
        
        # add a variable to identify a combo as having mean length or day values
        # enables easier plotting later
        tmp$is_mean_day = tmp$day == mean_day
        tmp$is_mean_length = tmp$length == mean_length
        
        # convert the length to the original raw scale: for plotting later 
        tmp$length_raw = tmp$length * sd_length_raw + mean_length_raw
        
        # convert the day to the original raw scale: for plotting later
        tmp$day_raw = tmp$day + min(dat$day_raw)
        
        # combine this data set with all of those that came before it
        out = rbind(out, tmp)
      }
    }
  }
  
  # return the combos of predictor variables
  return(out)
}

# A FUNCTION TO PREDICT RESPONSE VARIABLES
predict_model = function(fit) {
  # build data frame for prediction: individual years, all combos of covariates
  pred_data = make_pred_data()
  
  # obtain expected values at each combo of prediction covariates
  pred_data$cond  = predict(fit, pred_data, type = "conditional")  # conditional mean: expected value of all non-zero counts
  pred_data$resp  = predict(fit, pred_data, type = "response")  # response mean: expected value of all counts
  pred_data$nzprb = 1 - predict(fit, pred_data, type = "zprob") # probability of crossing hurdle
  
  # convert to long format across prediction types
  pred_data = reshape2::melt(pred_data, id.vars = c("year", "origin", "sex", "day", "length", "day_raw", "length_raw", "is_mean_day", "is_mean_length"))
  
  # split up identifiers and numerical output
  # bootMer can only handle numerical output objects
  out = list(
    id = pred_data[,-which(colnames(pred_data) == "value")],
    out = pred_data$value
  )
  
  # return the output
  return(out)
}

# produce bootstrap predictions of progeny produced at each covariate combination
# this approach simulates data out of the original model, refits same model to those data
# and produces predictions from the refitted model.
# these predictions are summarized later to represent uncertainty around model expected values.

# number of bootstrap iters
nboot = 1000

# start a timer
starttime = Sys.time()

# intialize a cluster for parallel computing
my_cluster = snow::makeSOCKcluster(ncpus)

# send the packages to the cluster
snow::clusterEvalQ(my_cluster, {library("lme4"); library("glmmTMB")})

# send the necessary objects to the cluster
snow::clusterExport(my_cluster, c("fit", "predict_model", "make_pred_data", "dat"))

# run the bootstrap
boot_out = lme4::bootMer(fit, FUN = function(x) predict_model(x)$out, nsim = nboot,
                         parallel = "snow", cl = my_cluster, ncpus = ncpus)

# stop the cluster and timer
snow::stopCluster(my_cluster)
stoptime = Sys.time()
cat("\nElapsed:", format(stoptime - starttime, digits = 2))

# combine bootstrapped output with the identifiers for each covariate combo
# and format. Make the fit to real data iter_0
pred_real = predict_model(fit)$out     # predicted values for the original fit
pred_vars = predict_model(fit)$id      # covariates for prediction
pred_boot = cbind(pred_real, t(boot_out$t))
colnames(pred_boot) = paste0("iter_", 0:nboot)
pred_boot = as.data.frame(pred_boot)
pred_boot = cbind(pred_vars, pred_boot)
pred_boot = reshape2::melt(pred_boot, 
                           id.vars = c("year", "origin", "sex", "day",
                                       "length", "day_raw", "length_raw",
                                       "variable", "is_mean_day", "is_mean_length"),
                           value.name = "value", variable.name = "iter")

# export the bootstrap samples
saveRDS(pred_boot, file = file.path(out_dir, paste0(y_variable, "-boot-samps.rds")))
