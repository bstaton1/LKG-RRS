# clear the workspace except for key variables
rm(list = setdiff(ls(), do_not_rm))

# load the saved object that stores the best model
fit = readRDS(file.path(out_dir, paste0(y_variable, "-best-fit.rds")))

# A FUNCTION TO CREATE A DATA SET OF PREDICTOR VARIABLES
make_pred_data = function() {
  out = expand.grid(origin = unique(dat$origin), year = unique(dat$year))
  return(out)
}

# A FUNCTION TO PREDICT RESPONSE VARIABLES
predict_model = function(fit) {
  # build data frame for prediction: individual years, all combos of covariates
  pred_data = make_pred_data()
  
  # obtain negative binomial parameters at each combo of prediction covariates
  mu  = predict(fit, pred_data, type = "cond")  # mu parameter of truncated_nbinom2
  phi = predict(fit, pred_data, type = "disp")  # phi parameter of truncated_nbinom2
  pi  = predict(fit, pred_data, type = "zprob") # probability of not crossing hurdle
  
  # obtain the expected values from the model at each combo of prediction covariates
  # these account for the truncation when calculating the expected value
  pred_data$resp  = sapply(1:nrow(pred_data), function(i) get_expected_resp(mu[i], phi[i], pi[i]))
  pred_data$cond  = sapply(1:nrow(pred_data), function(i) get_expected_cond(mu[i], phi[i]))
  pred_data$nzprb = 1 - pi
  
  # convert to long format across prediction types
  pred_data = reshape2::melt(pred_data, id.vars = c("year", "origin"))
  
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
snow::clusterExport(my_cluster, c("fit", "predict_model", "make_pred_data", "dnbinom2", "dtnbinom2", "get_expected_resp", "get_expected_cond", "dat"))

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
                           id.vars = c("year", "origin", "variable"),
                           value.name = "value", variable.name = "iter")

# export the bootstrap samples
saveRDS(pred_boot, file = file.path(out_dir, paste0(y_variable, "-boot-samps.rds")))
