
# clear workspace
rm(list = ls(all = TRUE))

# load functions used in multiple analyses
source("common-functions.R")

# choose settings
# progeny_type = "Juv"
progeny_type = "Adult"
n_boot = 1000
ref = "HxH"

# directory to save output to
out_dir = "03-cross-types/model-output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# set resolution for png figures
ppi = 600

# read in the data
in_file = file.path("00-data", paste0(progeny_type, "_Offs_per_Cross.csv"))
dat = read.csv(in_file)

# format the data
dat = dat[,c("Parental_SampleYear", "Parental_CrossType", "No.Offspring")]
colnames(dat) = c("year", "cross_type", "progeny")
dat$year = as.factor(dat$year)
dat$cross_type = factor(dat$cross_type, levels = c("NxN", "HxN", "NxH", "HxH"))

# calculate mean progeny by cross type and year
data_means = round(with(dat, tapply(progeny, list(cross_type, year), mean, na.rm = TRUE)), 2)

# calculate mean progeny ratios by cross type and year
data_mean_ratios = t(apply(data_means, 1, function(x) x/data_means[ref,]))
data_mean_ratios = data_mean_ratios[-which(rownames(data_mean_ratios) == ref),]

# select the appropriate family for fitting GLM
# both data sets have no zeros. untruncated versions had poor QQ-plot of standardized residuals
# this is because count models with low predicted values have a decent amount of expected zeros
# use of truncated dist is justified because data includes only non-zero progeny crosses

# fit models
cat("\nFitting Models\n")
fit0 = glmmTMB::glmmTMB(progeny ~ 1, data = dat, family = glmmTMB::truncated_nbinom2)
fit1 = glmmTMB::glmmTMB(progeny ~ cross_type, data = dat, family = glmmTMB::truncated_nbinom2)
fit2 = glmmTMB::glmmTMB(progeny ~ cross_type + year, data = dat, family = glmmTMB::truncated_nbinom2)
fit3 = glmmTMB::glmmTMB(progeny ~ cross_type * year, data = dat, family = glmmTMB::truncated_nbinom2)

# perform AIC to select the best model
cat("\nAIC Table:\n\n")
AICtab = AIC(fit0, fit1, fit2, fit3); AICtab
best_mod = rownames(AICtab)[which.min(AICtab[,"AIC"])]
best_mod = eval(parse(text = best_mod))

# perform residual diagnostics on the best model
resids = DHARMa::simulateResiduals(best_mod)
plot(resids)

# function to create a prediction data set: all year and cross type combos
make_pred_data = function(fit) expand.grid(year = unique(fit$frame$year), cross_type = unique(fit$frame$cross_type))

# function to simulate new data from the model and refit to simulated data set
# for parametric bootstrapping
simfit = function(fit) {
  
  # extract the covariates of the original data set
  original_x = fit$frame[,c("year", "cross_type")]
  
  # simulate new response variable
  new_y = simulate(object = fit, nsim = 1); colnames(new_y) = "progeny"
  
  # make a new data set
  new_dat = cbind(original_x, new_y)

  # fit the same model to the simulated data set
  # tryCatch to prevent making inference from models that had fitting issues
  new_fit = tryCatch(expr = glmmTMB::glmmTMB(formula(fit), data = new_dat, family = glmmTMB::truncated_nbinom2),
                 warning = function(cond) return("failure"),
                 error = function(cond) return("failure")
  )

  # if fitting issue found, will return all NA predicted values
  # otherwise, returns predicted values 
  if (is.character(new_fit)) {
    return(rep(NA, nrow(make_pred_data(fit))))
  } else {
    mu = predict(new_fit, newdata = make_pred_data(fit), type = "cond")
    phi = predict(new_fit, newdata = make_pred_data(fit), type = "disp")
    pred = sapply(1:length(mu), function(i) get_expected_resp(mu[i], phi[i], 0))
    return(pred)
  }
}

# set random seed
set.seed(1234)

# perform parametric bootstrap
cat("\nPerforming Parametric Bootstrap\n")
starttime = Sys.time()
boot_preds = lapply(1:n_boot, function(i) {cat("\rBootstrap Iter:", i); simfit(best_mod)})
boot_preds = do.call(cbind, boot_preds); cat("\n")
stoptime = Sys.time()
stoptime - starttime

# obtain the fitted values for original non-bootstrapped data
mu = predict(best_mod, newdata = make_pred_data(best_mod), type = "cond")
phi = predict(best_mod, newdata = make_pred_data(best_mod), type = "disp")
original_ests = sapply(1:length(mu), function(i) get_expected_resp(mu[i], phi[i], 0))
boot_preds = cbind(original_ests, boot_preds)

# assign iteration names; original dataset is iter_0
colnames(boot_preds) = paste0("iter_", 0:n_boot)

# append the prediction IDs: which year and cross_type are they?
boot_preds = cbind(make_pred_data(best_mod), boot_preds)

# convert to long format: easier to work with
boot_preds = reshape2::melt(boot_preds, id.vars = c("year", "cross_type"), value.name = "value", variable.name = "iter")

# convert to wide format: easier to work with
boot_preds = reshape2::dcast(boot_preds, year + iter ~ cross_type, value.var = "value")

# calculate RRS ratios relative to the reference cross type
vcols = 3:6
ratios = boot_preds
ratios[,vcols] = apply(boot_preds[,vcols], 2, function(x) x/boot_preds[,ref])
ratios = ratios[,-which(colnames(ratios) == ref)]
ratios = ratios[,-(1:2)]
colnames(ratios) = paste0(colnames(ratios), ":", ref)
boot_preds = cbind(progeny_type = progeny_type, boot_preds, ratios)

# save output for use by later scripts
saveRDS(best_mod, file.path(out_dir, paste0(progeny_type, "-best-fit.rds")))
saveRDS(boot_preds, file.path(out_dir, paste0(progeny_type, "-boot-samps.rds")))
