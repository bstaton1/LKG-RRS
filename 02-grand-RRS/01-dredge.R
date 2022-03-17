# clear the workspace except for key variables
rm(list = setdiff(ls(), do_not_rm))

# specify the full conditional model formula: models variability in expected counts that are non-zero
full_cond_formula = formula(y_var ~ origin * year)

# specify the full zero model formula: models variability in the probability that the count is zero
full_zi_formula = formula(~ origin * year)

# specify the full dispersion model formula: models variability in the negative binomial dispersion parameter
if (y_variable == "total_juv_grand") {
  full_disp_formula = formula(~ 1)
} else {
  full_disp_formula = formula(~ 1)
}

# the grand offspring variables have many NA records. Delete these prior to model fitting
dat = na.omit(dat)

### FIT THE FULL (GLOBAL) MODELS ###

# adult model estimates outrageously high variability (OD param < 1e-8)
# which causes problems in bootstrapping later and calculating expected values.
# solution: fix at 1

# negative binomial hurdle model
if (y_variable == "total_juv_grand") {
  full_model = glmmTMB::glmmTMB(formula     = full_cond_formula,
                                ziformula   = full_zi_formula,
                                data        = dat,
                                family      = glmmTMB::truncated_nbinom2, na.action = "na.fail")
  
} else {
  full_model = glmmTMB::glmmTMB(formula     = full_cond_formula,
                                ziformula   = full_zi_formula,
                                data        = dat,
                                family      = glmmTMB::truncated_nbinom2, na.action = "na.fail",
                                start = list(betad = 0),
                                map = list(betad = factor(c(NA))))
}

### FIT ALL SUBSETS OF THE FULL MODEL ###

# start a timer
starttime = Sys.time()

# initialize a cluster for parallel computing on this machine
my_cluster = parallel::makeCluster(getOption("cl.cores", ncpus), type = "PSOCK")

# send the data set and packages to the cluster cores
parallel::clusterExport(my_cluster, "dat")
parallel::clusterEvalQ(my_cluster, {library("parallel"); library("snow")})

# fit all subsets
dredge_out = MuMIn::pdredge(full_model, cluster = my_cluster, trace = 2)

# stop the cluster, clock, and report time elapsed
parallel::stopCluster(my_cluster)
stoptime = Sys.time()
cat("Elapsed:", format(stoptime - starttime, digits = 2))

# save the output for use by other scripts
saveRDS(dredge_out, file.path(out_dir, paste0(y_variable, "-dredge-out.rds")))
