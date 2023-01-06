# FUNCTIONS USED IN MULTIPLE ANALYSES
# DEFINE THEM HERE SO THEY AREN'T DUPLICATED IN THE SCRIPTS FOR EACH ANALYSIS

##### FIND MODELS THAT HAD A MODEL FITTING PROBLEM #####

# function to find models that did not converge
# dredge_output: the object returned by MuMIn::dredge() or MuMIn::pdredge()

find_problem_models = function(dredge_output) {
  
  # extract the warnings from each model
  warn_text = names(attributes(dredge_output)$warnings)
  
  # find the warnings that include convergence problems
  cnvg_warns = warn_text[stringr::str_detect(warn_text, "Model convergence problem")]
  
  # extract the part of the warning that includes the model identifier
  model_text = stringr::str_extract(cnvg_warns, "\\(in model .+$")
  
  # find the model identifier
  bad_models = unique(stringr::str_extract(model_text, "[:digit:]+"))
  
  # print a message
  message(length(bad_models), " out of ", nrow(dredge_output), " models failed to converge")
  
  # return the ID's of the models that didn't converge
  return(bad_models)
}

##### IDENTIFY WHICH VARIABLES WERE IN MODELS THAT HAD A MODEL FITTING PROBLEM #####

# function returns a vector of terms where the value is 
# the proportion of models that contained that 
# term that had a model fitting issue.
# dredge_output: the output of MuMIn::dredge() or MuMIn::pdredge()
# bad_mods: the output of find_problem_models()

p_bad_mods_with_var = function(dredge_output, bad_mods) {
  # determine if each model was bad
  is_bad_model = rownames(dredge_output) %in% bad_mods
  
  # extract the coefficient table
  df = as.data.frame(dredge_output)
  df = df[,-which(colnames(df) %in% c("cond((Int))", "zi((Int))", "disp((Int))", "df", "logLik", "AICc", "delta", "weight"))]
  
  # create a logical table: TRUE means that variable (column) was in that model (row)
  var_in_model = t(apply(df, 1, function(betas) !is.na(betas)))
  
  # create a logical table: TRUE means that variable was in that model AND that model was a bad model
  var_in_bad_model = apply(var_in_model, 2, function(in_model) in_model & is_bad_model)
  
  # keep = !var_in_model[,"cond(I(day^2))"]
  keep = rep(TRUE, nrow(var_in_model))
  
  # count up the totals
  models_containing = colSums(var_in_model[keep,])
  bad_models_containing = colSums(var_in_bad_model[keep,])
  
  # divide to get a proportion of models with that variable that had a model fitting problem
  bad_models_containing/models_containing
}

##### CONSTRUCT OBJECT CONTAINING THE TERMS IN EACH COMPONENT OF EACH MODEL #####

# dredge_output: the output of MuMIn::dredge() or MuMIn::pdredge()

get_formulas = function(dredge_output) {
  t(sapply(1:nrow(dredge_output), function(i) {
    form_split = as.character(attr(dredge_output, "model.calls")[[i]])
    out = c(cond_form = form_split[2], zi_form = form_split[5], dist = form_split[4])
    
    out = stringr::str_remove(out, "^y_var ~ ")
    out = stringr::str_remove(out, "~")
    out = stringr::str_remove(out, " \\+ 1")
    out = stringr::str_remove(out, "glmmTMB\\:\\:")
    
    out
  }))
}

##### CONSTRUCT A SIMPLIFIED AIC TABLE #####

# dredge_output: the output of MuMIn::dredge() or MuMIn::pdredge()

simple_AIC_table = function(dredge_out) {
  out = data.frame(get_formulas(dredge_out), n_params = dredge_out$df, AICc = dredge_out$AICc,
                   delta_AICc = dredge_out$AICc - min(dredge_out$AICc),  weight = dredge_out$weight)
  colnames(out)[1:3] = c("conditional", "zeroinf", "dist")
  out
}

##### GENERATE DHARMa STANDARDIZED RESIDUALS FROM A FITTED MODEL #####

# fit: a fitted model object
# nsim: the number of random data sets to simulate from the model for calculating eCDF-standardized residuals
make_resids = function(fit, nsim = 1000) {
  
  # simulate new response variable
  sim_response = as.matrix(simulate(fit, nsim = nsim))
  
  # build DHARMa residual object
  resids = DHARMa::createDHARMa(
    simulatedResponse = sim_response,
    observedResponse = fit$frame[,1], 
    fittedPredictedResponse = predict(fit, newdata = fit$frame, type = "response"),
    integerResponse = TRUE,
    method = "traditional"
  )
  
  # return residual object
  return(resids)
}

##### CREATE DHARMa DIAGNOSTIC PLOTS AND RETURN P-VALUES #####

# resids: the output of make_resids()

diag_resids = function(resids, return_pvals = TRUE) {
  
  # produce the diagnostic plots
  par(mfrow = c(1,2))
  DHARMa::plotQQunif(resids)
  DHARMa::plotResiduals(resids, rank = TRUE, quantreg = TRUE)
  
  # extract and return the p-values of the diagnostic tests
  if (return_pvals) {
    out = c(
      uniformity = DHARMa::testUniformity(resids, plot = FALSE)$p.value,
      dispersion = DHARMa::testDispersion(resids, plot = FALSE)$p.value,
      outliers = DHARMa::testOutliers(resids, type = "binomial", plot = FALSE)$p.value
    )
    return(out)
  }
}

# obtain the names of all of these objects so they are not removed later
custom_fn_names = ls()


