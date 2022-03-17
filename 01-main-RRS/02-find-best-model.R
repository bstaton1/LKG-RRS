# clear the workspace except for key variables
rm(list = setdiff(ls(), do_not_rm))

# find the model selection output for this response variable
dredge_file = list.files(out_dir, pattern = paste0("^", y_variable, ".+dredge"), full.names = TRUE)

# read in the contents of this file
dredge_out = readRDS(dredge_file)

##### STEP #1: IDENTIFY WHICH MODELS DIDN'T CONVERGE AND REMOVE FROM MODEL SET #####

# find the models that didn't fit
bad_mods = find_problem_models(dredge_out)

# remove them so aren't considered in inference
dredge_out_use = dredge_out[which(!(rownames(dredge_out) %in% bad_mods)),]

# identify which models didn't fit based which variables they contain
p_bad_mods_with_var(dredge_out, bad_mods)

##### STEP #2: SELECT TOP MODEL #####

# find the set of top models: 
# those that are within two AICc units of the minimum AICc model
dredge_out_top = dredge_out_use[(dredge_out_use$AICc - min(dredge_out_use$AICc)) < 2,]

# build simple AIC table for top models
AIC_tab = simple_AIC_table(dredge_out_top)

# extract the ID of the best model:
# single model with the fewest parameters among the top models
best_mod = rownames(dredge_out_top[which.min(dredge_out_top$df),])

# extract the best fitted model
best_fit = MuMIn::get.models(dredge_out_top[best_mod,], subset = TRUE)[[best_mod]]

##### STEP #3: PERFORM RESIDUAL DIAGNOSTICS FOR THE TOP MODEL #####

cat("Calculating Standardized Residuals\n")

# get the residual object
resids = make_resids(best_fit)

# create the plot file and save the p-values of formal tests
pdf(file.path(out_dir, paste0(y_variable, "-diag-plots.pdf")), w = 8, h = 4)
diag_p_vals = diag_resids(resids)
dev.off()

# print the p-values to the console
cat("DHARMa Residual Diagnostic Test P-Values:")
cat("\n  Uniformity:", diag_p_vals["uniformity"])
cat("\n  Dispersion:", diag_p_vals["dispersion"])
cat("\n  Outliers:", diag_p_vals["outliers"])

##### STEP 4: SAVE THE FITTED MODEL OBJECT FOR THE BEST MODEL #####

# save the object storing this best model for use in later scripts
saveRDS(best_fit, file = file.path(out_dir, paste0(y_variable, "-best-fit.rds")))

# save the AIC table
write.csv(AIC_tab, file.path(out_dir, paste0(y_variable, "-AIC-table.csv")))
