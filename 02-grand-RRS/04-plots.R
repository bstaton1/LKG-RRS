# NOTE: DO NOT RUN THIS SCRIPT UNTIL OUTPUT HAS BEEN GENERATED FOR JUVENILES AND ADULTS

# clear the workspace
rm(list = setdiff(ls(), do_not_rm))

# load dplyr
suppressPackageStartupMessages({
  suppressWarnings({
    library(dplyr)
  })
})

# which progeny types to include
progeny_types = c("total_juv_grand", "adult_grand")

# find the file paths to the fit and bootstrap objects
# reverse so that juveniles comes before adults
fit_files = rev(list.files(out_dir_parent, pattern = "-best-fit.rds", recursive = TRUE, full.names = TRUE))
boot_files = rev(list.files(out_dir_parent, pattern = "-boot-samps.rds", recursive = TRUE, full.names = TRUE))

# read in output from other scripts
best_fit = lapply(fit_files, function(i) readRDS(i))         # best model per progeny type
boot_samps = lapply(boot_files, function(i) readRDS(i))      # bootstrap predictions from best model per progeny type

# add a progeny_type variable to bootstrapped output and combine into one dataframe
boot_samps = lapply(1:length(progeny_types), function(i) {boot_samps[[i]]$progeny_type = progeny_types[i]; boot_samps[[i]]})
boot_samps = do.call(rbind, boot_samps)

# define plotting objects
# origin types
origins = c("H", "W")

# colors for bar plots
bar_cols = c(H = "grey50", W = "grey85")
bor_cols = c(H = "grey30", W = "grey60")

# resolution for png figures (pixels per inch)
ppi = 600

##### MODEL-PREDICTED REPRODUCTIVE SUCCESS MEASURES: CATEGORICAL COVARIATES #####

progeny_keep = "adult_grand"
var_keep = "resp"
boot_out = boot_samps


# function to summarize and plot model-predicted reproductive success measures
fitted_barplot = function(boot_out, progeny_keep, var_keep, legend, label_letter) {

  # extract model-fitted values to original data (stored as iter_0)
  fitted_vals = boot_out %>%
    filter(iter == "iter_0" & variable == var_keep & progeny_type == progeny_keep)

  # summarize bootstrap quantiles
  q_vals = boot_out %>%
    filter(iter != "iter_0" & variable == var_keep & progeny_type == progeny_keep) %>%
    group_by(year, origin) %>%
    summarise(lwr = quantile(value, 0.025), upr = quantile(value, 0.975), .groups = "drop")

  # extract/format the summaries for barplotting
  # origins as rows, years as columns
  means = fitted_vals %>%
    reshape2::dcast(origin ~ year, value.var = "value") %>%
    select(-origin) %>% as.matrix
  lwrs = q_vals %>%
    reshape2::dcast(origin ~ year, value.var = "lwr") %>%
    select(-origin) %>% as.matrix
  uprs = q_vals %>%
    reshape2::dcast(origin ~ year, value.var = "upr") %>%
    select(-origin) %>% as.matrix

  # shorten the year labels
  colnames(means) = colnames(lwrs) = colnames(uprs) = paste0("'", substr(colnames(means), 3, 4))

  # select the right label text
  label_text = switch(var_keep,
                      "resp" = "Grand-Progeny per Spawner",
                      "cond" = "Grand-Progeny per Successful Spawner",
                      "nzprb" = "Pr(Successfully Spawning Grand-Progeny)"
  )

  # plot means
  mp = barplot(means, beside = TRUE, ylim = c(0, max(uprs)) * 1.1, ylab = "", col = bar_cols, border = bor_cols, las = 1)

  # draw uncertainty estimates
  segments(mp, lwrs, mp, uprs, col = bor_cols)

  # add an x-axis
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
  axis(side = 1, at = colSums(mp)/2, labels = FALSE)

  # add a label to identify this panel
  text(usr[1], usr[4] - ydiff * 0.05, paste(label_letter, label_text), pos = 4, cex = 1)

  # add a legend if requested
  if (legend) {
    legend("topright", title = "Origin", legend = c("Hatchery", "Natural"), pt.cex = 2, pch = 22, pt.bg = bar_cols, col = bor_cols, bty = "n")
  }
}

# apply plotting function to make a multipanel plot
png(file.path(fig_dir, "fitted-categories.png"), width = 6 * ppi, height = 7 * ppi, res = ppi)
par(mfcol = c(3,2), mar = c(1,2,1,1), oma = c(2,2,1,0), mgp = c(2,0.35,0), tcl = -0.15)
fitted_barplot(boot_samps, progeny_keep = "total_juv_grand", var_keep = "resp", legend = FALSE, label_letter = "(a)")
fitted_barplot(boot_samps, progeny_keep = "total_juv_grand", var_keep = "cond", legend = FALSE, label_letter = "(b)")
fitted_barplot(boot_samps, progeny_keep = "total_juv_grand", var_keep = "nzprb", legend = FALSE, label_letter = "(c)")
fitted_barplot(boot_samps, progeny_keep = "adult_grand", var_keep = "resp", legend = TRUE, label_letter = "(d)")
fitted_barplot(boot_samps, progeny_keep = "adult_grand", var_keep = "cond", legend = FALSE, label_letter = "(e)")
fitted_barplot(boot_samps, progeny_keep = "adult_grand", var_keep = "nzprb", legend = FALSE, label_letter = "(f)")
mtext(side = 1, outer = TRUE, line = 1, "Brood Year")
mtext(side = 2, outer = TRUE, line = 0.5, "Measure of Reproductive Success")
mtext(side = 3, "Juvenile Grand-Progeny", outer = TRUE, line = -0.5, adj = 0.15, font = 2)
mtext(side = 3, "Adult Grand-Progeny", outer = TRUE, line = -0.5, adj = 0.875, font = 2)
dev.off()

##### MODEL-PREDICTED RELATIVE REPRODUCTIVE SUCCESS MEASURES #####

# function to calculate RRS
get_RRS = function(var_type, H_vals, W_vals) {
  # convert values to odds-scale if var_type == "nzprb"
  if (var_type == "nzprb") {
    H_vals = H_vals/(1 - H_vals)
    W_vals = W_vals/(1 - W_vals)
  }
  W_vals/H_vals
}

# function to make one barplot panel
rrs_barplot = function(boot_out, progeny_keep, var_keep, legend, label_letter) {

  # calculate RRS for fitted values from model fitted to original data
  fitted_vals = boot_out %>%
    filter(iter == "iter_0" & variable == var_keep & progeny_type == progeny_keep) %>%
    reshape2::dcast(year + iter ~ origin, value.var = "value") %>%
    mutate(rrs = get_RRS(var_type = var_keep, H_vals = H, W_vals = W))

  # extract/format the summaries
  q_vals = boot_out %>%
    filter(iter != "iter_0", variable == var_keep & progeny_type == progeny_keep) %>%
    reshape2::dcast(year + iter ~ origin, value.var = "value") %>%
    mutate(rrs = get_RRS(var_type = var_keep, H_vals = H, W_vals = W)) %>%
    group_by(year) %>%
    summarize(lwr = quantile(rrs, 0.025), upr = quantile(rrs, 0.975), .groups = "drop")

  # format RRS summaries for barplotting
  means = fitted_vals$rrs
  lwrs = q_vals$lwr
  uprs = q_vals$upr

  # shorten the year labels
  names(means) = names(lwrs) = names(uprs) = paste0("'", substr(q_vals$year, 3, 4))

  # extract the right label text
  label_text = switch(var_keep,
                      "resp" = "Grand-Progeny per Spawner",
                      "cond" = "Grand-Progeny per Successful Spawner",
                      "nzprb" = "Odds(Successful Spawner)"
  )

  if (all(means == 1)) {
    ylim = c(0, 2)
  } else {
    ylim = c(0, max(uprs))
  }

  # plot means
  mp = barplot(means, beside = TRUE, ylim = ylim * 1.1, ylab = "", col = bar_cols[1], border = bor_cols[1], las = 1)

  # draw uncertainty estimates
  segments(mp, lwrs, mp, uprs, col = bor_cols[1])

  # draw x-axis
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
  axis(side = 1, at = mp, labels = FALSE)

  # add a panel label
  text(usr[1], usr[4] - ydiff * 0.05, paste(label_letter, label_text), pos = 4, cex = 1)

  # draw a reference line at 1
  abline(h = 1, lty = 2)
}

# make the plot: shows both sexes and both progeny types so only one figure needed.
png(file.path(fig_dir, "RRS.png"), width = 6 * ppi, height = 7 * ppi, res = ppi)
par(mfcol = c(3,2), mar = c(1,2,1,1), oma = c(2,2,1,0), mgp = c(2,0.35,0), tcl = -0.15)
rrs_barplot(boot_samps, progeny_keep = "total_juv_grand", var_keep = "resp", label_letter = "(a)")
rrs_barplot(boot_samps, progeny_keep = "total_juv_grand", var_keep = "cond", label_letter = "(b)")
rrs_barplot(boot_samps, progeny_keep = "total_juv_grand", var_keep = "nzprb", label_letter = "(c)")
rrs_barplot(boot_samps, progeny_keep = "adult_grand", var_keep = "resp", label_letter = "(d)")
rrs_barplot(boot_samps, progeny_keep = "adult_grand", var_keep = "cond", label_letter = "(e)")
rrs_barplot(boot_samps, progeny_keep = "adult_grand", var_keep = "nzprb", label_letter = "(f)")
mtext(side = 1, outer = TRUE, line = 1, "Brood Year")
mtext(side = 2, outer = TRUE, line = 0.5, "Relative Reproductive Success")
mtext(side = 3, "Juvenile Grand-Progeny", outer = TRUE, line = -0.5, adj = 0.15, font = 2)
mtext(side = 3, "Adult Grand-Progeny", outer = TRUE, line = -0.5, adj = 0.875, font = 2)
dev.off()
