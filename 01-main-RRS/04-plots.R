# NOTE: DO NOT RUN THIS SCRIPT UNTIL OUTPUT HAS BEEN GENERATED FOR JUVENILES AND ADULTS

# clear the workspace except for key variables
rm(list = setdiff(ls(), do_not_rm))

# load dplyr
suppressPackageStartupMessages({
  suppressWarnings({
    library(dplyr)
  })
})

# which progeny types to include
progeny_types = c("total_juv", "adult")

# find the file paths to the fit and bootstrap objects
# reverse so that juveniles comes before adults
fit_files = rev(list.files(out_dir_parent, pattern = "-best-fit.rds", recursive = TRUE, full.names = TRUE))
boot_files = rev(list.files(out_dir_parent, pattern = "-boot-samps.rds", recursive = TRUE, full.names = TRUE))

# keep only those that are in progeny_types
fit_files = fit_files[stringr::str_detect(fit_files, paste(progeny_types, collapse = "|"))]
boot_files = boot_files[stringr::str_detect(boot_files, paste(progeny_types, collapse = "|"))]

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

# colors for line plots
fill_cols = c("H" = scales::alpha("salmon", 0.5), "W" = scales::alpha("royalblue", 0.5))  # polygon color
line_cols_solid = c("H" = "red", "W" = "blue")                                            # mean prediction color
line_cols_trans = scales::alpha(line_cols_solid, 0.75)                                    # border of polygon color

# in plots with 3x3 panels of years, which years should have which axes labeled?
yaxt_yrs = c(2008, 2011, 2014)
xaxt_yrs = 2014:2016

# resolution for png figures (pixels per inch)
ppi = 600

##### MODEL-PREDICTED REPRODUCTIVE SUCCESS MEASURES: CATEGORICAL COVARIATES #####

# function to summarize and plot model-predicted reproductive success measures
fitted_barplot = function(boot_out, progeny_keep, var_keep, sex_keep, legend, label_letter) {
  
  # extract model-fitted values to original data (stored as iter_0)
  fitted_vals = boot_out %>%
    filter(iter == "iter_0" & is_mean_day & is_mean_length & variable == var_keep & progeny_type == progeny_keep & sex == sex_keep)
  
  # summarize bootstrap quantiles
  q_vals = boot_out %>%
    filter(iter != "iter_0" & is_mean_day & is_mean_length & variable == var_keep & progeny_type == progeny_keep & sex == sex_keep) %>%
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
                      "resp" = "Progeny per Spawner",
                      "cond" = "Progeny per Successful Spawner",
                      "nzprb" = "Pr(Successful Spawner)"
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
  text(usr[1], usr[4] - ydiff * 0.05, paste(label_letter, label_text), pos = 4, cex = 1.2)
  
  # add a legend if requested
  if (legend) {
    legend("topright", inset = c(0, 0.1), title = "Origin", legend = c("HOR", "NOR"), pt.cex = 2, pch = 22, pt.bg = bar_cols, col = bor_cols, bty = "n")
  }
}

# FEMALES: apply plotting function to make a multipanel plot
png(file.path(fig_dir, "fitted-categories-female.png"), width = 6 * ppi, height = 7 * ppi, res = ppi)
par(mfcol = c(3,2), mar = c(1,2,1,1), oma = c(2,2,1,0), mgp = c(2,0.35,0), tcl = -0.15)
fitted_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "cond", sex_keep = "F", legend = TRUE, label_letter = "(a)")
fitted_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "nzprb", sex_keep = "F", legend = FALSE, label_letter = "(b)")
fitted_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "resp", sex_keep = "F", legend = FALSE, label_letter = "(c)")
fitted_barplot(boot_samps, progeny_keep = "adult", var_keep = "cond", sex_keep = "F", legend = FALSE, label_letter = "(d)")
fitted_barplot(boot_samps, progeny_keep = "adult", var_keep = "nzprb", sex_keep = "F", legend = FALSE, label_letter = "(e)")
fitted_barplot(boot_samps, progeny_keep = "adult", var_keep = "resp", sex_keep = "F", legend = FALSE, label_letter = "(f)")
mtext(side = 1, outer = TRUE, line = 1, "Brood Year")
mtext(side = 2, outer = TRUE, line = 0.5, "Measure of Female Reproductive Success")
mtext(side = 3, "Juvenile Progeny", outer = TRUE, line = -0.5, adj = 0.2, font = 2)
mtext(side = 3, "Adult Progeny", outer = TRUE, line = -0.5, adj = 0.85, font = 2)
dev.off()

# MALES: apply plotting function to make a multipanel plot
png(file.path(fig_dir, "fitted-categories-male.png"), width = 6 * ppi, height = 7 * ppi, res = ppi)
par(mfcol = c(3,2), mar = c(1,2,1,1), oma = c(2,2,1,0), mgp = c(2,0.35,0), tcl = -0.15)
fitted_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "cond", sex_keep = "M", legend = TRUE, label_letter = "(a)")
fitted_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "nzprb", sex_keep = "M", legend = FALSE, label_letter = "(b)")
fitted_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "resp", sex_keep = "M", legend = FALSE, label_letter = "(c)")
fitted_barplot(boot_samps, progeny_keep = "adult", var_keep = "cond", sex_keep = "M", legend = FALSE, label_letter = "(d)")
fitted_barplot(boot_samps, progeny_keep = "adult", var_keep = "nzprb", sex_keep = "M", legend = FALSE, label_letter = "(e)")
fitted_barplot(boot_samps, progeny_keep = "adult", var_keep = "resp", sex_keep = "M", legend = FALSE, label_letter = "(f)")
mtext(side = 1, outer = TRUE, line = 1, "Brood Year")
mtext(side = 2, outer = TRUE, line = 0.5, "Measure of Male Reproductive Success")
mtext(side = 3, "Juvenile Progeny", outer = TRUE, line = -0.5, adj = 0.2, font = 2)
mtext(side = 3, "Adult Progeny", outer = TRUE, line = -0.5, adj = 0.85, font = 2)
dev.off()

##### MODEL-PREDICTED REPRODUCTIVE SUCCESS MEASURES: DAY RELATIONSHIPS #####

# function to display the fitted relationship between arrival date and progeny
# for a specific progeny type, year, sex. Origin types are plotted on the same panel
day_panel = function(boot_out, progeny_keep, year_keep, sex_keep, legend, ylim) {
  
  # get predicted curve from model fitted to original data: iter_0
  fitted_vals = boot_out %>%
    filter(iter == "iter_0" & variable == "resp" & is_mean_length & !is_mean_day & progeny_type == progeny_keep & sex == sex_keep & year == year_keep) %>%
    select(origin, day, day_raw, value)
  colnames(fitted_vals)[4] = "mean"
  
  # summarize bootstraped predicted curves into a 95% confidence interval
  q_vals = boot_out %>%
    filter(iter != "iter_0" & variable == "resp" & is_mean_length & !is_mean_day & progeny_type == progeny_keep & sex == sex_keep & year == year_keep) %>%
    group_by(origin, day) %>%
    summarize(lwr = quantile(value, 0.025), upr = quantile(value, 0.975), .groups = "drop")
  
  # merge these into one data set
  plot_dat = merge(fitted_vals, q_vals, by = c("origin", "day"))
  
  # ensure proper ordering: origin type then day
  plot_dat = plot_dat[order(plot_dat$origin, plot_dat$day),]
  
  # an empty plot with correct dimensions and labels
  plot(mean ~ day_raw, data = plot_dat, type = "n", xlab = "", ylab = "", las = 1,
       xlim = round(range(dat$day_raw)), ylim = ylim,
       yaxt = ifelse(year_keep %in% yaxt_yrs, "s", "n"), xaxt = "n")
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  text(x = usr[2] + xdiff * 0.02, y = usr[4] - ydiff * 0.05, labels = year_keep, font = 2, pos = 2, cex = 1.4)
  
  # draw the fitted curve for each origin
  sapply(1:2, function(o) {
    with(subset(plot_dat, origin == origins[o]), polygon(c(day_raw, rev(day_raw)), c(lwr, rev(upr)), col = fill_cols[o], border = NA))
    lines(mean ~ day_raw, data = subset(plot_dat, origin == origins[o]), col = line_cols_solid[o], lwd = 2)
    lines(lwr ~ day_raw, data = subset(plot_dat, origin == origins[o]), col = line_cols_trans[o], lwd = 1)
    lines(upr ~ day_raw, data = subset(plot_dat, origin == origins[o]), col = line_cols_trans[o], lwd = 1)
  })
  
  # draw the legend if requested
  if (legend) {
    legend("topleft", title = "Origin", legend = c("Hatchery", "Natural"),
           pch = 22, pt.cex = 2.5, pt.bg = fill_cols, col = line_cols_trans,
           bty = "n")
  }
  
  # draw on better x-axis: show the first of each month
  # FIXME: remove StatonMisc dependency
  if (year_keep %in% xaxt_yrs) {
    all_doy = min(dat$day_raw):max(dat$day_raw)
    all_dates = StatonMisc::doy2date(all_doy, year = 2009, include_year = FALSE)
    axis_keep = which(all_dates %in% c("5/1", "6/1", "7/1", "8/1", "9/1"))
    
    axis(side = 1, at = all_doy[axis_keep], labels = c("May", "Jun", "Jul", "Aug", "Sep"))
  }
}

### APPLY THE FUNCTIONS ###

# FEMALES; TOTAL JUVENILES
png(file.path(fig_dir, "total_juv-fitted-continuous-day-females.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) day_panel(boot_samps, "total_juv", y, "F", ifelse(y == 2008, TRUE, FALSE), c(0, 12))
mtext(side = 1, outer = TRUE, line = 1.75, "Arrival Date")
mtext(side = 2, outer = TRUE, line = 1.5, "Juvenile Progeny")
dev.off()

# MALES; TOTAL JUVENILES
png(file.path(fig_dir, "total_juv-fitted-continuous-day-males.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) day_panel(boot_samps, "total_juv", y, "M", ifelse(y == 2008, TRUE, FALSE), c(0, 12))
mtext(side = 1, outer = TRUE, line = 1.75, "Arrival Date")
mtext(side = 2, outer = TRUE, line = 1.5, "Juvenile Progeny")
dev.off()

# FEMALES; ADULTS
png(file.path(fig_dir, "adult-fitted-continuous-day-females.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) day_panel(boot_samps, "adult", y, "F", ifelse(y == 2008, TRUE, FALSE), c(0, 2))
mtext(side = 1, outer = TRUE, line = 1.75, "Arrival Date")
mtext(side = 2, outer = TRUE, line = 1.5, "Adult Progeny")
dev.off()

# MALES; ADULTS
png(file.path(fig_dir, "adult-fitted-continuous-day-males.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) day_panel(boot_samps, "adult", y, "M", ifelse(y == 2008, TRUE, FALSE), c(0, 2))
mtext(side = 1, outer = TRUE, line = 1.75, "Arrival Date")
mtext(side = 2, outer = TRUE, line = 1.5, "Adult Progeny")
dev.off()

##### MODEL-PREDICTED REPRODUCTIVE SUCCESS MEASURES: LENGTH RELATIONSHIPS #####

# function to display the fitted relationship between length and progeny
# for a specific progeny type, year, sex. Origin types are plotted on the same panel
length_panel = function(boot_out, progeny_keep, year_keep, sex_keep, legend, ylim) {
  
  # get predicted curve from model fitted to original data: iter_0
  fitted_vals = boot_out %>%
    filter(iter == "iter_0" & variable == "resp" & is_mean_day & !is_mean_length & progeny_type == progeny_keep & sex == sex_keep & year == year_keep) %>%
    select(origin, length, length_raw, value)
  colnames(fitted_vals)[4] = "mean"
  
  # summarize bootstraped predicted curves into a 95% confidence interval
  q_vals = boot_out %>%
    filter(iter != "iter_0" & variable == "resp" & is_mean_day & !is_mean_length & progeny_type == progeny_keep & sex == sex_keep & year == year_keep) %>%
    group_by(origin, length) %>%
    summarize(lwr = quantile(value, 0.025), upr = quantile(value, 0.975), .groups = "drop")
  
  # merge these into one data set
  plot_dat = merge(fitted_vals, q_vals, by = c("origin", "length"))
  
  # ensure proper ordering: origin type then day
  plot_dat = plot_dat[order(plot_dat$origin, plot_dat$length),]
  
  # an empty plot with correct dimensions and labels
  plot(mean ~ length_raw, data = plot_dat, type = "n", xlab = "", ylab = "", las = 1,
       xlim = round(range(plot_dat$length_raw)), ylim = ylim,
       yaxt = ifelse(year_keep %in% yaxt_yrs, "s", "n"), xaxt = ifelse(year_keep %in% xaxt_yrs, "s", "n"))
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  text(x = usr[2] + xdiff * 0.02, y = usr[4] - ydiff * 0.05, labels = year_keep, font = 2, pos = 2, cex = 1.4)
  
  # draw the fitted curve for each origin
  sapply(1:2, function(o) {
    with(subset(plot_dat, origin == origins[o]), polygon(c(length_raw, rev(length_raw)), c(lwr, rev(upr)), col = fill_cols[o], border = NA))
    lines(mean ~ length_raw, data = subset(plot_dat, origin == origins[o]), col = line_cols_solid[o], lwd = 2)
    lines(lwr ~ length_raw, data = subset(plot_dat, origin == origins[o]), col = line_cols_trans[o], lwd = 1)
    lines(upr ~ length_raw, data = subset(plot_dat, origin == origins[o]), col = line_cols_trans[o], lwd = 1)
  })
  
  # draw the legend if requested
  if (legend) {
    legend("topleft", title = "Origin", legend = c("Hatchery", "Natural"),
           pch = 22, pt.cex = 2.5, pt.bg = fill_cols, col = line_cols_trans,
           bty = "n")
  }
}

### APPLY THE FUNCTIONS ###

# FEMALES; TOTAL JUVENILES
png(file.path(fig_dir, "total_juv-fitted-continuous-length-females.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) length_panel(boot_samps, "total_juv", y, "F", ifelse(y == 2008, TRUE, FALSE), c(0, 15))
mtext(side = 1, outer = TRUE, line = 1.75, "Length (mm)")
mtext(side = 2, outer = TRUE, line = 1.5, "Juvenile Progeny")
dev.off()

# MALES; TOTAL JUVENILES
png(file.path(fig_dir, "total_juv-fitted-continuous-length-males.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) length_panel(boot_samps, "total_juv", y, "M", ifelse(y == 2008, TRUE, FALSE), c(0, 25))
mtext(side = 1, outer = TRUE, line = 1.75, "Length (mm)")
mtext(side = 2, outer = TRUE, line = 1.5, "Juvenile Progeny")
dev.off()

# FEMALES; ADULTS
png(file.path(fig_dir, "adult-fitted-continuous-length-females.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) length_panel(boot_samps, "adult", y, "F", ifelse(y == 2008, TRUE, FALSE), c(0, 2))
mtext(side = 1, outer = TRUE, line = 1.75, "Length (mm)")
mtext(side = 2, outer = TRUE, line = 1.5, "Adult Progeny")
dev.off()

# MALES; ADULTS
png(file.path(fig_dir, "adult-fitted-continuous-length-males.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) length_panel(boot_samps, "adult", y, "M", ifelse(y == 2008, TRUE, FALSE), c(0, 2))
mtext(side = 1, outer = TRUE, line = 1.75, "Length (mm)")
mtext(side = 2, outer = TRUE, line = 1.5, "Adult Progeny")
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
    filter(iter == "iter_0" & variable == var_keep & is_mean_day & is_mean_length & progeny_type == progeny_keep) %>%
    reshape2::dcast(year + sex + iter ~ origin, value.var = "value") %>%
    mutate(rrs = get_RRS(var_type = var_keep, H_vals = H, W_vals = W))
  
  # extract/format the summaries
  q_vals = boot_out %>%
    filter(iter != "iter_0", variable == var_keep & is_mean_day & is_mean_length & progeny_type == progeny_keep) %>%
    reshape2::dcast(year + sex + iter ~ origin, value.var = "value") %>%
    mutate(rrs = get_RRS(var_type = var_keep, H_vals = H, W_vals = W)) %>%
    group_by(year, sex) %>%
    summarize(lwr = quantile(rrs, 0.025), upr = quantile(rrs, 0.975), .groups = "drop")
  
  # format RRS summaries for barplotting
  means = fitted_vals %>%
    reshape2::dcast(sex ~ year, value.var = "rrs") %>%
    select(-sex) %>% as.matrix
  lwrs = q_vals %>%
    reshape2::dcast(sex ~ year, value.var = "lwr") %>%
    select(-sex) %>% as.matrix
  uprs = q_vals %>%
    reshape2::dcast(sex ~ year, value.var = "upr") %>%
    select(-sex) %>% as.matrix
  
  # shorten the year labels
  colnames(means) = colnames(lwrs) = colnames(uprs) = paste0("'", substr(colnames(means), 3, 4))
  
  # extract the right label text
  label_text = switch(var_keep,
                      "resp" = "Progeny per Spawner",
                      "cond" = "Progeny per Successful Spawner",
                      "nzprb" = "Odds(Successful Spawner)"
  )
  
  # plot means
  mp = barplot(means, beside = TRUE, ylim = c(0, max(uprs)) * 1.15, ylab = "", col = bar_cols, border = bor_cols, las = 1)
  
  # draw uncertainty estimates
  segments(mp, lwrs, mp, uprs, col = bor_cols)
  
  # draw x-axis
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
  axis(side = 1, at = colSums(mp)/2, labels = FALSE)
  
  # add a panel label
  text(usr[1], usr[4] - ydiff * 0.05, paste(label_letter, label_text), pos = 4, cex = 1.2)
  
  # draw a reference line at 1
  abline(h = 1, lty = 2)
  
  # add a legend if requested
  if (legend) {
    legend("topright", inset = c(0, 0.1), legend = c("Male", "Female"), pt.cex = 2, pch = 22, pt.bg = bar_cols, col = bor_cols, bty = "n")
  }
}

# make the plot: shows both sexes and both progeny types so only one figure needed.
png(file.path(fig_dir, "RRS.png"), width = 6 * ppi, height = 7 * ppi, res = ppi)
par(mfcol = c(3,2), mar = c(1,2,1,1), oma = c(2,2,1,0), mgp = c(2,0.35,0), tcl = -0.15)
rrs_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "cond", legend = TRUE, label_letter = "(a)")
rrs_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "nzprb", legend = FALSE, label_letter = "(b)")
rrs_barplot(boot_samps, progeny_keep = "total_juv", var_keep = "resp", legend = FALSE, label_letter = "(c)")
rrs_barplot(boot_samps, progeny_keep = "adult", var_keep = "cond", legend = FALSE, label_letter = "(d)")
rrs_barplot(boot_samps, progeny_keep = "adult", var_keep = "nzprb", legend = FALSE, label_letter = "(e)")
rrs_barplot(boot_samps, progeny_keep = "adult", var_keep = "resp", legend = FALSE, label_letter = "(f)")
mtext(side = 1, outer = TRUE, line = 1, "Brood Year")
mtext(side = 2, outer = TRUE, line = 0.5, "Relative Reproductive Success (NOR:HOR)")
mtext(side = 3, "Juvenile Progeny", outer = TRUE, line = -0.5, adj = 0.2, font = 2)
mtext(side = 3, "Adult Progeny", outer = TRUE, line = -0.5, adj = 0.85, font = 2)
dev.off()
