
# load the data set: the same as in 01-main-RRS
# the "y_variable" object isn't relevant for this 
# analysis but is still needed to run that script
y_variable = "total_juv"
do_not_rm = "y_variable"
source("01-main-RRS/00-data-prep.R")
rm(y_variable); dat = dat[,-which(colnames(dat) == "y_var")]

# retain only spawners that had non-zero progeny assigned to them
dat = subset(dat, total_juv > 0)

# fit the GLMM: assumes relationship between adults and total juveniles produced per spawner
# with random slopes and intercepts for each year
# assessed using poisson but evidence of overdispersion was found.
fit = glmmTMB::glmmTMB(adult ~ total_juv + (1 + total_juv|year), data = dat,
                       family = glmmTMB::nbinom2)
summary(fit)

# save the fitted model object for use in other output
saveRDS(fit, "04-adult-vs-juv/model.rds")

# obtain standardized residuals
resids = DHARMa::simulateResiduals(fit)

# plot standard diagnostics
plot(resids)

# test for evidence of zero-inflation: none found
DHARMa::testZeroInflation(resids, plot = FALSE)

# create a prediction data set: for drawing the fitted model over top of data
# loop through years and create a sequence from the minimum to maximum total juveniles in that year
pred_data = NULL
for (y in unique(dat$year)) {
  tmp = data.frame(
    year = y,
    total_juv = seq(min(dat$total_juv[dat$year == y]), max(dat$total_juv[dat$year == y]))
  )
  
  # combine with prediction data sets from other years
  pred_data = rbind(pred_data, tmp)
}

# produce fitted values on the fish scale. 
preds = predict(fit, pred_data, type = "link", se.fit = TRUE)
pred_data$mean = exp(preds$fit)
pred_data$lwr95ci = exp(preds$fit + qnorm(0.025) * preds$se.fit)
pred_data$upr95ci = exp(preds$fit + qnorm(0.975) * preds$se.fit)

# which years have which axes? don't draw axes on internal panels
yaxt_yrs = c(2008, 2011, 2014)
xaxt_yrs = 2014:2016

# function to create a scatterplot with fitted curve for one year
fit_plot = function(y) {
  # subet the data for this year
  dat_sub = subset(dat, year == y)
  
  # an empty plot with correct dimensions/labeling
  plot(1,1, type = "n", xlim = range(dat$total_juv), ylim = range(dat$adult),
       xlab = "", ylab = "", las = 1,
       yaxt = ifelse(y %in% yaxt_yrs, "s", "n"),
       xaxt = ifelse(y %in% xaxt_yrs, "s", "n"))
  
  # draw on the fitted curve + polygon for uncertainty region
  with(subset(pred_data, year == y), {
    polygon(c(total_juv, rev(total_juv)), c(lwr95ci, rev(upr95ci)), border = NA, col = scales::alpha("grey50", 0.25))
    lines(mean ~ total_juv, data = subset(pred_data, year == y), lwd = 2)
    lines(lwr95ci ~ total_juv, data = subset(pred_data, year == y), col = "grey")
    lines(upr95ci ~ total_juv, data = subset(pred_data, year == y), col = "grey")
  })
  
  # add data points
  points(adult ~ total_juv, data = dat_sub,
         pch = 16, col = scales::alpha("grey20", 0.25), cex = 1.8)
  
  # add year label text
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  text(x = usr[2] + xdiff * 0.02, y = usr[4] - ydiff * 0.05, labels = y, font = 2, pos = 2, cex = 1.4)
}

# make the plot
ppi = 600
png("04-adult-vs-juv/adult-vs-juv.png", width = 7.5 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(3,3), oma = c(3,3,0,0), mgp = c(2,0.35,0), tcl = -0.15, mar = rep(0.25, 4), cex.axis = 1.2)
for (y in unique(dat$year)) fit_plot(y)
mtext(side = 1, outer = TRUE, line = 1.75, "Total Juvenile Progeny Assigned")
mtext(side = 2, outer = TRUE, line = 1.5, "Total Adult Progeny Assigned")
dev.off()
