# clear the workspace
rm(list = ls(all = TRUE))

# load dplyr: makes summarizing value for plot easier
suppressWarnings({
  suppressPackageStartupMessages({
    library(dplyr)
  })
})

# set directory to save output
out_dir = "03-cross-types/model-output"

# file names
in_files = list.files(out_dir, pattern = "boot", full.names = TRUE)

# read in both progeny types and combine into one data set
boot_out = do.call(rbind, lapply(in_files, readRDS))

# extract only the columns that store the ratios
ratio_cols = stringr::str_which(colnames(boot_out), "\\:")
ratios_only = boot_out[,c("progeny_type", "year", "iter", colnames(boot_out)[ratio_cols])]

# convert to long format
ratios = reshape2::melt(ratios_only, id.vars = c("year", "progeny_type", "iter"), value.name = "rrs", variable.name = "type")

# extract the numerator and denominator of the ratios
ref = unique(substr(ratios$type, 5, 7))
ratios$type = substr(ratios$type, 1, 3)

# ordered cross_types
ordered_types = c("NxN", "HxN", "NxH", "HxH")

# assign order to cross type
ratios$type = factor(ratios$type, levels = ordered_types[-which(ordered_types == ref)])

# set colors
bar_cols = c("grey40", "grey70", "grey90")
border_cols = c("black", "grey30", "grey60")

# set resolution
ppi = 600

# prepare the estimates for barplotting
prep_for_barplot = function(progeny_keep) {
  # obtain the point estimates of ratios based on original data set
  # formatted for barplotting
  means = ratios %>%
    filter(iter == "iter_0" & progeny_type == progeny_keep) %>%
    reshape2::dcast(type ~ year, value.var = "rrs") %>%
    select(-type) %>%
    as.matrix
  
  # summarize: obtain endpoints of 95% bootstrap CI
  q_vals = ratios %>%
    # remove the point estimates from original data prior to summarizing
    filter(iter != "iter_0" & progeny_type == progeny_keep) %>%
    group_by(year, type) %>%
    summarize(lwr = quantile(rrs, 0.025, na.rm = TRUE), upr = quantile(rrs, 0.975, na.rm = TRUE), .groups = "drop")
  
  # build matrices of 95% CLs
  # formatted for barplotting
  lwrs = q_vals %>%
    reshape2::dcast(type ~ year, value.var = "lwr") %>%
    select(-type) %>%
    as.matrix
  uprs = q_vals %>%
    reshape2::dcast(type ~ year, value.var = "upr")%>%
    select(-type) %>%
    as.matrix
  
  # since the best model doesn't include cross_type * year, ratios are the same for all years
  # just return and plot 1 year
  out = cbind(means = means[,"2008"], lwrs = lwrs[,"2008"], uprs = uprs[,"2008"])
  rownames(out) = levels(ratios$type)
  out
}

# extract the summaries from fitted model and bootstrap
bp_ratios = abind::abind(Juv = prep_for_barplot("Juv"), Adult = prep_for_barplot("Adult"), along = 3)

# make the plot
png(file.path(out_dir, "cross-type-RRS.png"), width = 4 * ppi, height = 4 * ppi, res = ppi)
par(mar = c(3,3,1,1), mgp = c(2,0.35,0), tcl = -0.15, cex.axis = 0.8)
mp = barplot(bp_ratios[,"means",], beside = TRUE, ylim = c(0, max(bp_ratios[,"uprs",])) * 1.1, col = bar_cols, border = border_cols,
             las = 1, xaxt = "n", xlab = "Progeny Type", ylab = ("Reproductive Success Relative to HxH"))
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
abline(h = 1, lty = 2)
segments(mp, bp_ratios[,"lwrs",], mp, bp_ratios[,"uprs",], col = border_cols)
axis(side = 1, at = colMeans(mp), labels = c("Juvenile", "Adult"))
legend("topright", cex = 0.8, title = "Numerator in Ratio", legend = levels(ratios$type), pt.cex = 2, pch = 22, col = border_cols, pt.bg = bar_cols, bty = "n", horiz = TRUE)
dev.off()
