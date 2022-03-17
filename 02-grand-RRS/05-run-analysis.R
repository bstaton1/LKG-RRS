# ensure the workspace is totally clean
rm(list = ls(all = TRUE))

# set the random seed for exact reproducibility
set.seed(1234)

# load custom functions that are used in multiple analyses
source("common-functions.R")

# read command line arguments
args = commandArgs(trailingOnly = TRUE)
y_variable = args[1]
ncpus = as.numeric(args[2])
make_figures = as.logical(args[3])

# create a main model output directory if doesn't exist
out_dir_parent = "02-grand-RRS/model-output"
if (!dir.exists(out_dir_parent)) dir.create(out_dir_parent)

# create a subdirectory for all output specific to this response y_variable
out_dir = file.path(out_dir_parent, y_variable)
if (!dir.exists(out_dir)) dir.create(out_dir)

# objects to keep 
do_not_rm = c("do_not_rm", "y_variable", "dat", "ncpus", "make_figures", "out_dir_parent", "out_dir", "fig_dir", custom_fn_names, "custom_fn_names")

# prepare the data set
cat("Preparing Data\n")
source("01-main-RRS/00-data-prep.R")
dat = na.omit(dat); dat = droplevels(dat)

# fit all subsets
cat("Fitting All Subsets\n")
source("02-grand-RRS/01-dredge.R")

# select the best model
cat("\nSelecting the Best Model\n")
source("02-grand-RRS/02-find-best-model.R")

# obtain bootstrapped predictions from model
cat("\nPerforming Parametric Bootstrap\n")
source("02-grand-RRS/03-bootstrap-best-model.R")

# determine if output for both progeny types now exists
both_analyses_done = all(c("total_juv_grand", "adult_grand") %in% basename(list.dirs(out_dir_parent)))

# return message if figures requested but cannot be made
if (!both_analyses_done & make_figures) {
  message("\nFigures were requested, however output from only one of the two progeny types was found. No output figures will be produced.")
}

# produce figures if requested and able
if (make_figures & both_analyses_done) {
  # create a subdirectory for all final figures
  fig_dir = file.path(out_dir, "..", "..", "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir)

  cat("\nProducing Final Figures\n")
  source("02-grand-RRS/04-plots.R")
}
