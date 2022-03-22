> This repository subdirectory contains all code to reproduce the analyses informing research question #1:
>
> _Is expected reproductive output a function of variables such as origin, sex, body size, arrival date, and year of spawning?_

## Analysis Overview

The analysis is divided into multiple primary steps, each handled by a primary R script. The initial step is data preparation, handled by `00-data-prep.R`, which gets the raw data files ready for model fitting later. Executing `00-data-prep.R` requires that a variable termed `y_variable` is defined in the environment and must take on one of these values: `"total_juv"`, `"adult"`, `"parr"`, `"smolt"`, `"total_juv_grand"`, or `"adult_grand"`.  It is this trick that enables the rest of the scripts to be applicable for any of these progeny types.

1. `01-dredge.R` - Fits the global model and performs an all subsets analysis. Saves the output to be read by later scripts.
2. `02-find-best-model.R` - Filters out models that did not converge, filters the remainder into those that have delta AICc values < 2, selects the one with the fewest parameters of these, and verifies that this best model does not have assumption violations. The fitted model object of this best model is saved, along with an AICc table and a diagnostic plot.
3. `03-bootstrap-best-model.R` - Creates a prediction data set to obtain model-predicted values for the conditional and zero models and the response (combination of conditional and zero models). Performs a parametric bootstrap on the best model and returns these predicted values for each bootstrap iteration, as well as for the original data set. Bootstrapped output is saved.
4. `04-plots.R` - Contains the plotting code to produce figures shown in the manuscript and supplement. **Should not be executed until after both `"total_juv"` and `"adult"` analyses have been conducted.**
5. `05-run-analysis.R` - Calls each of the previous scripts in order given a set of command line arguments.

## How to Run the Analysis

### Via Terminal (Recommended)

This analysis is most easily reproduced by opening a terminal window in the R project directory and executing:

```bash
Rscript 01-main-RRS/05-run-analysis.R total_juv 5 FALSE
```

Which will run the entire analysis for the total juvenile progeny stage using 5 CPU cores and without making the final plots. Adjust the CPU cores accordingly for your machine. When this is completed (should not take more than several hours, the bootstrap is the most time-intensive step), perform the adult progeny stage analysis and this time specify that you wish to have the output plots generated:

```bash
Rscript 01-main-RRS/05-run-analysis.R adult 5 TRUE
```

If you wish to render the supplement yourself, you will also need to run these two lines similarly as above (this performs the sensitivity analysis of disaggregating total juvenile progeny assignments into their component parts of parr and smolt):

```bash
Rscript 01-main-RRS/05-run-analysis.R parr 5 FALSE
```

```bash
Rscript 01-main-RRS/05-run-analysis.R smolt 5 FALSE
```

### Without Terminal

If you do not have access to a terminal, you can still run the analysis in an interactive R session. Open the `05-run-analysis.R` file, ensure the working directory is set to the R project directory, and replace this line:

```R
args = commandArgs(trailingOnly = TRUE)
```

with this:

```R
args = c("total_juv", "5", "FALSE")
```

and execute the entire script to run the total juvenile progeny analysis. This can be repeated for the other progeny types as described above.