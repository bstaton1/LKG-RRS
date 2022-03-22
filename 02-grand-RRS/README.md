>This repository subdirectory contains all code to reproduce the analyses informing research question #2:
>
>_Do any reproductive output differences found in question #1 carry through to later generations?_

## Analysis Overview

The analysis is divided into multiple primary steps, each handled by a primary R script. The initial step is data preparation, handled by the `01-main-RRS/00-data-prep.R` script.

The structure of this analysis is very similar to that for question #1 (`01-main-RRS`) but uses grand-progeny rather than progeny and uses a simpler global model. Please see that subdirectory for more details on the purpose of each file.

## How to Run the Analysis

### Via Terminal (Recommended)

This analysis is most easily reproduced by opening a terminal window in the R project directory and executing:

```bash
Rscript 02-grand-RRS/05-run-analysis.R total_juv_grand 5 FALSE
```

Which will run the entire analysis for the total juvenile grand-progeny stage using 5 CPU cores and without making the final plots. Adjust the CPU cores accordingly for your machine. When this is completed (should not take more than several minutes, the bootstrap is the most time-intensive step), perform the adult grand-progeny stage analysis and this time specify that you wish to have the output plots generated:

```bash
Rscript 02-grand-RRS/05-run-analysis.R adult_grand 5 TRUE
```

### Without Terminal

If you do not have access to a terminal, you can still run the analysis in an interactive R session. Open the `05-run-analysis.R` file, ensure the working directory is set to the R project directory, and replace this line:

```R
args = commandArgs(trailingOnly = TRUE)
```

with this:

```R
args = c("total_juv_grand", "5", "FALSE")
```

and execute the entire script to run the total juvenile grand-progeny analysis. This can be repeated for the adult grand-progeny type as described above.