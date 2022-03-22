> This repository subdirectory contains all code to reproduce the analyses informing research question #4:
>
> _Is reproductive output to the juvenile stage a good indicator of reproductive output to the adult stage?_

## Analysis Overview

The analysis for this question is simpler than for the previous questions in the sense that only one model is fitted and the plotting code is in the same script as the model fitting code.

`adult-vs-juv_fit-plot.R` - prepares the data file, fits a negative binomial mixed effects model with random slopes and intercepts for each year (`adult ~ total_juv + (1 + total_juv|year)`), saves the fitted model object, runs residual diagnostics, obtains model-predicted values at a range of juvenile progeny levels for each year (with 95% CIs), and produces the plot found in the manuscript.

## How to Run the Analysis

Unlike the other analyses in this repo, this analysis should be executed in an interactive R session. Simply open R(Studio), ensure the working directory is set to the R project location, and execute the script, either all at once or line by line.