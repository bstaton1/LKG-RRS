> This repository stores the code used to perform the GL(M)M analyses presented in the manuscript _Improved productivity of naturalized spring Chinook salmon following reintroduction from a hatchery stock in Lookingglass Creek, Oregon_ by H.M. Nuetzel, P.F. Galbreath, B.A. Staton, C.A. Crump, L.M. Naylor, and G.E. Shippentower, accepted for publication in the _Canadian Journal of Fisheries and Aquatic Sciences_.

[![ArticleDOI](https://img.shields.io/badge/Article%20DOI-10.1139%2Fcjfas--2022--0114-blue)](https://doi.org/10.1139/cjfas-2022-0114)

[![GitHub Repo Archive DOI](https://img.shields.io/badge/GitHub%20Repo%20Archive%20DOI-10.5281%2Fzenodo.6621724-blue)](https://doi.org/10.5281/zenodo.6621724)

## Data Ownership

To conform with data sharing practices followed by the data owners, the Confederated Tribes of the Umatilla Indian Reservation (CTUIR) & the Oregon Department of Fish and Wildlife, and to honor principles of Indigenous Data Sovereignty, the raw data files are not available in this repository. If you would like access to these data so that you may execute the scripts with that data set, please contact the following: Hayley Nuetzel at [hnuetzel@critfc.org](mailto:hnuetzel@critfc.org), Gene Shippentower at [geneshippentower@ctuir.org](mailto:geneshippentower@ctuir.org), Les Naylor at [lesnaylor@ctuir.org](mailto:lesnaylor@ctuir.org), and Carrie Crump at [carriecrump@ctuir.org](mailto:carriecrump@ctuir.org).

The input files to the relative reproductive success analyses found in this repo contain the results from parentage assignment analyses, which can be reproduced using the code found in the repo [hnuetzel/Lookingglass-Creek-spring-Chinook-RRS](https://github.com/hnuetzel/Lookingglass-Creek-spring-Chinook-RRS).

## Repo Organization

This repository is organized into four primary analyses, each reflecting the main research questions laid out in the manuscript. Each subdirectory contains complete code to run the analysis and generate the plots found in the main text and supplement.

* `01-main-RRS` - code to conduct an all subsets AIC analysis using negative binomial hurdle models. The models explain among-spawner variability in their expected reproductive output to the juvenile or adult stages, and are used to generate ratios between NOR and HOR spawners that control for effects of factors like sex, arrival date, and spawner size.
* `02-grand-RRS` - code to conduct a similar analysis to that found in `01-main-RRS`, except for grand-progeny per spawner. The global models are still negative binomial hurdle models, but consider fewer covariates.
* `03-cross-types` - code to estimate the expected reproductive output of spawning pairs with differing origins of the parents. The models are zero-truncated negative binomial GLMs (no hurdle used because there were no zeros in this analysis).
* `04-adult-vs-juv` - code to estimate the relationship between adult and juvenile progeny assigned to individual spawners. The model is a negative binomial mixed-effects model with random slopes and intercepts for each year.

The sub-directory titled `05-supplement` contains the source code to reproduce the supplementary material that accompanies the manuscript.

The raw data files are contained in the `00-data` subdirectory.

The file `common-functions.R` contains a variety of custom functions that facilitate multiple analyses.

## Reproducibility

The output files are too large to track in a git repo, so this code will need to be executed locally to reproduce the output presented in the manuscript and supplement. Please see the README files in each of the subdirectories listed above for more details.

All analyses that use random number generators (e.g., parametric bootstrapping) include a `set.seed()` statement, so our results should be exactly reproducible.

## Dependencies

All GL(M)M analyses were conducted in R (v4.0.2). Many R packages were used, listed below in general order of their centrality to our analysis (i.e., core model fitting/diagnostics packages listed before general-purpose data preparation packages).

> The original code for analysis as published (tagged as [v1.0](https://github.com/bstaton1/LKG-RRS/releases/tag/v1.0) and archived under DOI: [10.5281/zenodo.7278182](https://10.5281/zenodo.7278182)) used `glmmTMB` v1.0.2.1, which contained a bug when obtaining conditional (and as a result, response) expected values for hurdle models (see [#634](https://github.com/glmmTMB/glmmTMB/issues/634) for the original mention in the developers' issue tracker); we independently discovered the same issue when undertaking the analysis for the manuscript and we implemented an ad-hoc fix to prevent this from affecting our results.
>
> However, since publication, `glmmTMB` has been updated on CRAN (to v1.1.5 at time of writing) and the bug no longer exists, but the (now unnecessary) fix we implemented for the old version gives the wrong inference with `glmmTMB` >= v1.1.5.  Please see [here](https://gist.github.com/bstaton1/9c745315797bef59c0107e0426e38bee) for a complete description of the bug that was present in the old version in the context of our analysis, the solution we implemented to ensure valid inference at the time of publication, and an illustration that the fix we implemented is no longer needed with `glmmTMB` >=v1.1.5.
>
> **This version of the analysis code has the ad-hoc fix removed, thus, it is of critical importance that you use `glmmTMB` >= v1.1.5 to reproduce our results. This version has been tested with R v4.2.2 and the listed versions of the below packages and completely replicates the results published in the article.**

| Package Name | Version Used | Purpose                                                      |
| ------------ | ------------ | ------------------------------------------------------------ |
| `glmmTMB`    | 1.1.5        | Fitting of GL(M)Ms                                           |
| `MuMIn`      | 1.47.1       | Model selection tasks                                        |
| `DHARMa`     | 0.4.6        | Model assumption diagnostics                                 |
| `lme4`       | 1.1.31       | Parametric bootstrapping via `lme4::boot.mer()`              |
| `emmeans`    | 1.8.3        | Performing multiple comparisons                              |
| `parallel`   | 4.2.2        | Parallel processing                                          |
| `snow`       | 0.4.4        | Parallel processing                                          |
| `stringr`    | 1.5.0        | Character string manipulations                               |
| `reshape2`   | 1.4.4        | Data reformatting, e.g., from long to wide                   |
| `abind`      | 1.4.5        | Creating higher-dimensional arrays from lower-dimensional arrays |
| `lubridate`  | 1.9.0        | Handling dates                                               |
| `dplyr`      | 1.0.10       | Summarizing large output data frames                         |
| `bookdown`   | 0.31         | R markdown output formats that include cross references      |
| `knitr`      | 1.41         | Rendering R markdown files                                   |
| `kableExtra` | 1.3.4        | Formatting of tables generated with `knitr::kable()`         |
| `details`    | 0.3          | Creating collapsible output in R markdown                    |
| `scales`     | 1.2.1        | Creating transparent colors                                  |

