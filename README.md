This is a [Stan](http://mc-stan.org) implementation of Drew Linzer's dynamic Bayesian election forecasting model, with some tweaks to incorporate national poll data, pollster house effects, correlated priors on state-by-state election results and comovement of public opinion across states. 

The model is presented briefly at the end of [`report.html`](http://pkremp.github.io/report.html).

`runmodel.R` downloads poll data from the HuffPost Pollster API, processes the data, and runs the Stan model in `state and national polls.stan`.

`report.Rmd` is a Rmarkdown document used to automatically generate the graphs/tables/maps in [`report.html`](http://pkremp.github.io/report.html) and relies on `graphs.R`.

# Reproducing the analysis

1. Clone this repository.
2. Install the required R packages (listed below)
3. Remove or comment out the lines in `graphs.R`, `runmodel.R` and `report.Rmd`
that read `setwd("~/GitHub/polls")`.
4. Optionally, modify line 2 of `runmodel.R` to use a number of cores that is
less than all available (e.g., `options(mc.cores = parallel::detectCores()
- 1)` to leave one core free for multitasking
5. In an R session, run `source("runmodel.R")`

## Required R packages

```r
curl
dplyr
DT
ggplot2
ggrepel
knitr
lubridate
mapproj
maps
mvtnorm
purrr
reshape2
rmarkdown
rstan
shinystan
stringr
tidyr
```
