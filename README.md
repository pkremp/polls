This is a [Stan](http://mc-stan.org) implementation of Drew Linzer's dynamic Bayesian election forecasting model, with some tweaks to incorporate national poll data, pollster house effects, correlated priors on state-by-state election results and comovement of public opinion across states. 

`runmodel.R` downloads poll data from the [Princeton Election Consortium website](http://election.princeton.edu)
(which are themselves compiled from the HuffPost Pollster API), processes the data, and runs the Stan model in `state and national polls.stan`.

`report.Rmd` is a Rmarkdown document used to automatically generate the graphs/tables/maps in `report.html` and relies on `graphs.R`.
