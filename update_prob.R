# @knitr read_data_create_functions

rm(list = ls())
# setwd("~/GitHub/polls")

library(mvtnorm)
# library(knitr)

load("last_sim.RData")

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) 1/(1 + exp(-x))


# Adding ME1 and ME2, NE1 NE2 to sim_forecast matrix and ev vector

ev <- ev_state[colnames(sim_forecast)]
ev["ME"] <- 1
ev["NE"] <- 2
ev <- c(ev, "DC" = 3, "ME1" = 1, "ME2" = 1, "NE1" = 1, "NE2" = 1, "NE3" = 1)

sim_forecast <- cbind(sim_forecast, # Taking the 2012 delta with ME and NE state vote + noise
                      "ME1" = sim_forecast[,"ME"] + rnorm(nrow(sim_forecast),  .03,.0075),
                      "ME2" = sim_forecast[,"ME"] + rnorm(nrow(sim_forecast), -.03,.0075),
                      "DC" = 0.80 + rnorm(nrow(sim_forecast), 0,.015), 
                      "NE1" = sim_forecast[,"NE"] + rnorm(nrow(sim_forecast),  .03, .0075),
                      "NE2" = sim_forecast[,"NE"] + rnorm(nrow(sim_forecast),  .08, .0075),
                      "NE3" = sim_forecast[,"NE"] + rnorm(nrow(sim_forecast), -.10, .0075))

ev <- ev[colnames(sim_forecast)]

Sigma <- cov(logit(sim_forecast))
mu <- colMeans(logit(sim_forecast))
names(mu) <- colnames(sim_forecast)


draw_samples <- function(clinton_states = NULL, trump_states = NULL, states = NULL, 
                         upper_clinton = NULL, lower_clinton = NULL, print_acceptance = FALSE, target_nsim = 1000){
    sim <- matrix(NA, nr = 1, nc = length(mu))
    n <- 0
    while(nrow(sim) < target_nsim){
        # randomly sample from the posterior distribution and reject when constraints are not met
        n <- n + 1
        proposals <- inv_logit(rmvnorm(1e5, mu, Sigma, method = "svd")) # "DC" is pretty much uncorrelated
        colnames(proposals) <- names(mu)
        if (!is.null(clinton_states)) proposals[which(proposals[,clinton_states] < .5)] <- NA
        if (!is.null(  trump_states)) proposals[which(proposals[,  trump_states] > .5)] <- NA
        if (!is.null(        states)){
            for (s in states){
                proposals[which(proposals[, s] > upper_clinton[s] | 
                                proposals[, s] < lower_clinton[s])] <- NA
            }
        }
        reject <- apply(proposals, 1, function(x) any(is.na(x)))
        sim <- rbind(sim, proposals[!reject,])
        if (nrow(sim) < target_nsim & nrow(sim)/(nrow(proposals)*n) < 1-99.99/100){
            stop(paste("rmvnorm() is working hard... but more than 99.99% of the samples are rejected; you should relax some contraints.", sep = ""))
        }
    }
    return(list("matrix" = sim[-1,], "acceptance_rate" = nrow(sim)/(nrow(proposals)*n)))
}

update_prob <- function(clinton_states = NULL, trump_states = NULL, clinton_scores_list = NULL, target_nsim = 1000, show_all_states = FALSE){
    states <- names(clinton_scores_list)
    lower_clinton <- sapply(clinton_scores_list, function(x) x[1]/100)
    upper_clinton <- sapply(clinton_scores_list, function(x) x[2]/100)
    sim <- draw_samples(clinton_states = clinton_states, trump_states = trump_states, states = states, 
                        upper_clinton = upper_clinton, lower_clinton = lower_clinton, 
                        target_nsim = target_nsim)
    ev_dist <- (sim[["matrix"]] > .5) %*% ev
    state_win <- colMeans(sim[["matrix"]] > .5)
    p <- mean(ev_dist >= 270)
    sd <- sqrt(p*(1-p)/length(ev_dist))
    if (show_all_states){
        cat("Pr(Clinton wins) by state, in %:\n")
        print(t(round(100*state_win)))
        cat("--------\n")
    }
    cat(paste("Pr(Clinton wins the electoral college) = ", round(100*p), "%\n[nsim = ", length(ev_dist), "; se = ", round(sd*100,1), "%]", sep = ""))
    if (show_all_states) cat("\n--------\n")
}
