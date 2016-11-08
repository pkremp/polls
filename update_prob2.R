# @knitr read_data_create_functions

rm(list = ls())
# setwd("~/GitHub/polls")

library(mvtnorm)
# library(knitr)

load("last_sim.RData")

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

Sigma <- cov(sim_forecast)
mu <- colMeans(sim_forecast)
names(mu) <- colnames(sim_forecast)

draw_samples <- function(clinton_states = NULL, trump_states = NULL, states = NULL, 
                         mean_clinton = NULL, sigma_clinton = NULL, print_acceptance = FALSE, target_nsim = 1000){
    sim <- matrix(NA, nr = 1, nc = length(mu))
    n <- 0
    if (!is.null(states)){
        Sigma2 <- diag(56)*10
        dimnames(Sigma2) <- dimnames(Sigma)
        mu2 <- mu
        for (s in states){
            mu2[s] <- mean_clinton[s]
            Sigma2[s, s] <- sigma_clinton[s]^2
        }
        inv_Sigma <- solve(Sigma)
        inv_Sigma2 <- solve(Sigma2)
        mu_conditional <- solve(inv_Sigma + inv_Sigma2) %*% (inv_Sigma %*% mu + inv_Sigma2 %*% mu2)
        Sigma_conditional <- solve(inv_Sigma + inv_Sigma2)
    }
    while(nrow(sim) < target_nsim){
        # randomly sample from the posterior distribution and reject when constraints are not met
        n <- n + 1
        if (!is.null(states)){
            proposals <- rmvnorm(1e5, mu_conditional, Sigma_conditional, method = "svd")
        }else{
            proposals <- rmvnorm(1e5, mu, Sigma, method = "svd")
        }
        colnames(proposals) <- names(mu)
        if (!is.null(clinton_states)) proposals[which(proposals[,clinton_states] < .5)] <- NA
        if (!is.null(  trump_states)) proposals[which(proposals[,  trump_states] > .5)] <- NA
        reject <- apply(proposals, 1, function(x) any(is.na(x)))
        sim <- rbind(sim, proposals[!reject,])
        if (nrow(sim) < target_nsim & nrow(sim)/(nrow(proposals)*n) < 1-99.99/100){
            print(paste("More than 99.99% of the samples are rejected; your scenario might be too unrealistic, or you may need relax some contraints.", sep = ""))
            break
            return(NULL)
        }
    }
    return(list("matrix" = sim[-1,], "acceptance_rate" = nrow(sim)/(nrow(proposals)*n)))
}

update_prob2 <- function(clinton_states = NULL, trump_states = NULL, clinton_normal = NULL, target_nsim = 1000, show_all_states = FALSE){
    states <- names(clinton_normal)
    mean_clinton <- sapply(clinton_normal, function(x) x[1]/100)
    sigma_clinton <- sapply(clinton_normal, function(x) x[2]/100)
    sim <- draw_samples(clinton_states = clinton_states, 
                        trump_states = trump_states, 
                        states = states, 
                        mean_clinton = mean_clinton, 
                        sigma_clinton = sigma_clinton, 
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
