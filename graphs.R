# @knitr read_data_create_functions

library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(maps)
library(mapproj)
library(knitr)
library(mvtnorm)
library(DT)

rm(list = ls())
setwd("~/GitHub/polls")
load("out.RData")

logit <- function(p) log(p/(1-p))

inv_logit <- function(x) 1/(1+exp(-x))

interpolate_mu_b_sim <- function(mu_b_weekly_points){
    mu_b_daily <- approx(x = as.numeric(as.Date(all_weeks_until_election)), 
                         y = mu_b_weekly_points, 
                         xout = as.numeric(as.Date(all_t_until_election)), 
                         rule = c(2,2))$y # R's linear interpolation function
    return(mu_b_daily)
}

get_sim_series <- function(state_abbr_vec, sim){
    d <- NULL
    for (state in state_abbr_vec){
        mu_a_filled <- c(mu_a[sim,], 
                         rep(0, length(all_t_until_election) - length(all_t)))
        d <- rbind(d,
                   data.frame(t = all_t_until_election,
                              state = state,
                              p_sim = inv_logit(mu_a_filled + interpolate_mu_b_sim(mu_b[sim, , state]))))
    }
    return(d)
}

plot_score <- function(state_abbr_vec, show_nsim = NULL){
    ncolumns <- min(5, length(state_abbr_vec))
    ordered_states <- state_abbr_vec[order(state_abbr_vec)]
    state_labels <- paste(ifelse(ordered_states != "--", state_name[ordered_states], "National Vote"), 
                          "\nPr(Clinton wins) =", 
                          round(pred[pred$state %in% ordered_states & pred$t == election_day,]$clinton_win, 2))
    names(state_labels) <- ordered_states
    if (!is.null(show_nsim)){
        g <- ggplot(data = pred[pred$state %in% state_abbr_vec,])
        for (i in sample(1:nrow(mu_a), show_nsim, replace = FALSE)){
            g <- g + geom_line(data = get_sim_series(state_abbr_vec, i), 
                               aes(x = t, y = 100*p_sim), 
                               col = "darkgrey", alpha = .1)
        }
    }else{
        g <- ggplot(data = pred[pred$state %in% state_abbr_vec,])
    }
    g <- g +
        geom_line(aes(x = t, y = 100*p, linetype = ifelse(t < max(all_t), "dotted",  "solid")), color = "blue") +
        geom_ribbon(aes(x = t, ymin = 100*low, ymax = 100*high), 
                    alpha = 0.2, fill = "blue") + 
        geom_point(data = df[df$state %in% state_abbr_vec,],
                   aes(x = t, y = 100*p_clinton),
                   color = "blue", size = 1/min(2, length(state_abbr_vec))) +
        geom_vline(xintercept = as.numeric(election_day)) +
        geom_hline(yintercept = 50) +
        geom_hline(data = data.frame(state = state_abbr_vec, prior = 100*inv_logit(mu_b_prior[state_abbr_vec])), 
                   color = "black", linetype = "dotted",
                   aes(yintercept=prior)) +
        ylab("Clinton Vote in %") +
        xlab("") + scale_linetype_discrete(guide=FALSE) +
        facet_wrap(~ state, ncol = ncolumns, labeller = as_labeller(state_labels, multi_line = TRUE)) +
        theme(strip.text.y = element_text(angle = 0))
    return(g)
}


##################################
# Plot state and national trends #
##################################



# @knitr plot_national

plot_score("--")

# @knitr plot_states

plot_score(all_polled_states[2:11], show_nsim = 100)
plot_score(all_polled_states[12:21], show_nsim = 100)
plot_score(all_polled_states[22:31], show_nsim = 100)
plot_score(all_polled_states[32:41], show_nsim = 100)
plot_score(all_polled_states[42:51], show_nsim = 100)


###############################
# Shot pollster house effects #
###############################

# @knitr plot_house_effects

dp <- data.frame(polls = colnames(mu_c),
                 house_effect_P50 = 100*(inv_logit(as.vector(apply(mu_c, 2, median)))-.5),
                 house_effect_P05 = 100*(inv_logit(as.vector(apply(mu_c, 2, function(x) quantile(x, 0.05))))-.5),
                 house_effect_P95 = 100*(inv_logit(as.vector(apply(mu_c, 2, function(x) quantile(x, 0.95))))-.5))

ggplot(data = dp) + 
    geom_histogram(aes(x = house_effect_P50, 
                       fill = ifelse(house_effect_P50>=0, "Pro-Clinton Bias", "Pro-Trump Bias")), 
                   boundary = 0,
                   bins = 40) + 
    scale_fill_manual(values=c("#6E90F8", "#FF6666"), guide = guide_legend(title = "")) +
    xlab("Approximate Pollster House Effect in Percentage Points") + 
    ylab("Number of pollsters") + 
    scale_y_continuous(breaks = seq(0,100, 2))

dp[,2:4] <- round(dp[,2:4], 1)

# @knitr polls_most_pro_clinton

dp %>% arrange(-house_effect_P50) %>% head %>% kable(., col.names = c("Poll Origin", "Median", "P95", "P05"))

# @knitr polls_most_pro_trump

dp %>% arrange(house_effect_P50) %>% head %>% kable(., col.names = c("Poll Origin", "Median", "P95", "P05"))

#########
# alpha #
#########

# @knitr alpha

ggplot(data.frame(alpha = 100*(inv_logit(alpha + logit(pred$p[pred$state == "--" & pred$t == all_t[length(all_t)]]))-pred$p[pred$state == "--" & pred$t == all_t[length(all_t)]]) )) + 
    geom_histogram(aes(x = alpha), bins = 50) + 
    xlab("Estimated National/States Discrepancy of Clinton Scores (in % Points)") +
    ylab("Number of simulations")

################################
# State-by-state probabilities #
################################

# @knitr plot_state_probabilities

ggplot(data = pr_all_states) + 
    geom_point(aes(x = p, y = state_name, order = order_states, color = p, alpha = ifelse(polled, 1, 1/10))) +
    xlab("Pr(Clinton wins)") +
    ylab("") +
    ggtitle("State Probabilities") + 
    guides(color = FALSE, alpha = FALSE) + 
    scale_color_gradient2(low = "#FF6666", mid = "purple", high = "#6E90F8", midpoint = 50, na.value = "grey50")

# @knitr map

states_map <- map_data("state")
ggplot() + 
    geom_map(data = states_map, 
             map = states_map, aes(x=long, y=lat, map_id = region)) +
    geom_map(data = pr_all_states, 
             map = states_map, aes(fill= p, map_id = tolower(state_name))) +
    scale_fill_gradient2(low = "red3", mid = "white", high = "royalblue", midpoint = 50) + 
    theme(panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.title=element_blank()) + 
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(x = NULL, y = NULL) 

###############################
# Graph of Distribution of EV #
###############################

# Not accounting for the EV allocation rules of Nebraska and Maine

# @knitr plot_ev

ggplot() + 
    geom_histogram(data = data.frame(ev = result_ev_all_states), aes(ev, fill = ifelse(ev >= 270,"Clinton","Trump")), binwidth = 1) + 
    scale_fill_manual(values=c("#6E90F8", "#FF6666"), guide = guide_legend(title = "Winner")) +
    xlab("Electoral Votes for Clinton") + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.title.y = element_blank()) +
    ggtitle(paste("EV distribution - Pr(Clinton wins) = ", round(mean(result_ev_all_states >= 270)*100, 0), "%", sep = "")) 


## @knitr model_checks

print(stan_summary)

## @knitr plot_mu

median_mu_a <- apply(mu_a, c(2), median)
median_mu_b <- apply(mu_b, c(2,3), median)[,-1]

median_mu_a <- apply(mu_a, c(2), median)
median_mu_b <- apply(mu_b, c(2,3), median)[,-1]

plot(as.Date(names(median_mu_a)), median_mu_a, ylim = c(-.8, .8), 
     type = "l", col = "red", lwd = 2, 
     xlab = "Date", ylab = expression(paste("Median ",mu[a](t)," and ", mu[b](t,s), " (logit scale)")), 
     xlim = range(as.numeric(all_t_until_election)))
for(s in all_polled_states[-1]) points(as.Date(rownames(median_mu_b)), median_mu_b[,s], type = "l", col = "darkgrey")
abline(v = max(all_t))

## @knitr priors

m <- rmvnorm(1e5, mu_b_prior[-1], sigma_mu_b_end)
colnames(m) <- all_polled_states[-1]
m0 <- rmvnorm(1e5, mu_b_prior[-1], cov_matrix(length(mu_b_prior[-1]), sigma_mu_b_end[1,1], 0))
colnames(m0) <- all_polled_states[-1]

## @knitr table_predictions

table_pred <- data.frame(pred[pred$t == all_t[length(all_t)], c("state","p")], pred[pred$t == election_day, c("p", "clinton_win")])
table_pred$state_name <- state_name[as.character(table_pred$state)]
table_pred$state_name[is.na(table_pred$state_name)] <- "National Vote"
table_pred[,2:3] <- round(table_pred[,2:3], 3)
table_pred[,4] <- round(table_pred[,4],2)
rownames(table_pred) <- NULL
colors_red_to_blue <- colorRampPalette(c("#FF6666", "white","#6E90F8"))

# kable(table_pred[,c(5,2:4)], col.names = c("State", "Current Score", "Forecast Score", "Pr(Clinton Wins)"))
# Note: kable will not highlight rows/cells; using datatable (DT package) instead.

datatable(table_pred[,c(5,2:4)], 
          colnames = c("State", "Current Score", "Forecast Score", "Pr(Clinton Wins)"),
          rownames = FALSE,
          options = list(dom = c('f t'), pageLength = nrow(table_pred))) %>% 
    formatStyle('clinton_win', 
                backgroundColor=styleInterval(seq(.01,.99,.01), colors_red_to_blue(100))) %>%
    formatPercentage('clinton_win', 0) %>%
    formatPercentage(c('p', 'p.1'), 1)

# correlation_states
m <- rmvnorm(1e4, mu_b_prior[-1], sigma_mu_b_end)
rho <- apply(m, 1, function(x) cor(inv_logit(mu_b_prior[-1]), inv_logit(x)))
m0 <- rmvnorm(1e4, mu_b_prior[-1], cov_matrix(length(mu_b_prior[-1]),sigma_mu_b_end[1,1], 0))
rho0 <- apply(m0, 1, function(x) cor(inv_logit(mu_b_prior[-1]), inv_logit(x)))