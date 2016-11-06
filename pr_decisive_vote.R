# @knitr p_votecounts

library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(maps)
library(mapproj)
library(knitr)
library(mvtnorm)
library(DT)
library(reshape2)
library(ggrepel)


setwd("~/GitHub/polls")
load("out.RData")

# No polls for DC, but it does not matter: using the DC prior (about 90% Clinton)
sim_forecast <- cbind(sim_forecast, "DC" = .486 + states2012$diff_score[states2012$state == "DC"])

ev <- ev_state[colnames(sim_forecast)]
ev_matrix <- matrix(rep(ev, nrow(sim_forecast)), byrow = TRUE, nrow = nrow(sim_forecast))
colnames(ev_matrix) <- colnames(sim_forecast)
net_clinton_ev_matrix <- sign(sim_forecast - .5) * ev_matrix

turnout <- states2012$total_count*(1+states2012$adult_pop_growth_2011_15)
names(turnout) <- states2012$state
turnout <- turnout[colnames(sim_forecast)]

p_votecounts <- data.frame(state = colnames(net_clinton_ev_matrix),
                           p_state_decisive = NA,
                           p_state_tied_when_decisive = NA,
                           n = NA,
                           ev = ev,
                           turnout = turnout,
                           clinton_margin = 100*colMeans(sim_forecast)-50,
                           clinton_sd = 100*apply(sim_forecast, 2, sd))
p_votecounts$clinton_p <- 1-pnorm(0, p_votecounts$clinton_margin, p_votecounts$clinton_sd)

for(s in 1:51){
    sum_without_s  <- rowSums(net_clinton_ev_matrix[,-s]) # Vector of total net margin omitting state s
    logical_state_decisive <- abs(sum_without_s) <= ev[s] # Logical vector indicating if state puts the candidate on top (for each simulation)
    sims_state_decisive <- which(logical_state_decisive) # Indices corresponding to these simulations
    p_votecounts$p_state_decisive[s] <- mean(abs(sum_without_s) < ev[s]) + 
        .5*mean(abs(sum_without_s) == ev[s]) # Probability that the state is decisive
    state_margin_when_decisive <- turnout[s] * (sim_forecast[sims_state_decisive, s] - .5)
    p_votecounts$n[s] <- length(sims_state_decisive)
    mean <- mean(state_margin_when_decisive)
    sd <- sd(state_margin_when_decisive)
    p_votecounts$p_state_tied_when_decisive[s] <- 1/sd*dt(mean/sd, 4)
}


# Applying Bayes rule!
# p(individual vote is decisive) = p(state is tied AND state decides who wins)
#                                = p(state decides who wins)*p(state is tied | state decides who wins)

p_votecounts$p_vote_decisive <- p_votecounts$p_state_tied_when_decisive *
                                p_votecounts$p_state_decisive


p_votecounts$state_name <- state_name[as.character(p_votecounts$state)]

# states_map <- map_data("state")
# ggplot() + 
#     geom_map(data = states_map, 
#              map = states_map, aes(x=long, y=lat, map_id = region)) +
#     geom_map(data = p_votecounts , 
#              map = states_map, aes(fill= p_vote_decisive, map_id = tolower(state_name))) +
#     scale_fill_gradientn("",
#                          colors = c("yellow","yellow", "red", "red"),
#                          values = c(0,.2, .85, 1),
#                          limits = c(1e-12, max(p_votecounts$p_vote_decisive)),
#                          breaks = c(1e-11, 1e-9, 1e-7),
#                          labels = c("1 in\n100 billion", "1 in\n1 billion", "1 in\n10 million"),
#                          trans = "log10") +
#     theme(panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
#           axis.text = element_blank(), legend.position="bottom", plot.title=element_text(vjust=-50)) + 
#     ggtitle("Chance that your vote will decide the election") +
#     guides(fill = guide_colorbar(barwidth = unit(4, "in"), title.position = "top")) +
#     coord_map("albers", lat0 = 39, lat1 = 45) +
#     labs(x = NULL, y = NULL)

states_map <- map_data("state")
ggplot() + 
    geom_map(data = states_map, 
             map = states_map, aes(x=long, y=lat, map_id = region)) +
    geom_map(data = p_votecounts , 
             map = states_map, aes(fill= p_vote_decisive, map_id = tolower(state_name))) +
    scale_fill_gradientn("",
                         colors = c("yellow","yellow", "red", "red"),
                         values = c(0,.2, .92, 1),
                         limits = c(1e-12, max(p_votecounts$p_vote_decisive)),
                         breaks = c(1e-12, 1e-10, 1e-8, 1e-6),
                         labels = c("1 in\n1 trillion", "1 in\n10 billion", "1 in\n100 million", "1 in\n1 million"),
                         trans = "log10") +
    theme(panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.position="bottom", plot.title=element_text(vjust=-50)) + 
    ggtitle("Chance that your vote will decide the election") +
    guides(fill = guide_colorbar(barwidth = unit(4, "in"), title.position = "top")) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(x = NULL, y = NULL)

# @knitr p_votecounts_figure

# Try with fig.width = 8, fig.height = 5.3

#quartz(height = 4, width = 7.2)
ggplot(data = p_votecounts %>% filter(state != "DC")) +
    geom_text(aes(y = p_state_decisive, 
                        x = p_state_tied_when_decisive,
                        label = state), size = 3) +
    scale_y_log10(name = "Chance that your state is needed\nfor an electoral college win", 
                  limits = c(1e-3, 1), 
                  minor_breaks = NULL,
                  breaks = c(.001, .01, .1, 1),
                  labels = c(expression(10^-3), expression(10^-2), expression(10^-1), 1)) +
    scale_x_log10(name = "Chance that your state is tied, given\nthat its electoral votes are needed",
                  limits = c(1e-10, 1e-4),
                  minor_breaks = NULL,
                  breaks = c(1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4),
                  labels = c(expression(10^-10), expression(10^-9), expression(10^-8), expression(10^-7), 
                             expression(10^-6), expression(10^-5), expression(10^-4))) +
    stat_function(fun = function(x) 1e-7/x, size = .5, linetype = "dotted") +
    stat_function(fun = function(x) 1e-9/x, size = .5, linetype = "dotted") +
    stat_function(fun = function(x) 1e-11/x, size = .5, linetype = "dotted") +
    geom_text(data = data.frame(x = c(1e-7, 1e-9, 1e-10),
                                y = c(1,    1,    1e-1),
                                text = c("Pr(your vote is decisive):    \n1 in 10 million     ", 
                                         "1 in 1 billion", 
                                         "1 in 100 billion")), 
              aes(x = x, y = y, label = text, angle = -45), vjust = "top", hjust = "left", nudge_y = -.1, size = 3, color = "black")


# @knitr p_votecounts_table

translate_small_prob <- function(p_vector){
    # Translate a vector of tiny probabilities into something more meaningful,
    # like "1 in 20 million" instead of 4.283572e-08
    n <- 1/p_vector
    rounds <- data.frame(thousand = round(n*1e-3), 
                         million = round(n*1e-6), 
                         billion = round(n*1e-9))
    rounds[rounds == 0] <- NA
    number <- pmin(rounds$thousand, rounds$million, rounds$billion, na.rm = TRUE)
    unit   <- sapply(seq_along(number), function(i) 
        colnames(rounds)[(rounds == number & !is.na(rounds))[i,]])
    number <- ifelse(number > 10 & number < 100, round(number/10)*10, 
                     ifelse(number > 100, round(number/100)*100, number))
    paste("1 in", number, unit)
}

p_votecounts %>% 
    arrange(-p_vote_decisive) %>% 
    filter(state != "DC") %>%
    mutate(p_state_decisive_translated = paste(ifelse(floor(p_state_decisive*100) > 1, 
                                                      round(p_state_decisive*100), "<1"),
                                               "%", sep = ""),
           # Translating small probabilities:
           p_state_tied_when_decisive_translated = translate_small_prob(p_state_tied_when_decisive),
           p_vote_decisive_translated = translate_small_prob(p_vote_decisive)) %>%
    select(state_name, ends_with("_translated")) %>%
    kable(., col.names = c("State", "Pr(State EV are critical)", "Pr(State vote is tied | State EV are critical)", "P(your vote is decisive)"), align = "r")


# @knitr misc

ggplot(data = p_votecounts %>% filter(state != "DC")) + 
    geom_text_repel(aes(x = ev, y = p_state_decisive, label = state), size = 2, segment.size = .2) + 
    geom_point(aes(x = ev, y = p_state_decisive, color = clinton_p), size = .5) +
    scale_x_log10("State electoral votes", breaks = c(3, 9, 27), minor_breaks = NULL) +
    scale_y_log10("Chance that your state is needed\nfor an electoral college win", breaks = .01*2^seq(0, 6), minor_breaks = NULL) +
    scale_color_gradientn("Pr(Clinton\nwins)",colors= c("red", "purple", "blue"), values = c(0, .5, 1), 
                          rescaler = function(x, ...) x, oob = identity)
