rm(list = ls())
options(mc.cores = parallel::detectCores())

library(rstan)
library(dplyr)
library(stringr)
library(lubridate)
library(curl)
library(shinystan)
library(rmarkdown)

setwd("~/GitHub/polls")

logit <- function(p) log(p/(1-p))

inv_logit <- function(x) 1/(1+exp(-x))

corr_matrix <- function(m){
	(diag(m)^-.5 * diag(nrow = nrow(m))) %*% m %*% (diag(m)^-.5 * diag(nrow = nrow(m))) 
}

cov_matrix <- function(n, sigma2, rho){
    m <- matrix(nrow = n, ncol = n)
    m[upper.tri(m)] <- rho
    m[lower.tri(m)] <- rho
    diag(m) <- 1
    (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

get_polls <- function(national = FALSE, file, start_date){
    url <- ifelse(national == TRUE, 
                  "http://elections.huffingtonpost.com/pollster/2016-general-election-trump-vs-clinton.csv",
                  "http://election.princeton.edu/code/data/2016_StatePolls.csv")
    curl_download(url, file)
    print(paste("Done downloading", url))
    polls_df <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
    colnames(polls_df) <- colnames(polls_df) %>% tolower
    if (national == TRUE){
        # Transform the national polls data frame (from HuffPost) so that
        # its variables correspond to the state polls data frame (from 
        # Sam Wang's website).
        polls_df <- polls_df %>% 
            rename(pop = number.of.observations,
                   vtype = population,
                   method = mode) %>%
            mutate(endyy = as.integer(substring(end.date, 1, 4)),
                   endmm = as.integer(substring(end.date, 6, 7)),
                   enddd = as.integer(substring(end.date, 9, 10)),
                   state = "--") %>%
            filter(vtype == "Likely Voters" | vtype == "Registered Voters" | vtype == "Adults")
    }
    polls_df <- polls_df %>% 
        mutate(pollster = str_extract(pollster, pattern = "[A-z ]+") %>% sub("\\s+$", "", .),
               t = as.Date(paste(endmm, enddd, endyy, sep = "/"), format="%m/%d/%Y"),
               undecided = ifelse(is.na(undecided), 0, undecided),
               other = ifelse(is.na(other), 0, other),
               sum = clinton + trump
        ) %>%
        select(state, t, pollster, vtype, method, pop, trump, clinton, other, undecided, sum) %>%
        arrange(t, state, pollster, vtype, sum)
    polls_df$pollster <- ifelse(polls_df$pollster == "Fox News", "FOX", polls_df$pollster) # Cleaning up: noticed that Fox News polls were sometimes called "FOX", sometimes "Fox News"...
    print("Last 10 rows:")
    tail(polls_df, n = 10) %>% print
    polls_df <- polls_df[polls_df$t >= start_date & !is.na(polls_df$t),]
    cat(nrow(polls_df), "rows in data frame\n")
    return(polls_df)
}

system("cp state_polls.csv state_polls-old.csv")
system("cp national_polls.csv national_polls-old.csv")

start_date <- as.Date("2016-04-01") # Keeping all polls after April 1, 2016.

state_polls <- get_polls(national = FALSE,
                         file = "state_polls.csv",
                         start_date = start_date)

national_polls <- get_polls(national = TRUE,
                            file = "national_polls.csv",
                            start_date = start_date)

# state_polls <- get_polls(file = "state_polls.csv",
#                          start_date = start_date)
# 
# national_polls <- get_polls(file = "national_polls.csv",
#                             start_date = start_date)


all_polled_states <- state_polls$state %>% unique %>% sort
ndays <- max(state_polls$t, national_polls$t) - min(state_polls$t, national_polls$t)
all_t <- min(state_polls$t, national_polls$t) + days(0:(ndays))
all_weeks <- floor_date(all_t, unit = "week") %>% unique

all_polled_states <- c("--", all_polled_states)

# Reading 2012 election data to (1) set priors on mu_b and alpha,
#                               (2) get state_weights, 
#                               (3) get state_names and EV

states2012 <- read.csv("2012.csv", 
                       header = TRUE, stringsAsFactors = FALSE) %>% 
                mutate(score = obama_count / (obama_count + romney_count),
                       national_score = sum(obama_count)/sum(obama_count + romney_count),
                       diff_score = score - national_score,
                       share_national_vote = (total_count*(1+adult_pop_growth_2011_15))
                                            /sum(total_count*(1+adult_pop_growth_2011_15))) %>%
                arrange(state)
rownames(states2012) <- states2012$state

prior_diff_score <- states2012[all_polled_states[-1],]$diff_score
names(prior_diff_score) <- all_polled_states[-1]

state_weights <- c(0, states2012[all_polled_states[-1],]$share_national_vote / sum(states2012[all_polled_states[-1],]$share_national_vote))
names(state_weights) <- c("--", states2012[all_polled_states[-1],]$state)

state_name <- states2012$state_name
names(state_name) <- states2012$state

# Electoral votes, by state:

ev_state <- states2012$ev
names(ev_state) <- states2012$state

# Putting together rows of national_polls and state_polls in a single date frame

df <- rbind(national_polls, state_polls) %>%
       mutate(week = floor_date(t, unit = "week"),
           day_of_week = as.integer(format(t, format = "%w")),
           # Trick to keep "likely voter" polls when multiple results are reported.
           polltype = as.integer(as.character(recode(vtype, "Likely Voters" = "0", 
                                                            "Registered Voters" = "1",
                                                            "Adults" = "2"))), 
           n_clinton = round(pop * clinton/100),
           # Only looking at trump or clinton voters, leaving 3rd party candidates and undecided voters out for now.
           n_respondents = round(pop*(clinton+trump)/100),
           p_clinton = clinton/(clinton+trump))  %>%
       arrange(state, t, polltype, sum) %>% 
       distinct(state, t, pollster, .keep_all = TRUE) %>% # Only keeping 1 poll result per state, date, and pollster.
       select(n_clinton, n_respondents, p_clinton, state, pollster, t, week, day_of_week) %>%
       mutate(index_s = as.numeric(as.factor(as.character(state))), 
              # Factors are alphabetically sorted: 1 = --, 2 = AL, 3 = AK, 4 = AZ...
           index_t = 1 + as.numeric(t) - min(as.numeric(all_t)),
           index_w = as.numeric(as.factor(week)),
           index_p = as.numeric(as.factor(as.character(pollster))))

unique_ts <- unique(df$index_t[df$state == "--"])

df$index_t_unique <- sapply(1:nrow(df), 
                              function(i) ifelse(df$state[i] == "--", 
                                                 which(unique_ts == df$index_t[i]), 
                                                 0))

unique_ws <- sapply(unique_ts, function(i) unique(df$index_w[df$index_t == i]))

election_day <- as.Date("2016-11-08")
all_pollsters <- levels(as.factor(df$pollster))
all_weeks_until_election <- min(all_weeks) + weeks(0:((election_day - min(all_weeks))/7 %>% floor))
all_t_until_election <- min(all_t) + days(0:(election_day - min(all_t)))
all_states <- states2012$state

# Mean of the mu_b_prior
# 0.486 is the predicted Clinton share of the national vote according to the Time for Change model.
mu_b_prior <- logit(0.486 + c("--" = 0, prior_diff_score))

# The model uses national polls to complement state polls when estimating the national term mu_a.
# One problem until early September, was that voters in polled states were different from average voters :
# Several solid red states still hadn't been polled, the weighted average of state polls was slightly more pro-Clinton than national polls.

score_among_polled <- sum(states2012[all_polled_states[-1],]$obama_count)/
    sum(states2012[all_polled_states[-1],]$obama_count + 
        states2012[all_polled_states[-1],]$romney_count)
alpha_prior <- log(states2012$national_score[1]/score_among_polled)


sigma_mu_b_end <-cov_matrix(n = length(mu_b_prior) - 1, sigma2 = 1/20, rho = 0.5)
sigma_walk_b_forecast <- cov_matrix(length(mu_b_prior) - 1, 7*(0.015)^2, 0.75)

out <- stan("state and national polls.stan", 
     data = list(N = nrow(df),                  # Number of polls
                 S = max(df$index_s),           # Number of states
                 T = length(all_t_until_election),           # Number of days
                 W = length(all_weeks_until_election),           # Number of weeks
                 P = max(df$index_p),           # Number of pollsters
                 last_poll_T = length(all_t),
                 last_poll_W = length(all_weeks),
                 T_unique = max(df$index_t_unique), # Number of unique t indices for which there is at least one national poll: this is used to avoid redundant computations of the national vote in the likelihood of the model.
                 t_unique = df$index_t_unique,
                 unique_ts = unique_ts,
                 unique_ws = unique_ws,
                 s = df$index_s,
                 t = df$index_t,
                 w = df$index_w,
                 p = df$index_p,
                 n_clinton = df$n_clinton,
                 n_respondents = df$n_respondents,
                 state_weights = state_weights,
                 alpha_prior = alpha_prior,
                 mu_b_prior =  mu_b_prior,
                 sigma_mu_b_end = sigma_mu_b_end,
                 sigma_walk_b_forecast = sigma_walk_b_forecast,
                 week = as.integer(as.factor(floor_date(all_t, unit="week"))),
                 day_of_week = as.numeric(format(all_t, format = "%w"))),
     chains = 4, iter = 2000)

time_lastrun <- Sys.time()

last_polls <- head(df %>% arrange(-as.numeric(t)), n = 10) %>% 
    mutate(end_date = t,
           p_clinton = round(p_clinton*100, 1), 
           p_trump = 100-p_clinton, 
           N=n_respondents) %>% 
    select(end_date, pollster, state, p_clinton, p_trump, N)

stan_summary <- capture.output(print(out, pars = c("alpha", "sigma_c", 
                                                   "sigma_u_state", "sigma_u_national",
                                                   "sigma_walk_a_past", "sigma_walk_b_past",
                                                   paste("mu_b[31,", as.character(2:max(df$index_s)),"]", 
                                                         sep =""))))
stan_summary

p <- extract(out, pars = "predicted_score")[[1]]
alpha <- extract(out, pars = "alpha")[[1]]
mu_a <- extract(out, pars = "mu_a")[[1]]
mu_b <- extract(out, pars = "mu_b")[[1]]
mu_c_standardized <- extract(out, pars = "mu_c")[[1]]
sigma_c <- extract(out, pars = "sigma_c")[[1]]
mu_c <- as.vector(sigma_c)*mu_c_standardized
sigma_walk_b_past <- extract(out, pars = "sigma_walk_b_past")[[1]]
sigma_walk_a_past <- extract(out, pars = "sigma_walk_a_past")[[1]]
sigma_u_state <- extract(out, pars = "sigma_u_state")[[1]]
sigma_u_national <- extract(out, pars = "sigma_u_national")[[1]]


dates <- sort(c(all_t, unique(setdiff(all_weeks_until_election, all_weeks))))
dates <- c(dates[-length(dates)], election_day) # For convenience: the last date will be named Nov 8 (not Nov 6, when the week begins).
dimnames(p) <- list(1:nrow(p), as.character(dates), all_polled_states)
dimnames(mu_a) <- list(1:nrow(mu_a), as.character(all_t))
dimnames(mu_b) <- list(1:dim(mu_b)[1],
                       as.character(all_weeks_until_election),
                       all_polled_states)
dimnames(mu_c) <- list(1:nrow(mu_c), all_pollsters)

hist(apply(p[,as.character(election_day),-1], 1, function(vec) cor(vec, inv_logit(mu_b_prior[-1]))))

pred <- data.frame(t = rep(dates, length(all_polled_states)),
                   state = as.character(rep(all_polled_states, each = length(dates))),
                   p =    apply(p, c(2,3), median) %>% as.vector,
                   high = apply(p, c(2,3), function(x) quantile(x, .95)) %>% as.vector,
                   low =  apply(p, c(2,3), function(x) quantile(x, .05))  %>% as.vector,
                   clinton_win = apply(p, c(2,3), function(x) mean(x > .5))  %>% as.vector)

# Predicted electoral votes for each simulation

sim_win <- p[, as.character(election_day),-1] > 0.5

all_non_polled_states <- setdiff(all_states, all_polled_states[-1])
non_polled_win        <- states2012[all_non_polled_states,]$score > .5
names(non_polled_win) <- all_non_polled_states

non_polled_win_matrix <- rep(non_polled_win, nrow(sim_win)) %>%
    matrix(nr = nrow(sim_win), byrow = TRUE,
           dimnames = list(1:nrow(sim_win), all_non_polled_states))

sim_win_all_states <- cbind(sim_win, non_polled_win_matrix)

result_ev_all_states <- sim_win_all_states %*% ev_state[colnames(sim_win_all_states)]

# P(win electoral college)
mean(result_ev_all_states >= 270)

# P(win national vote)
mean(p[, as.character(election_day), "--"] > .5)

# Sorted predictions for every state, ready to be passed to ggplot

pr_polled_states <- pred %>% filter(t == election_day & state != "--") %>%
    arrange(-clinton_win) %>%
    transmute(state, p=100*clinton_win, polled = TRUE)
all_non_polled_states <- setdiff(all_states, all_polled_states[-1])
non_polled_win <- states2012[all_non_polled_states,]$score > .5
names(non_polled_win) <- all_non_polled_states
pr_other_states <- data.frame(state = names(non_polled_win), p = 100*non_polled_win, polled = FALSE)

pr_all_states <- rbind(pr_other_states, pr_polled_states)

pr_all_states$position <- ifelse(pr_all_states$p == 100 & !pr_all_states$polled,
                                 0,
                                 ifelse(pr_all_states$p == 0 & !pr_all_states$polled,
                                        2,
                                        1))
pr_all_states <- pr_all_states %>% arrange(-p, position)

pr_all_states$state_name <- state_name[as.character(pr_all_states$state)]
pr_all_states$order_states <- nrow(pr_all_states):1
pr_all_states$state_name <- factor(pr_all_states$state_name, levels = pr_all_states$state_name[pr_all_states$order_states])

# Take a subset of 100 simulations of predicted scores to display in ggplot
p_subset <- p[sample(1:nrow(p), 100, replace = FALSE),,]

rm(out)
rm(p)
save.image("out.RData")
render("report.Rmd")