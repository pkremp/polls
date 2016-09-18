data{
    int S;    // Number of states (for which at least 1 poll is available) + 1
    int T;    // Number of days
    int N;    // Number of polls
    int W;    // Number of weeks
    int P;    // Number of pollsters
    int T_unique;
    int last_poll_W;
    int last_poll_T;
    int s[N]; // State index
    int t[N]; // Day index
    int w[N]; // Week index (weeks start on Sundays)
    int p[N]; // Pollster index
    int t_unique[N]; // = Which unique national poll date index does this poll correspond to?
    int unique_ts[T_unique];
    int unique_ws[T_unique];
    vector[S] state_weights;
    real alpha_prior;
    int n_clinton[N];
    int <lower = 0> n_respondents[N];
    vector [S] mu_b_prior;
    real sigma_walk_a_past;
    real sigma_walk_b_past;
    matrix [S-1, S-1] sigma_mu_b_end;
    matrix [S-1, S-1] sigma_walk_b_forecast;
}

transformed data{
    matrix [S-1, S-1] chol_sigma_walk_b_forecast;
    matrix [S-1, S-1] chol_sigma_mu_b_end;
    row_vector [S-1] zero_vec;
    chol_sigma_walk_b_forecast = cholesky_decompose(sigma_walk_b_forecast);
    chol_sigma_mu_b_end = cholesky_decompose(sigma_mu_b_end);
    for (state in 1:(S-1)) zero_vec[state] = 0;
}

parameters{
    vector[last_poll_T-1] delta_a;
    matrix[W-1,S] delta_b;
    vector[S] mu_b_end;
    vector[P] mu_c;
    real alpha;
    real<lower = 0, upper = 0.50> sigma_c;
    real u[N];
    real<lower = 0, upper = 1> sigma_u_national;
    real<lower = 0, upper = 1> sigma_u_state;

}

transformed parameters{
    real<lower = 0, upper = 1> p_clinton_hat[N];
    vector[last_poll_T] mu_a;
    matrix[W,S] mu_b;
    vector[T_unique] average_states;
    matrix[T_unique , S-1] matrix_inv_logit_mu_ab;
    real mu_a_t;
    # Calculating mu_a
    mu_a[last_poll_T] = 0;
    for (i in 1:(last_poll_T-1)){
        mu_a[last_poll_T-i] = mu_a[last_poll_T-i+1] + sigma_walk_a_past * delta_a[last_poll_T-i];
    }
    # Calculating mu_b (using the cholesky decompositions of covariance matrices)
    mu_b[W, 2:S] = to_row_vector(mu_b_prior[2:S] + chol_sigma_mu_b_end * to_vector(mu_b_end[2:S]));
    for (wk in 1:(W - last_poll_W)){
        mu_b[W - wk, 2:S] = mu_b[W - wk + 1, 2:S] + 
                            to_row_vector(chol_sigma_walk_b_forecast * to_vector(delta_b[W - wk, 2:S]));
    }
    for (wk in (W - last_poll_W + 1):(W-1)){
        mu_b[W - wk, 2:S] = mu_b[W - wk + 1, 2:S] + sigma_walk_b_past * sqrt(7) * delta_b[W - wk, 2:S];
    }
    for (wk in 1:W) mu_b[wk, 1] = 0;
    # Calculating weighted state averages, only for days in which there are national polls.
    for(i in 1:T_unique){
        mu_a_t = mu_a[unique_ts[i]];
        for (state in 1:(S-1)){
            matrix_inv_logit_mu_ab[i, state] = inv_logit(mu_a_t + mu_b[unique_ws[i], state+1]);
        } 
    }
    average_states =  matrix_inv_logit_mu_ab * state_weights[2:S];
    # Calculating p_clinton_hat parameter for each national/state poll
    for(i in 1:N){
        if (s[i] == 1){
            # For national polls: 
            # p_clinton_hat is a function of: a national parameter **mu_a**,
            # the weighted average of the (interpolated) state parameters **mu_b**
            # an adjustment parameter **alpha** reflecting the fact that the average polled
            # state voter is not representative of the average US voter
            # and pollster house effects **mu_c**.
            p_clinton_hat[i] = inv_logit(logit(average_states[t_unique[i]]) + alpha + sigma_c*mu_c[p[i]] + sigma_u_national*u[i]);
        }
        else{
            # For state polls:
            # p_clinton_hat is a function of national and state parameters **mu_a**, **mu_b**
            # and pollster house effects **mu_c**
            p_clinton_hat[i] = inv_logit(mu_a[t[i]] + mu_b[w[i], s[i]] + sigma_c*mu_c[p[i]] + sigma_u_state*u[i]);
        }
    }
}

model{
    # To simplify sampling:
    # mu_b_end, delta_a, & delta_b are drawn from Normal(0,1), and values for mu_a and mu_b
    # are calculated in the transformed parameters block; this speeds up convergence
    # dramatically because mu_a and mu_b params can be highly correlated, 
    # Prior of state parameters on election day mu_b
    mu_b_end[2:S] ~ normal(0, 1);
    # Steps delta_a and delta_b of the reverse random walks.
    delta_a ~ normal(0, 1); 
    for (wk in 1:(W-1)){
        delta_b[W - wk, 2:S] ~ normal(0, 1);
    }
    # Prior for the difference between national and weighted average of tate parameters:
    alpha ~ normal(alpha_prior, .2);
    u ~ normal(0, 1);
    # Pollster house effects
    mu_c ~ normal(0, 1);
    n_clinton ~ binomial(n_respondents, p_clinton_hat);
}
