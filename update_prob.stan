data{
    matrix [56, 56] Sigma;
    vector [56] mu;
    vector [56] clinton_low;
    vector [56] clinton_high;
}

transformed data{
    matrix [56, 56] L;
    L = cholesky_decompose(Sigma);
}

parameters{
    vector<lower=0, upper=1> [56] y;
}

transformed parameters{
    vector<lower=0, upper=1> [56] y_constrained;
    for (s in 1:56)
        y_constrained[s] = clinton_low[s] + y[s]*(clinton_high[s] - clinton_low[s]);

}
model{
    target += multi_normal_cholesky_lpdf(y_constrained | mu, L);
}
