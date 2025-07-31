
data {
    int<lower=0> N;
    int<lower=0> n_seeds;
    int<lower=0> n_players;
    array[N] int seed;
    array[N] int player;
    vector<lower=0>[N] t;
    vector<lower=0>[N] a;
}
parameters {
    vector<lower=0>[n_seeds] beta_seed;
    vector<lower=0>[n_players] beta_player;
    real<lower=0, upper=1> phi;
    real<lower=0> sigma;
}
transformed parameters {
    vector<lower=0>[N] beta;
    for (i in 1:N) {
        beta[i] = beta_player[player[i]] + beta_seed[seed[i]];
    }
}
model {
    for (i in 1:N) {
        beta[i] ~ exponential(1);
    }
    vector[N] y = -80 / (beta .* (t/1800)^phi + 1) + 80;
    a ~ normal(y, sigma) T[0, ];
}
