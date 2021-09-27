    // Means of linear models
    vector[N] mu_y;
    vector[N] mu_m;
    // Regression parameter priors
    dy ~ normal(0, prior_dy);
    dm ~ normal(0, prior_dm);
    a ~ normal(0, prior_a);
    b ~ normal(0, prior_b);
    cp ~ normal(0, prior_cp);
    // SDs and correlation matrix
    Tau[1] ~ cauchy(0, prior_tau_cp);   // u_cp
    Tau[2] ~ cauchy(0, prior_tau_b);    // u_b
    Tau[3] ~ cauchy(0, prior_tau_a);    // u_a
    Tau[4] ~ cauchy(0, prior_tau_dy);   // u_intercept_y
    Tau[5] ~ cauchy(0, prior_tau_dm);   // u_intercept_m
    L_Omega ~ lkj_corr_cholesky(prior_lkj_shape);
    // Allow vectorized sampling of varying effects via stdzd z_U
    to_vector(z_U) ~ normal(0, 1);

    // Regressions
    for (n in 1:N){
        mu_y[n] = (cp + U[id[n], 1]) * X[n] +
                  (b + U[id[n], 2]) * M[n] +
                  (dy + U[id[n], 4]);
        mu_m[n] = (a + U[id[n], 3]) * X[n] +
                  (dm + U[id[n], 5]);
    }
