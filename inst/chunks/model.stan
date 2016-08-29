    // Means of linear models
    vector[N] mu_y;
    vector[N] mu_m;
    // Cholesky factor of covariance matrix
    matrix[K, K] L_Sigma;
    // Regression parameter priors
    dy ~ normal(0, intercept_scale);
    dm ~ normal(0, intercept_scale);
    a ~ normal(0, slope_scale);
    b ~ normal(0, slope_scale);
    cp ~ normal(0, slope_scale);
    // RE SDs and correlation matrix
    tau[1] ~ cauchy(0, tau_scale);      // u_cp
    tau[2] ~ cauchy(0, tau_scale);      // u_b
    tau[3] ~ cauchy(0, tau_scale);      // u_a
    tau[4] ~ cauchy(0, tau_scale);      // u_intercept_y
    tau[5] ~ cauchy(0, tau_scale);      // u_intercept_m
    L_Omega ~ lkj_corr_cholesky(lkj_shape);
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
