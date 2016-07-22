    // Regression Y on X and M
    real dy;                    // Intercept
    real cp;                    // X to Y effect
    real b;                     // M to Y effect
    real<lower=0> sigma_y;      // Residual
    // Regression M on X
    real dm;                    // Intercept
    real a;                     // X to M effect
    real<lower=0> sigma_m;      // Residual

    // Correlation matrix and SDs of participant-level varying effects
    cholesky_factor_corr[K] L_Omega;
    vector<lower=0>[K] tau;

    // Standardized varying effects
    matrix[K, J] z_U;
