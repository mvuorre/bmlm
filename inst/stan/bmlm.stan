// Stan code for multilevel mediation model

data {
    int<lower=1> N;             // Number of observations
    int<lower=1> J;             // Number of participants
    int<lower=1,upper=J> id[N]; // Participant IDs
    vector[N] X;                // Manipulated variable
    vector[N] M;                // Mediator
    vector[N] Y;                // Outcome
    real prior_scale;           // Prior scale for regression params
    real intrcpt_scale;         // Prior scale for intercepts
    real y_mean;                // Mean of Y
}

parameters{
    // Regression Y on X and M
    real dy;                    // Intercept
    real cp;                    // X to Y effect
    real b;                     // M to Y effect
    real<lower=0> sigma_y;      // Residual
    // Regression M on X
    real dm;                    // Intercept
    real a;                     // X to M effect
    real<lower=0> sigma_m;      // Residual

    // Correlation matrix and SDs of random effects
    corr_matrix[5] Omega;
    vector<lower=0>[5] Tau;

    // Random effects
    matrix[J, 5] U;
}
transformed parameters {
    // RE covariance matrix
    matrix[5, 5] Sigma;
    Sigma = quad_form_diag(Omega, Tau);
}
model {
    // Means of likelihoods for regression models
    vector[N] mu_y;
    vector[N] mu_m;
    // Priors
    // Regression parameters
    dm ~ normal(y_mean, intrcpt_scale);
    dy ~ normal(y_mean, intrcpt_scale);
    a ~ normal(0, prior_scale);
    b ~ normal(0, prior_scale);
    cp ~ normal(0, prior_scale);
    // RE SDs and correlation matrix
    Tau ~ cauchy(0, 1);
    Omega ~ lkj_corr(2);

    // Sample random effects
    for (j in 1:J) { U[j] ~ multi_normal(rep_vector(0, 5), Sigma); }

    // Regressions (note order of REs for obtaining ab covariance below)
    for (n in 1:N){
        mu_y[n] = dy + U[id[n], 1] +
                   (cp + U[id[n], 2]) * X[n] +
                   (b + U[id[n], 3]) * M[n];
        mu_m[n] = dm + U[id[n], 4] + (a + U[id[n], 5]) * X[n];
    }

    // Data model
    Y ~ normal(mu_y, sigma_y);
    M ~ normal(mu_m, sigma_m);
}
generated quantities{
    // Average mediation parameters
    real covab;                 // a-b covariance
    real corrab;                // a-b correlation
    real ab;                    // Indirect effect
    real c;                     // Total effect
    real pme;                   // % mediated effect

    // Person-specific mediation parameters
    vector[J] u_ab;
    vector[J] u_cp;
    vector[J] u_c;
    vector[J] u_pme;

    covab = Sigma[5,3];
    corrab = Omega[5,3];
    ab = a*b + covab;
    c = cp + a*b + covab;
    pme = ab / c;

    for (j in 1:J) {
        u_ab[j] = (a + U[j, 5]) + (b + U[j, 3]);
        u_cp[j] = cp + U[j, 2];
        u_c[j] = u_cp[j] + u_ab[j];
        u_pme[j] = u_ab[j] / u_c[j];
    }
}
