    int<lower=1> N;             // Number of observations
    int<lower=1> J;             // Number of participants
    int<lower=1,upper=J> id[N]; // Participant IDs
    vector[N] X;                // Manipulated variable
    vector[N] M;                // Mediator
    // Priors
    real prior_dm;
    real prior_dy;
    real prior_a;
    real prior_b;
    real prior_cp;
    real prior_tau_dm;
    real prior_tau_dy;
    real prior_tau_a;
    real prior_tau_b;
    real prior_tau_cp;
    real prior_lkj_shape;
