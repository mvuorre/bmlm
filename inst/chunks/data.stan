    int<lower=1> N;             // Number of observations
    int<lower=1> J;             // Number of participants
    int<lower=1,upper=J> id[N]; // Participant IDs
    vector[N] X;                // Manipulated variable
    vector[N] M;                // Mediator
    real slope_scale;           // Prior scale for regression params
    real intercept_scale;       // Prior scale for intercepts
    real tau_scale;             // Prior scale for RE SDs
    real lkj_shape;             // Shape for LKJ prior
