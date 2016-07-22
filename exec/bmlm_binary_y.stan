// Stan code for multilevel mediation model

data {
    #include "data.stan"
    int<lower=0, upper=1> Y[N]; // Dichotomous outcome
}
transformed data{
    #include "transformed_data.stan"
}
parameters{
    #include "parameters.stan"
}
transformed parameters {
    #include "transformed_parameters.stan"
}
model {
    #include "model.stan"
    // Data model
    Y ~ bernoulli_logit(mu_y);
    M ~ normal(mu_m, sigma_m);
}
generated quantities{
    #include "generated_quantities.stan"
}
