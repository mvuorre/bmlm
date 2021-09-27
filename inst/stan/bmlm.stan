// Stan code for multilevel mediation model

data {
#include /chunks/data.stan
    vector[N] Y;                // Continuous outcome
}
transformed data{
#include /chunks/transformed_data.stan
}
parameters{
#include /chunks/parameters.stan
    real<lower=0> sigma_y;      // Residual
}
transformed parameters {
#include /chunks/transformed_parameters.stan
}
model {
#include /chunks/model.stan
    // Data model
    Y ~ normal(mu_y, sigma_y);
    M ~ normal(mu_m, sigma_m);
}
generated quantities{
#include /chunks/generated_quantities.stan
}
