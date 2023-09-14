// Stan code for multilevel mediation model

data {
#include chunks/data.stan
  array[N]  int<lower=0, upper=1> Y; // Dichotomous outcome
}
transformed data{
#include chunks/transformed_data.stan
}
parameters{
#include chunks/parameters.stan
}
transformed parameters {
#include chunks/transformed_parameters.stan
}
model {
#include chunks/model.stan
    // Data model
    Y ~ bernoulli_logit(mu_y);
    M ~ normal(mu_m, sigma_m);
}
generated quantities{
#include chunks/generated_quantities.stan
}
