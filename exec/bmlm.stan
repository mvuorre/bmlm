// Stan code for multilevel mediation model

data {
    #include "data.stan"
    vector[N] Y;                // Continuous outcome
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
    Y ~ normal(mu_y, sigma_y);
    M ~ normal(mu_m, sigma_m);
}
generated quantities{
    #include "generated_quantities.stan"
}
