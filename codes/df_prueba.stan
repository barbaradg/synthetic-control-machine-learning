data {
  int<lower=0> J;      //numero de sellers (803)
  int<lower=0> K;      //numero de features (50)
  int<lower=0> S;      //numero de semanas de train (3)
  int<lower=0> S_new;  //numero de semanas de train (4)
  matrix[J,S] x_prev;  //vetas semanas 1,2,3 (803x3)
  real y[J];           //ventas semana 4 (observed)
  matrix [J,K] Z;      //covariables sellers (803x50)
  matrix[J, S] x_new;  //ventas semanas 1,2,3,4 (803x4)
}

parameters {
  //hyperparameters
  real mu_a;
  real sigma_a;
  real mu_b;
  real sigma_b;
  //intercept
  real a;
  //error
  real<lower=0> sigma;
  
}

transformed parameters{
  matrix[J,S] beta;
  beta= Z*mu_b+sigma_b;
}

model {
  //hyperpriors
  mu_b ~ normal(0, 50);
  mu_a ~ normal(0, 50);
  sigma_a ~ cauchy(0, 5);
  //priors
  a ~normal(mu_a, sigma_a);
  //likelihood
  
  y ~ normal(a+beta[J,S], sigma);
}

#predicciones?

generated quantities {
  vector[N_new] y_new;
  for (n in 1:J)
    y_new[n] = normal_rng(x_new[n] * beta[J,S_new], sigma);
}

