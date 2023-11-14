data {
    int<lower=0> S; #sellers train 640
    int<lower=0> S_new; # sellers test 163
    int<lower=0> D; # features 8 -> LASSO     
    int<lower=0> W; # semana train 4
    matrix [S,W] X_prev; #vtas semanas 1,2,3,4 sellers train 640x4
    matrix[S_new,W] X_new; #vtas semanas 1,2,3,4 sellers test 163x4
    matrix [S,D] Z_prev; #caracteristicas sellers train 640x12
    matrix  [S_new,D] Z_new; #caracteristicas sellers test 640x12
    vector<lower=0> [S] y_train; #vtas semana 5 sellers train 640
}
parameters {
  //hyperparameters
  real  mu_a; 
  matrix <lower=0> [D,W] gamma_b;
  row_vector[W] mu_bb;
  real<lower=0> sigma;
}
transformed parameters{
  //beta sellers train
  matrix[S,W] beta;
  matrix[S,W]  mu_b;
  for (k in 1:S){
    mu_b[k,]=mu_bb ;
}
  beta= mu_b+Z_prev*gamma_b;
  // beta sellers test
  matrix[S_new,W]  mu_b_new;
  matrix[S_new,W] beta_new;
  for (k in 1:S_new){
    mu_b_new[k,]=mu_bb ;
}
  beta_new= mu_b_new+Z_new*gamma_b;
}
model {
  //hyperpriors
  //to_vector(mu_b) ~ normal(0, 50);
  mu_bb ~ normal(0, 50);
  //to_vector(mu_b_new) ~ normal(0,50);
  mu_a ~ normal(0, 20);
  to_vector(gamma_b) ~ normal(0,1000000);
  sigma ~ inv_gamma(0.001,0.001); #cambiar, probar
  for (i in 1:S)
   y_train[i] ~ lognormal(mu_a + beta[i,W]*X_prev[i,W], sigma);
}
generated quantities {
  vector [S_new] y_new;
  for (n in 1:S_new)
    y_new[n] = lognormal_rng(mu_a+beta_new[n,W]*X_new[n,W], sigma); 
} 

