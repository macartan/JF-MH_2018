// Program for Liberia Study Model
// 2017/01/14
// version with no village level effects on non-negative parameters


functions {

  // function makes weights for categorical distribution given parameters
  // key assumption is alpha_i is distributed normal 
  // Then different values of alpha_i determine different choices.
  // requires gamma positive. Choices in 0:3
  vector make_weights(real E, real q, real r,
                      real alpha, real gamma, real rho, real phi, real c) {

    vector[4] w_U;

   // max category: probabilities calculated assuming choice on  0-3 scale for utilty
   // even though options are labelled 1-4

    w_U[1] =
        normal_cdf(  ( gamma * 1 - 2 * gamma * rho * E - r - phi * q ),
                      alpha, c);
    w_U[2] =
        normal_cdf(  ( gamma * 3 - 2 * gamma * rho * E - r - phi * q ),
                      alpha, c ) -
        normal_cdf(  ( gamma * 1 - 2 * gamma * rho * E - r - phi * q ),
                      alpha, c );
	 w_U[3] =
        normal_cdf(  ( gamma * 5 - 2 * gamma * rho * E - r - phi * q ),
                      alpha, c ) -
        normal_cdf(  ( gamma * 3 - 2 * gamma * rho * E - r - phi * q ),
                      alpha, c );
	w_U[4] =
        1 -
        normal_cdf(  ( gamma * 5 - 2 * gamma * rho * E - r - phi * q ),
                      alpha, c );

    return w_U;
  }
}

data {
  // counters
  int<lower=0> N;             // number of subjects
  int<lower=0> K;             // number of villages

  // main data: Note some redundancy in this data; but makes the transformations more transparent
  
  int<lower=1,upper=4>              X[N];                  // contributions made on 1-4 scale
  real<lower=0,upper=3>             E[N];                  // expectations  on 0-3 scale, continuous
  int<lower=2,upper=5>              r[N];                  // multipliers
  real<lower=0,upper=1>             q[N];                  // probability of punishment from survey
  int<lower=1,upper=K>              village_id[N];         // village identifier
  int<lower=0,upper=1>              homog[K];              // homog (1) / mixed (0) treatment village identifier
  int<lower=0,upper=1>              mixed[K];              // homog (0) / mixed (1) treatment village identifier
  int<lower=0,upper=1>              male[N];               // male (1) / female (0) individual
  int<lower=0,upper=1>              fhomog[N];             // female in homog (1) other (0) 
  int<lower=0,upper=1>              fmixed[N];             // female in mixed (1) other (0) 
  real<lower=0>                     sigma[7];              // variance for priors

  }
parameters {
 
 // village random intercepts for village varying parameters: alpha,  phi set to 0 mean
 // positive lower bound on sigma
  vector[K] alpha_re;
  real <lower=.1>  sigma_alpha_re;
  vector[K] phi_re;
  real <lower=.1>  sigma_phi_re;

 // treatment condition  parameters  
  real alpha_F;     // condition effect for alpha MALE
  real alpha_M;     // condition effect for alpha MALE
  real alpha_H;     // condition effect for alpha HOMOG
  real phi_F;       // condition effect for phi
  real phi_M;       // condition effect for phi
  real phi_H;       // condition effect for phi
  real<lower=0.01> gamma_F;     // condition effect for gamma
  real<lower=0.01> gamma_M;     // condition effect for gamma
  real<lower=0.01> gamma_H;     // condition effect for gamma
  real<lower=0.01> rho_F;       // condition effect for rho
  real<lower=0.01> rho_M;       // condition effect for rho
  real<lower=0.01> rho_H;       // condition effect for rho
  real<lower=0.01> c_MX;        // condition effect for c (assumed common across genders in mixed communities)
  real<lower=0.01> c_H;         // condition effect for c (assumed common across genders)
 }

transformed parameters {

  // primitives setup on individual level
  real                     alpha[N];         // intrinsic value of public good
  real                     phi[N];           // expected punishment
  real<lower=0>            gamma[N];         // conformity concerns
  real<lower=0>            rho[N];           // conformity reference point
  real<lower=0>            c[N];             // range of intrinsic value of public good
  

  // transform to individual level 
  for (n in 1:N) {
    alpha[n] = alpha_re[village_id[n]]  + alpha_M*male[n]   + alpha_H*fhomog[n] + alpha_F*fmixed[n];
    phi[n]   = phi_re[village_id[n]]    + phi_M*male[n]     + phi_H*fhomog[n]   + phi_F*fmixed[n];
    gamma[n] =                            gamma_M*male[n]   + gamma_H*fhomog[n] + gamma_F*fmixed[n];
    rho[n]   =                            rho_M*male[n]     + rho_H*fhomog[n]   + rho_F*fmixed[n];	
    c[n]     = c_H*(homog[village_id[n]]) + c_MX*(mixed[village_id[n]]);                                                         
  	}
}

	
model {
  // weights for categorical distribution
  vector[4] w_U[N];
 
  // generate weights for each individual choice using custom function
  for (n in 1:N) {
      w_U[n] = make_weights( E[n], q[n], r[n],
                             alpha[n], gamma[n], rho[n], phi[n], c[n] );
	}

 
  
  // Likelihood
  for (n in 1:N) {
    X[n] ~ categorical( w_U[n] );
 	}

  // Priors
  alpha_re   ~ normal(0, sigma_alpha_re);
  phi_re   ~ normal(0, sigma_phi_re);

  sigma_alpha_re ~ normal(0, 10);
  sigma_phi_re   ~ normal(0, 10);
  
  alpha_F    ~ normal(0, sigma[1]);  # K base levels -- differs across villages
  alpha_M    ~ normal(0, sigma[1]);  # MALE
  alpha_H    ~ normal(0, sigma[1]);  # wHOMOG
  phi_F      ~ normal(0, sigma[4]);  # K base levels -- differs across villages
  phi_M      ~ normal(0, sigma[4]);  # Effect of MALE
  phi_H      ~ normal(0, sigma[4]);  # Effect of HOMOG
  rho_F      ~ normal(0, sigma[3]);
  rho_M      ~ normal(0, sigma[3]);
  rho_H      ~ normal(0, sigma[3]);
  gamma_F    ~ normal(0, sigma[2]);
  gamma_M    ~ normal(0, sigma[2]);
  gamma_H    ~ normal(0, sigma[2]);
  c_H        ~ normal(0, sigma[5]);
  c_MX       ~ normal(0, sigma[5]);
   // Prior on boast implicit uniform over 0, 1
  }
