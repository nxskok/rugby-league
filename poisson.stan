
data {
  int<lower=1> ng;
  int<lower=1> nt;
  int t1[ng];
  int t2[ng];
  int s1[ng];
  int s2[ng];
}

parameters {
  real h;
  real k;
  real rat[nt];
}

model {
  real d;
  real lp1;
  real lp2;
  real mu1;
  real mu2;
  // priors
  h ~ normal(0, 3);
  k ~ normal(3, 9);
  rat ~ normal(0, 10);
  // likelihood
  for (i in 1:ng) {
    d = rat[t1[i]]-rat[t2[i]];
    lp1 = h + k + d;
    lp2 = k - d;
    mu1 = exp(lp1);
    mu2 = exp(lp2);
    s1[i] ~ poisson(mu1);
    s2[i] ~ poisson(mu2);
  }  
}

