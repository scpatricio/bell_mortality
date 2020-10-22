# pacotes
if(!require(lamW)) {
  install.packages("lamW")}

##################### Leis ##################### 
# Gompertz
gompertz = function(t, theta){
  a = theta[1]
  b = theta[2]
  
  return(a*exp(b*t))
}

# Gompertz-Makeham
makeham = function(t, theta){
  a = theta[1]
  b = theta[2]
  c = theta[3]
  
  return(a*exp(b*t)+c)
}


######################  log-verossimilhancas ##################### 
# distribuicao bell
logLike_bell = function(theta, Dx, Ex, mu, t){
  theta = abs(theta)
  
  lamb = Ex*mu(t, theta)
  W0 = lamW::lambertW0(lamb)
  log_lik = Dx*log(W0)-exp(W0)
  
  return(-sum(log_lik))
}

# distribuicao binomial negativa
serie = function(j, phi){
  out = NULL
  
  for(i in 1:length(j)){
    out[i] = sum(log(0:(j[i]-1) + phi))
  }
  
  return(out)
}

logLike_BN = function(theta, Dx, Ex, mu, t){
  theta = abs(theta)
  phi = theta[1]
  
  lamb = Ex*mu(t, theta[-1])

  log_lik = serie(Dx, phi) +Dx*log(lamb)-(Dx+phi)*log(phi+lamb)+phi*log(phi)
  
  return(-sum(log_lik, na.rm = T))
}

##################### vida media residual ##################### 
# Gomeprtz
# E1 function
E1 = function(z){
  integrate(function(t){
    (exp(-t))/t
  }, z, Inf)$value
}

ex_gomp = function(t, a, b){
  exp(a*exp(b*t)/b)*E1(a*exp(b*t)/b)/b
}

# Makeham
# Gamma-Imcompleta superior
Gamma_Inc_sup = function(s, z){
  integrate(function(t){
    t^(s-1)*exp(-t)
  }, z, Inf)$value
}

ex_mak = function(t, a, b, c){
  (exp(a*exp(b*t)/b)/b)*((a*exp(b*t)/b)^(c/b))*Gamma_Inc_sup(-c/b,a*exp(b*t)/b)
}

