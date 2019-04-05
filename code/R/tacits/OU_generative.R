####################################################################################
####                Generating Ornstein-Uhlenbeck time series                   ####
####################################################################################

random_walk <- function (total_time,n,sigma,x0) {
  dw <- rnorm(n, 0, sqrt(total_time/n))
  x <- c(x0)
  for (i in 2:(n+1)) {
    x[i]  <-  x[i-1] + sigma*dw[i-1]
  }
  return(x);
}

# OU process: mean-reverting, Gaussian, Markov
# total_time: amount of time, higher T reverts more quickly to the mean
#          n: number of runs
#         mu: mean that trends to
#     theta: mean reversion speed
#      sigma: variance
#         x0: starting point for the series
OU <- function(total_time,n,mu,theta,sigma,x0){
  dw  <- rnorm(n, 0, sqrt(total_time/n))
  dt  <- total_time/n
  x <- c(x0)
  for (i in 2:(n+1)) {
    x[i]  <-  x[i-1] + theta*(mu-x[i-1])*dt + sigma*dw[i-1]
  }
  return(x);
}

# only difference is this takes a vector of means, rather than a single one
OU_drift <- function(total_time,n,mu,theta,sigma,x0){
  dw  <- rnorm(n, 0, sqrt(total_time/n))
  dt  <- total_time/n
  
  x <- rep(x0,n)
  for (i in 2:(n+1)) {
    x[i]  <-  x[i-1] + theta*(mu[i]-x[i-1])*dt + sigma*dw[i-1]
  }
  return(x);
}

# just does one at a time, should get an updated mu and x0 every time
# this will be the one needed if participants are free to make interventions
OU_seq <- function(total_time,n,mu,theta,sigma,x0){
  dw  <- rnorm(1, 0, sqrt(total_time/n))
  dt  <- total_time/n
  
  x <- x0 + theta*(mu-x0)*dt + sigma*dw
  return(x);
}


####################################################################################
####                Generating OU series that track other series                ####
####################################################################################

# beta: 'causal strength', the degree to which the effect tracks the cause
# cause: vector of causal series' values
OU_track <- function(total_time, n, cause, theta, sigma, x0, beta) {
  dw  <- rnorm(n, 0, sqrt(total_time/n))
  dt  <- total_time/n
  
  x <- rep(x0,n)
  for (e in 2:(n+1)) {
    x[e]  <-  x[e-1] + theta*(beta*cause[e-1]-x[e-1])*dt + sigma*dw[e-1]
  }
  return(x);
}

# takes one value at a time
# cause: value of the cause at the previous time step
OU_track_seq <- function(total_time, n, cause, theta, sigma, x0, beta){
  dw  <- rnorm(1, 0, sqrt(total_time/n))
  dt  <- total_time/n
  
  x <- x0 + theta*(beta*cause-x0)*dt + sigma*dw
  return(x);
}

####################################################################################
####                  Generating mutually tracking OU series                    ####
####################################################################################

# x0_A/x0_B: starting point for series A and B
#    beta_A: multiple A applies to B's value
#    beta_B: multiple B applies to A's value
#   delay_A: A tracks onto B with delay_A
#   delay_B: B tracks onto A with delay_B
OU_dual_track <- function(total_time, n, theta, sigma, x0_A, x0_B, beta_A, beta_B, delay_A, delay_B){
  A <- rep(x0_A,n)
  B <- rep(x0_B,n)
  
  for (e in 2:n) {
    # A tracking B
    if (e-delay_A < 1) {
      A[e] <- OU_seq(total_time, n, A[e], theta, sigma, A[e-1])
    } else {
      A[e] <- OU_track_seq(total_time, n, B[e-delay_A], theta, sigma, A[e-1], beta_A)
    }
    
    # B tracking A
    if (e-delay_B < 1) {
      B[e] <- OU_seq(total_time, n, B[e], theta, sigma, B[e-1])
    } else {
      B[e] <- OU_track_seq(total_time, n, A[e-delay_B], theta, sigma, B[e-1], beta_B)
    }
  }
  return(rbind(A,B))
}

####################################################################################
####                                  Networks                                  ####
####################################################################################
# MAKE SURE BETAS ARRAY IS IN THE RIGHT ORDER!!!! Children should always be in columns
# betas should be an n x n array (where n is the # of variables).
#    -1     beta_xy  beta_xz
#  beta_yx     -1    beta_yz
#  beta_zx  beta_zy     -1
OU_general <- function(betas, vars = c(0,0,0), intervention = c(NA,NA,NA), theta=.1, sigma=5, trunc_val=100) {
  
  #If intervention is causal on this timestep (?)
  # if (any(!is.na(intervention)))
  # {
  #   vars[!is.na(intervention)]<-intervention[!is.na(intervention)]
  # }
  
  # generating the mean of the shift, should be a 3-vec
  mean_shift <- theta * (vars %*% betas - vars)
  # actual shift is distorted by some noise
  shift <- mean_shift + sigma*rnorm(length(vars))
  # next state is just the previous value plus auto-generated shift
  movement <- vars + shift
  
  #State of any intervened on variables is overridden
  if (any(!is.na(intervention)))
  {
    movement[!is.na(intervention)]<-intervention[!is.na(intervention)]
  }
  
  # truncating, right now assume symmetry but wouldn't have to
  if (sum(movement>trunc_val)>1)       movement[which(movement>trunc_val)] <- trunc_val
  if (sum(movement<(-trunc_val))<1)    movement[which(movement<(-trunc_val))] <- (-trunc_val)
  
  return(movement)
}


# sample full run
OU_general_run <- function(betas, vars=c(0,0,0), interventions, theta=.1, sigma=5, trunc_val=100, ts_len=100) {
  if(missing(betas))
  {
    betas <- array(c(-1,0,0,1,-1,0,0,0,-1), dim=c(3,3))
  }
  if (missing(vars))
  {
    vars  <- c(100,0,0)
  }
  
  if(!missing(interventions))
  {
    vars[!is.na(interventions[1,])]<-interventions[1,!is.na(interventions[1,])]
  }
  runstore <- matrix(NaN, nrow=ts_len, ncol=ncol(interventions))
  runstore[1,] <- vars
  
  for (e in 2:ts_len) {
    if(!missing(interventions))
    {
      runstore[e,] <- OU_general(vars=runstore[e-1,], intervention = interventions[e-1,], betas = betas, sigma = sigma)
    } else {
      runstore[e,] <- OU_general(vars=runstore[e-1,], betas = betas, sigma = sigma)
    }

  }
  return(runstore)
}














