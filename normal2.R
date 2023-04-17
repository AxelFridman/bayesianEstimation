# Datos
heights <- d$height

# log.likelihood
log.likelihood <- function(mu, sigma, data) {
  sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# priors
mu_prior <- function(mu) {
  dnorm(mu, mean = 178, sd = 20, log = TRUE)
}
sigma_prior <- function(sigma) {
  dunif(sigma, min = 0, max = 50, log = TRUE)
}

# funcion de propuesta
mu_proposal <- function(mu, scale) {
  rnorm(1, mean = mu, sd = scale)
}
sigma_proposal <- function(sigma, scale) {
  abs(rnorm(1, mean = sigma, sd = scale))
}

# Metropolis-Hastings 
metropolis_hastings <- function(data, iterations, burnin, thinning, mu_start, sigma_start, mu_proposal_scale, sigma_proposal_scale) {
  
  mu_samples <- numeric(iterations)
  sigma_samples <- numeric(iterations)
  
  mu    = mu_start
  sigma = sigma_start
  
  for (i in 1:iterations) {
    
    # propongo nuevos valores para mu y sigma
    mu_proposal <- mu_proposal(mu, mu_proposal_scale)
    sigma_proposal <- sigma_proposal(sigma, sigma_proposal_scale)
    
    # log de la variable ratio
    log_alpha_mu <- log.likelihood(mu_proposal, sigma, data) + mu_prior(mu_proposal) - log.likelihood(mu, sigma, data) - mu_prior(mu)
    log_alpha_sigma <- log.likelihood(mu, sigma_proposal, data) + sigma_prior(sigma_proposal) - log.likelihood(mu, sigma, data) - sigma_prior(sigma)
    
    # acetpto o rechazo
    if (log(runif(1)) < log_alpha_mu) {
      mu <- mu_proposal
    }
    if (log(runif(1)) < log_alpha_sigma) {
      sigma <- sigma_proposal
    }
    
    # guardo los valores
    mu_samples[i] <- mu
    sigma_samples[i] <- sigma
  }
  
  # borro transitorio
  mu_samples <- mu_samples[(burnin + 1):iterations]
  sigma_samples <- sigma_samples[(burnin + 1):iterations]
  mu_samples <- mu_samples[seq(1, length(mu_samples), thinning)]
  sigma_samples <- sigma_samples[seq(1, length(sigma_samples), thinning)]
  
  # guardo resultado
  return(list(mu = mu_samples, sigma = sigma_samples))
}

# Corro Metropolis-Hastings 
samples <- metropolis_hastings(data = heights, iterations = 10000, burnin = 1000, thinning = 10, mu_start = 175, sigma_start = 10, mu_proposal_scale = 2, sigma_proposal_scale = 2)

# Plots de la distribucion posterior
par(mfrow = c(2, 1))
hist(samples$mu, main = "Posterior distribution of mu", xlab = "mu", breaks = 50)
hist(samples$sigma, main = "Posterior distribution of sigma", xlab = "sigma", breaks = 50)
