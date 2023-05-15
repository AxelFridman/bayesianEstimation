library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)

require(brms)

bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40), 
                       prior_aux = exponential(1),
                       chains = 4, iter = 5000*2, seed = 84735)



mcmc_dens_overlay(bike_model)

tidy(bike_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.80)

# 50 simulated model lines
bikes %>%
  add_fitted_draws(bike_model, n = 4000) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.15) + 
  geom_point(data = bikes, size = 0.05)
