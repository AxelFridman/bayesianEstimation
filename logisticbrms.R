require(brms)
require(broom)
require(broom.mixed)
require(tidyverse)
require(tidybayes)
require(modelr)
require(ggdist)

x  = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
xc = x - mean(x)
n  = c(59, 60, 62, 56, 63, 59, 62, 60)
y  = c(6, 13, 18, 28, 52, 52, 61, 60)

d = data.frame(x, n, y, prop = y/n) %>%  mutate(xc)

fit <- brm(formula = y | trials(n) ~ xc,  
           data    = d, 
           family  = binomial(link = "logit"),
           prior   = c(prior(normal(0, 10), class = "Intercept"),
                       prior(normal(0, 10), class = "b", coef = "xc")),
           iter = 1000, warmup = 500, chains = 2,
           file = "ejemplo_logistic")

plot(fit)
rhat(fit)
pp_check(fit, type = "ribbon", x = "xc")

alfa = as_draws(fit)[[1]]$b_Intercept
beta = as_draws(fit)[[1]]$b_xc

xc.seq = seq(-0.1,0.1,by=0.01)
yp = rep(NA, length(xc.seq))
for (i in 1:length(xc.seq)){
  p     = 1 / (1 + exp(-(alfa + beta * xc.seq[i])))
  yp[i] = mean(p)
}
plot(xc, y/n)
lines(xc.seq, yp)

# prior
fit.prior <- brm(formula = y | trials(n) ~ xc,  
                 data    = d, 
                 family  = binomial(link = "logit"), 
                 prior   = c(prior(normal(0, 10), class = "Intercept"), 
                             prior(normal(0, 10), class = "b", coef = "xc")),
                 sample_prior = "only",
                 file = "ejemplo_logistic_prior")

pp_check(fit.prior, type = "ribbon", x = "xc")

posterior_samples(fit.prior) %>% 
  mutate(p = 1 / (1 + exp(-(b_Intercept + b_xc * (1.88-mean(x)))))) %>% 
  ggplot(aes(x = p)) + geom_histogram()


####
get_prior(formula = y | trials(n) ~ xc,  
  data    = d, 
  family  = binomial(link = "logit"))



alfa = as_draws(fit.prior)[[1]]$b_Intercept
beta = as_draws(fit.prior)[[1]]$b_xc

xc.seq = seq(-0.1,0.1,by=0.01)
yp = rep(NA, length(xc.seq))
for (i in 1:length(xc.seq)){
  p     = 1 / (1 + exp(-(alfa + beta * xc.seq[i])))
  yp[i] = mean(p)
}
plot(xc, y/n)
lines(xc.seq, yp)

####
d %>%
  data_grid(xc = seq_range(xc, n = 101), n = 100) %>%
  add_predicted_draws(fit) %>%
  ggplot(aes(x = xc, y = 100*prop)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = d, size = 2) +
  scale_fill_brewer() +
  theme_classic()
