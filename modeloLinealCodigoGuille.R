require(tidyverse)
require(broom)
require(brms)
require(rethinking)

# datos fake
n = 10
x = rnorm(n, 0, 1)
y = 2 + 1.5 * x + rnorm(n, 0, 1)  # a = 2, b = 1.5, sigma = 1
d = data.frame(x, y)

ggplot(data = d, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 3/4) +
  theme_classic() 


# modelo lineal - freq (modelo estimado y su IC del 95%)
fit.0 <- lm(data = d, y ~ 1 + x)
tidy(fit.0, conf.int = T) %>% 
  mutate_if(is.double, round, digits = 2)

ggplot(data = d, aes(x = x, y = y)) +
  stat_smooth(method = "lm", color = "grey92", fill = "grey67", alpha = 1, fullrange = T) +
  geom_point(size = 2, alpha = 3/4) +
  theme_classic() 

# modelo lineal - bayes
fit.1 <- 
  brm(data = d, 
      family = gaussian, # rta gaussiana
      y ~ 1 + x,         # modelo lineal
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1),  class = sigma)),
      seed = 1)

posterior_summary(fit.1)[1:3, ] %>% round(digits = 2)

plot(fit.1)

# plot del mean posterior
ggplot(data = d, mapping = aes(x, y)) +
  geom_abline(intercept = fixef(fit.1)[1], 
              slope     = fixef(fit.1)[2]) +  
  geom_point(size = 2, alpha = 3/4) +
  theme_classic()


# plot de 100 rectas sampleadas de la posterior
post <- as_draws_df(fit.1)
post %>% slice(1:5) 
ggplot(data =  d, aes(x, y)) +
  geom_abline(data = post %>% slice(1:100),
              aes(intercept = b_Intercept, slope = b_x),
              linewidth = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2) +
  theme_classic()


# plot del ajuste del modelo con el intervalo de credibilidad del 95%.

x_seq <- tibble(x = seq(-3,3,by=0.1))

mu_summary <-
  fitted(fit.1, 
         newdata = data.frame(x_seq)) %>%
  data.frame() %>%
  bind_cols(x_seq)

head(mu_summary)

ggplot(data = d, aes(x, y)) +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, linewidth = 1/2) +
  geom_point(size = 2, alpha = 2/3) +
  theme_classic()


# prediction intervals
pred_y <-
  predict(fit.1,
          newdata = x_seq) %>%
  data.frame() %>%
  bind_cols(x_seq)

pred_y %>%
  slice(1:6)

d %>%
  ggplot(aes(x)) +
  geom_ribbon(data = pred_y,
              aes(ymin = Q2.5, ymax = Q97.5),
              fill = "grey83") +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, linewidth = 1/2) +
  geom_point(aes(x,y),
            size = 2, alpha = 2/3) +
  theme_classic()



# Priors
n_lines <- 100

lines <-
  tibble(n = 1:n_lines,
         a = rnorm(n_lines, mean = 0, sd = 10),
         b = rnorm(n_lines, mean = 0, sd = 10)) %>% 
  expand_grid(x = range(d$x)) %>% 
  mutate(y = a + b * x)

head(lines)

lines %>% 
  ggplot(aes(x, y, group = n)) +
  geom_line(alpha = 0.3) +
  theme_classic() +
  geom_point(data = d, aes(x, y), size = 2, alpha = 2/3) + 
  coord_cartesian(ylim = c(-5, 5))
