require(brms)
x = seq(0,20,0.1)
x2 = x*x
y = rnorm(length(x), -20*x + x2, 3)
plot(x,y)
df = data.frame(x,x2,y)
fit <- brm(
  formula = y ~ x + x2,
  data=df)


