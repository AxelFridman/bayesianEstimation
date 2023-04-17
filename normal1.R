require(coda)
require(mvtnorm)
require(devtools)
require(loo)
require(dagitty)
require(rethinking)

## Priors
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

## Prior predictive checks
sample_mu    = rnorm( 1e4 , 178 , 20 )
sample_sigma = runif( 1e4 , 0 , 50 )
prior_h      = rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

## Prior predictive checks (con otros priors)
sample_mu = rnorm( 1e4 , 178 , 100 )
prior_h   = rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

## Datos
data(Howell1)
d <- Howell1
d <- d[ d$age >= 18 , ]

## Aproximación de grilla
mu.list    = seq( from=150, to=160 , length.out=100 )
sigma.list = seq( from=7 , to=9 , length.out=100 )
post       = expand.grid( mu=mu.list , sigma=sigma.list )

for (i in 1:nrow(post)){
  post$LL[i] = sum( log(dnorm(d$height, post$mu[i], post$sigma[i])) )
}

post$prod = post$LL + dnorm( post$mu , 178 , 20 , TRUE ) + dunif( post$sigma , 0 , 50 , TRUE )
post$prod = post$prod - max(post$prod)
post$prob = exp( post$prod )

## Plots de la distribución posterior 
contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )

## Muestras de la posterior
sample.rows  = sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
sample.mu    = post$mu[ sample.rows ]
sample.sigma = post$sigma[ sample.rows ]

plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

dens( sample.mu, adj=1 )
dens( sample.sigma )

PI( sample.mu )
PI( sample.sigma )



## La posterior no tiene por qué ser "gaussiana"
d2 = sample( d$height , size=20 )

## Repito la aproximación de grilla
mu.list    = seq( from=150, to=160 , length.out=100 )
sigma.list = seq( from=7 , to=9 , length.out=100 )
post.20    = expand.grid( mu=mu.list , sigma=sigma.list )

for (i in 1:nrow(post)){
  post.20$LL[i] = sum( log(dnorm(d2, post.20$mu[i], post.20$sigma[i])) )
}

post.20$prod = post.20$LL + dnorm( post.20$mu , 178 , 20 , TRUE ) + dunif( post.20$sigma , 0 , 50 , TRUE )
post.20$prod = post.20$prod - max(post.20$prod)
post.20$prob = exp( post.20$prod )

## Repito tomar muestras de la posterior
sample.rows  = sample(1:nrow(post.20), size=1e4, replace=TRUE, prob=post.20$prob)
sample.mu    = post.20$mu[ sample.rows ]
sample.sigma = post.20$sigma[ sample.rows ]

dens( sample.sigma )

