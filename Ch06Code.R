break ## heh, heh

library(rethinking)

sppnames <- c("afarensis", "africanus", "habilis", "boisei", 
              "rudolfensis", "ergaster", "sapiens")
brainvolcc <- c( 438 , 452 , 612 , 521 , 752 , 871 , 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species = sppnames , brain = brainvolcc , mass = masskg )

m6.1 <- lm( brain ~ mass , data = d)

1 - var(resid(m6.1)) / var(d$brain)

m6.2 <- lm( brain ~ mass + I(mass^2) , data = d)
m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3) , data = d)
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) , data = d)
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) , data = d)
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6) , 
            data = d)
1 - var(resid(m6.2)) / var(d$brain)
1 - var(resid(m6.3)) / var(d$brain)
1 - var(resid(m6.4)) / var(d$brain)
1 - var(resid(m6.5)) / var(d$brain)
1 - var(resid(m6.6)) / var(d$brain)

m6.7 <- lm( brain ~ 1 , data = d)

plot( brain ~ mass , d , col = "slateblue" )
for( i in 1:nrow(d) )
{
  d.new <- d[-i, ]
  m0 <- lm( brain ~ mass, d.new)
  abline(m0, col=col.alpha("black", .5))
}


p <- c(0.3, 0.7)
-sum(p*log(p))

m6.1 <- lm(brain ~ mass, d)
(-2) * logLik(m6.1)

d$mass.s <- (d$mass - mean(d$mass)) / sd(d$mass)

m6.8 <- map(
  alist(
    brain ~ dnorm( mu , sigma ) ,
    mu <- a + b*mass.s
  ), data = d, 
  start = list(a = mean(d$brain), b = 0, sigma = sd(d$brain)), 
  method = "Nelder-Mead")

theta <- coef(m6.8)

dev <- (-2) * sum( dnorm(
  d$brain,
  mean = theta[1] + theta[2] * d$mass.s,
  sd = theta[3], 
  log = TRUE))
dev

## ???
N <- 20
kseq <- 1:5
dev <- sapply(kseq, function(k) {
  print(k);
  r <- replicate(1e4, sim.train.test(N=N, k=k));
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})
## parallelize
dev <- sapply(kseq, function(k) {
  print(k);
  r <- mcreplicate(1e4, sim.train.test(N=N, k=k), mc.cores=4);
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})

plot(1:5 , dev[1,] , ylim = c( min(dev[1:2,])-5 , max(dev[1:2,])+10) , 
     xlim = c(1,5.1), xlab = "number of parameters" , ylab = "deviance")

## BIC - p. 192
data(cars)
m <- map(alist(
  dist ~ dnorm(mu,sigma),
  mu <- a + b*speed,
  a ~ dnorm(0, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 30)
), data = cars)
post <- extract.samples(m, n=1000)

n_samples <- 1000
ll <- sapply(1:n_samples , 
             function(s) {
               mu <- post$a[s] + post$b[s]*cars$speed
               dnorm(cars$dist, mu, post$sigma[s] , log = TRUE)
             })

n_cases <- nrow(cars)
lppd <- sapply(1:n_cases , function(i) log_sum_exp(ll[i,]) - log(n_samples) )

sum(lppd)

pWAIC <- sapply(1:n_cases , function(i) var(ll[i,]) )
sum(pWAIC)

-2 * (sum(lppd) - sum(pWAIC)) ## WAIC

waic_vec <- -2*(lppd - pWAIC)
sqrt(n_cases*var(waic_vec))

## using both regularization and information criteria will always
## beat using only one or the other alone
## p. 194

## p. 196 - model averaging and model comparison
## model comparison first

data(milk)
d <- milk[ complete.cases(milk) , ]
d$neocortex <- d$neocortex.perc / 100
dim(d)

## compared models must be fit to exactly the same observations!!!

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))

m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma) )
  ), data = d, start = list(a = a.start, log.sigma = sigma.start))

m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma) ) , 
    mu <- a + bn*neocortex
  ), data = d, start = list(a = a.start, bn = 0, log.sigma = sigma.start))

m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma) ), 
    mu <- a + bm*log(mass)
  ), data = d, start = list(a = a.start, bm = 0, log.sigma = sigma.start))

m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma) ) , 
    mu <- a + bn*neocortex + bm*log(mass)
  ), data = d, start = list(a = a.start, bn=0, bm=0, log.sigma = sigma.start))

WAIC(m6.14)

(milk.models <- compare(m6.11, m6.12, m6.13, m6.14) )

plot(milk.models , SE = TRUE , dSE = TRUE)

diff <- rnorm(1e5, 6.7, 7.26)
sum(diff<0)/1e5

coeftab(m6.11, m6.12, m6.13, m6.14)

plot(coeftab(m6.11, m6.12, m6.13, m6.14))

## model averaging

## counterfactual code

nc.seq <- seq(from = 0.5, to = 0.8, length.out = 30)
d.predict <- list(
  kcal.per.g = rep(0, 30),
  neocortex = nc.seq,
  mass = rep(4.5, 30)
)
pred.m6.14 <- link( m6.14 , data = d.predict )
mu <- apply( pred.m6.14 , 2 , mean )
mu.PI <- apply( pred.m6.14 , 2 , PI)

plot(kcal.per.g ~ neocortex , d , col=rangi2)
lines(nc.seq , mu , lty = 2)
lines(nc.seq , mu.PI[1,] , lty = 2)
lines(nc.seq , mu.PI[2,] , lty = 2)

## ensemble the models, using weights as proportions

milk.ensemble <- ensemble( m6.11, m6.12, m6.13, m6.14, data = d.predict)
mu <- apply( milk.ensemble$link , 2 , mean )
mu.PI <- apply( milk.ensemble$link , 2 , PI)
lines(nc.seq , mu)
shade(mu.PI, nc.seq)

