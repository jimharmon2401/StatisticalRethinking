## chapter 3 code

PrPV <- 0.95
PrPM <- 0.01
PrV <- 0.001
PrP <- PrPV * PrV + PrPM * (1 - PrV)
PrVP <- PrPV * PrV / PrP
PrVP

## p. 52

p_grid <- seq( from = 0, to = 1, length.out = 1000 )
prior <- rep(1, 1000)
likelihood <- dbinom ( 6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob = posterior, size=1e4, replace = TRUE)

plot(samples)

library(rethinking)
dens(samples)

# add up posterior probabilty wher p < 0.5
sum ( posterior[ p_grid < 0.5])

sum(samples < 0.5) / 1e4 ## 0.1743, close to text

sum(samples > 0.5 & samples < 0.75) / 1e4 ## prob between 0.5 and 0.75

quantile(samples, 0.8) ## 0.7607608

quantile(samples, c(0.1, 0.9))

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

PI(samples, prob = 0.5) ## p. 56

HPDI(samples, prob = 0.5)

p_grid[which.max(posterior)] ## 1

chainmode(samples, adj=0.01) ## 0.997

mean(samples)
median(samples)

sum(posterior*abs(0.5 - p_grid))

loss <- sapply(p_grid, function(d) sum(posterior*abs(d - p_grid)))

plot(loss)

p_grid[which.min(loss)] ## 0.84084

#####################################

## copute probabilities of each event happening
dbinom(0:2, size = 2, prob = 0.7)

rbinom(10, size=2, prob = 0.7)

## generate 100,000 dummy obs
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)

table(dummy_w)/1e5 ## show table of "empirical" probs

## now let's do the same for the globe example
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
## and plot the simple line histogram
simplehist(dummy_w, xlab = "dummy water count")
table(dummy_w) / length(dummy_w)

w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

## now, take samples from the posterior
## make sure to use samples from ep.52 size=9 computations
## in the first block above (not the size=3 samples)
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)