## num points
num.pts = 100

## define grid
p_grid <- seq(from = 0 , to = 1, length.out = num.pts)

## define prior - 3 different ways.  
##prior <- rep(1, num.pts)
prior <- ifelse(p_grid < 0.5, 0, 1)
##prior <- exp(-5*abs(p_grid - 0.5))

num.trials <- 7
num.waters <- 5

## compute llh at each value in grid
likelihood <- dbinom(num.waters, size = num.trials, prob = p_grid)

## compute produce of likelihood and prior
unstd.posterior <- likelihood * prior

## standardize the posterior so it somes to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## plot points
plot(p_grid, posterior, type = "l", 
     xlab = "probability of water", ylab = "poterior preobability")
mtext("100 points")

#############################################################


library(rethinking)

globe.qa <- map(
  alist(
    w ~ dbinom(9, p), 
    p ~ dunif(0,1)
  ), 
  data=list(w=6))

## display summary of quadratic approx
precis(globe.qa)

## analytic calculation
w<- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from=0, to=1)
# quaddratic approximation
curve(dnorm(x, 0.67, 0.16), lty=2, add=TRUE)