break ## heh, heh

##########
## EASY
##########

## 4E1
The first line is the likelihood

## 4E2
2, mu and sigma

## 4E3
P(mu, sigma | y) = P(y, | mu, sigma) p(mu) p(sigma) / p(y)

## 4E4
The second line is the linear model.  

## 4E5
Three.  Alpha, beta, sigma.  Mu is a function of alpha, beta, and the data

##########
## MEDIUM
##########

## 4M1
## y_i ~ dnorm(mu, sigma)
## mu ~ dnorm(0, 10)
## sigma ~ dunif(0, 10)
## sim obs'd heights from the prior, not the posterior

mu.obs <- rnorm(1e4, 0, 10)

## 4M2
## write map formula

m4M2<-map(
  alist(
    height ~ dnorm(mu, sigma)
    mu ~ dnorm(0, 10)
    sigma ~ dunif(0, 10)
  ), 
  data = ###
)

## 4M3
## translate into a math model def
y_i ~ Normal(mu_i, sigma)
mu_i = alpha + beta * x_i
alpha ~ Normal(0, 50)
beta ~ Uniform(0, 10)
sigma ~ Uniform(0, 50)

## 4M4
height_i ~ Normal(mu_i, sigma) ## regression model
mu_i = alpha + beta * year_i ## linear model
alpha ~ Normal(150, 20) ## diffuse prior on alpha, roughly centered at 150 cm ~ 5 ft.
beta ~ Normal(0, 10) ## diffuse prior on beta
sigma ~ Uniform(0, 50) ## diffuse prior on sigma

## 4M5
Yes.  I would adjust the prior on alpha to be centered at 120, since that 
represents height at time zero.  For the second part, I might center beta 
around a positive number, indicating that I expect it to be positive.

## 4M6
In this case, I might adjust the prior on sigma to be uniform(0,9), to indicate
that the variance is never more than 64, but it could be in the future.  :)

##########
## HARD
##########

## 4H1
## use model based predictions.  
## first, recreate the model

library(rethinking)
data(Howell1)
d <- Howell1

d$weight.s <- ( d$weight - mean(d$weight) ) / sd(d$weight)
d$weight.s2 <- d$weight.s^2
d$weight.s3 <- d$weight.s^3

m4H1 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b1*weight.s + b2*weight.s2, 
    a ~ dnorm(178, 100), 
    b1 ~ dnorm(0, 10), 
    b2 ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ),
  data = d)

new_dat<-c(46.95, 43.72, 64.78, 32.59, 54.63)
new_dat.c<- (new_dat - mean(d$weight)) / sd(d$weight)
pred_dat <- list(weight.s = new_dat.c, 
                 weight.s2 = new_dat.c^2, 
                 weight.s3 = new_dat.c^3)
mu <- link(m4H1, data = pred_dat)
mu.PI <- apply(mu, 2, PI, prob=0.89)
mu.mean <- apply(mu, 2, mean)

exp     89% int
158.2   157.7, 158.6
155.9   155.4, 156.4
156.0   154.1, 157.9
141.9   141.4, 142.5
160.3   159.4, 161.1

## 4H2
d3 <- d[d$age < 18, ]
str(d3) ## check!

plot(height ~ weight, data = d3)

## 4H2-a 
d3$weight.s <- (d3$weight - mean(d3$weight)) / sd(d3$weight)

m4H1 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b1*weight.s, 
    a ~ dnorm(110, 100), 
    b1 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d3)

precis(m4H1)
        Mean StdDev   5.5%  94.5%
a     108.32   0.61 107.35 109.29
b1     24.23   0.61  23.25  25.20
sigma   8.44   0.43   7.75   9.13

a is the average height at zero standardized weight, or 18.4 kg.  b1 is the 
average increase in height for one standard deviation change in weight.  That is
8.9 kg.  So for every 10 kg, the child changes in height on average 24.23 * 10 / 8.9
or 27.2 cm.  Sigma is the average standard deviation around the mean. 

## 4H2-b
## plot

weight.seq <- seq(from = -2.2, to = 3, length.out = 30)
pred_dat <- list(weight.s = weight.seq)
mu <- link(m4H1, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4H1, data=pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

plot(height ~ weight.s, d3, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

## 4H2-c

Model is not doing a great job of picking up on the curvature in the data.
A better model would include higher order terms (quadratic or cubic) to pick up
on the curvature in the data

## 4H3
## model height on log(weight)

library(rethinking)
data(Howell1)
d <- Howell1

d$weight.l <- log(d$weight)

m4H3 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b1*weight.l, 
    a ~ dnorm(178, 100), 
    b1 ~ dnorm(0, 100),
    sigma ~ dunif(0, 50)
  ),
  data = d)

precis(m4H3)
        Mean StdDev   5.5%  94.5%
a     -23.78   1.34 -25.92 -21.65
b1     47.08   0.38  46.46  47.69
sigma   5.13   0.16   4.89   5.38

"a" is the expected height for zero log weight, or weight = 1 kg.  so it is 
really uninterpretable because even babies weight more than this.  
"b1" is the average change in height for a one-unit increase in log weight.  
Which is also hard to interpret because a one unit increase in log weight means
different amounts at different log-weights.  Sigma is still the mean variation around
height, which is the same regardless.

plot(height ~ weight.l, data = d) ## 1.4 - 4.0+

## 4H3-b

## make 97% HPDI for mean and predicted heights.

weight.seq <- seq(from = 1.4, to = 4.1, length.out = 30)
pred_dat <- list(weight.l = weight.seq)
mu <- link(m4H3, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.97)
sim.height <- sim(m4H3, data=pred_dat)
height.HPDI <- apply(sim.height, 2, HPDI, prob=0.97)

## transform weight.seq back to non-logged values

weight.seq.exp <- exp(weight.seq)

plot(height ~ weight, data = Howell1, col = col.alpha(rangi2, 0.4))
lines(weight.seq.exp, mu.mean)
shade(mu.HPDI, weight.seq.exp)
shade(height.HPDI, weight.seq.exp)
