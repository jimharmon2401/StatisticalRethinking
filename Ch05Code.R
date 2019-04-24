break ## heh, heh

## p. 122

library(rethinking)

data(WaffleDivorce)
d<-WaffleDivorce

d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / 
  sd(d$MedianAgeMarriage)

m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma), 
    mu <- a + bA * MedianAgeMarriage.s , 
    a ~ dnorm(10, 10) , 
    bA ~ dnorm(0, 1), 
    sigma ~ dunif( 0 , 10 )
  ) , data = d)

MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

precis(m5.1)

d$Marriage.s<-(d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

m5.2 <- map(
  alist(
    Divorce ~ dnorm( mu, sigma) , 
    mu <- a + bR * Marriage.s, 
    a ~ dnorm(10, 10) , 
    bR ~ dnorm(0,1), 
    sigma ~ dunif(0, 10)
  ), data = d)

MR.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu.MR <- link(m5.2, data = data.frame(Marriage.s = MR.seq))
mu.MR.PI<-apply(mu.MR, 2, PI)

plot(Divorce ~ Marriage.s, data = d, col = rangi2)
abline(m5.2)
shade(mu.MR.PI, MR.seq)

## p. 125, first multivariate regression model

m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s + bA * MedianAgeMarriage.s, 
    a ~ dnorm(10, 10), 
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
  ), data = d)

precis(m5.3)

plot(precis(m5.3))

## predictor residual plots
## p. 126

m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s, 
    a ~ dnorm(0,10), 
    b ~ dnorm(0,1), 
    sigma ~ dunif(0, 10)
  ), data = d)

## compute residuals
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
m.resid <- d$Marriage.s - mu

plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
# loop over states
for(i in 1:length(m.resid))
{
  x <- d$MedianAgeMarriage.s[i]
  y <- d$Marriage.s[i]
  lines(c(x,x), c(mu[i], y), lwd = 0.5, col = col.alpha("black",0.7))
}

## counterfactual plots
## p. 129
## prepare new counterfactual data
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(
  Marriage.s=R.seq,
  MedianAgeMarriage.s = A.avg
)

## compute counterfactual mean divorce (muj)
mu <- link(m5.3, data=pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

## simulate counterfactual divorce outcomes
R.sim <- sim(m5.3, data = pred.data, n=1e4)
R.PI <- apply(R.sim, 2, PI)

## display predictions, hiding raw data with type = "n"
plot(Divorce ~ Marriage.s, data = d, type = "n")
mtext("MedianAgeMarriage.s = 0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)

## now, do the other counterfactual
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to = 3.5, length.out = 30)
pred.data2 <- data.frame(
  Marriage.s=R.avg,
  MedianAgeMarriage.s = A.seq
)

mu <- link(m5.3, data=pred.data2)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

A.sim <- sim(m5.3, data = pred.data2, n=1e4)
A.PI <- apply(A.sim, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, type = "n")
mtext("Marriage.s = 0")
lines(A.seq, mu.mean)
shade(mu.PI, A.seq)
shade(A.PI, A.seq)


## posterior prediction plots 
## p. 132

mu <- link(m5.3)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

divorce.sim <- sim(m5.3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI)

plot(mu.mean ~ d$Divorce, col=rangi2, ylim = range(mu.PI), 
     xlab = "Observed divorce", ylab = "Predicted divorce")
abline(a=0, b=1, lty=2)
for(i in 1:nrow(d))
{
  lines(rep(d$Divorce[i],2), c(mu.PI[1,i],mu.PI[2,i]), col=rangi2)
}
#identify(x=d$Divorce, y=mu.mean, labels=d$Loc, cex=0.8)

## residual plot
divorce.resid <- d$Divorce - mu.mean
o <- order(divorce.resid)
dotchart(divorce.resid[o], labels = d$Loc[o], xlim=c(-6,5), cex = 0.6)
abline(v=0, col=col.alpha("black", 0.2))
for(i in 1:nrow(d))
{
  j <- o[i]
  lines(d$Divorce[j]-c(mu.PI[1,j], mu.PI[2,j]), rep(i,2))
  points(d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2), 
         pch = 3, cex = 0.6, col="gray")
}

## overthinking p. 134-135

N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
d_spur <- data.frame(y, x_real, x_spur)

pairs(d_spur)

summary(lm(y ~ x_real + x_spur, data = d_spur))

## masked relationship
## p. 135

data(milk)
d <- milk
str(d)

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + bn*neocortex.perc, 
    a ~ dnorm(0, 100), 
    bn ~ dnorm(0, 1), 
    sigma ~ dunif(0,1)
  ), data = d
)
## ERROR, problem is missing values
d$neocortex.perc

dcc <- d[complete.cases(d), ]

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + bn*neocortex.perc, 
    a ~ dnorm(0, 100), 
    bn ~ dnorm(0, 1), 
    sigma ~ dunif(0,1)
  ), data = dcc
)

precis(m5.5, digits = 3)

coef(m5.5)["bn"] * (76 - 55)

np.seq <- 0:100
pred.data <- data.frame(neocortex.perc = np.seq)

mu <- link(m5.5, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

## use log-mass

dcc$log.mass <- log(dcc$mass)

m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + bm*log.mass, 
    a ~ dnorm(0, 100), 
    bm ~ dnorm(0, 1), 
    sigma ~ dunif(0,1)
  ), data = dcc)

precis(m5.6)

## figure 5.7, upper right
np.seq <- seq(from=-3, to=5, length.out = 30)
pred.data <- data.frame(log.mass = np.seq)

mu <- link(m5.6, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

## both predictors in model
## p. 139

m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + bn*neocortex.perc + bm*log.mass, 
    a ~ dnorm(0, 100), 
    bn ~ dnorm(0, 1), 
    bm ~ dnorm(0, 1), 
    sigma ~ dunif(0,1)
  ), data = dcc)
precis(m5.7)

## counterfactual plots
## p. 140

mean.log.mass <- mean(log(dcc$mass))
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass
)

mu <- link(m5.7, data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

## other counterfactual

mean.neoc <- mean(dcc$neocortex.perc)
np.seq <- seq(from=-3, to=5, length.out = 30)
pred.data <- data.frame(
  neocortex.perc = mean.neoc,
  log.mass = np.seq
)

mu <- link(m5.7, data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

## overthinking - p. 141

N <- 100
rho <- 0.7
x_pos <- rnorm(N)
x_neg <- rnorm(N, rho*x_pos, sqrt(1-rho^2))
y <- rnorm(N, x_pos - x_neg)
d <- data.frame(y, x_pos, x_neg)

pairs(d)
summary(lm(y ~ x_pos, data = d))
summary(lm(y ~ x_neg, data = d))
summary(lm(y ~ x_pos + x_neg, data = d))

## multicollinear legs
## p. 142

N <- 100
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop * height + rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)
d <- data.frame(height, leg_left, leg_right)

m5.8<- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + bl*leg_left + br*leg_right, 
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d)
precis(m5.8)

plot(precis(m5.8))

post <- extract.samples(m5.8)
plot(bl ~ br, post, col=col.alpha(rangi2, 0.1), pch=16)

sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br")

m5.9 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + bl * leg_left, 
    a ~ dnorm(10,100), 
    bl ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d)
precis(m5.9)
plot(precis(m5.9))

## reload milk
data(milk)
d <- milk

m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf*perc.fat, 
    a ~ dnorm(0.6, 10), 
    bf ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ), data = d)

m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose, 
    a ~ dnorm(0.6, 10), 
    bl ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ), data = d)

precis(m5.10, digits=3)
precis(m5.11, digits=3)

m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf*perc.fat + bl*perc.lactose, 
    a ~ dnorm(0.6, 10), 
    bf ~ dnorm(0, 1), 
    bl ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ), data = d)
precis(m5.12, digits = 3)

pairs( ~ kcal.per.g + perc.fat + perc.lactose, 
       data = d, col=rangi2)
cor(d$perc.fat, d$perc.lactose)

## overthinking, simulating collinearity - p. 150

sim.coll <- function(r = 0.9)
{
  d$x <- rnorm(nrow(d), mean=r*d$perc.fat, 
               sd = sqrt( (1-r^2)*var(d$perc.fat)))
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  sqrt(diag(vcov(m)))[2]
}
rep.sim.coll <- function(r=0.9, n=100)
{
  stddev <- replicate(n, sim.coll(r) )
  mean(stddev)
}
r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r=z, n=100))
plot(stddev ~ r.seq, type = "l", col=rangi2, lwd=2, xlab="correlation")

## post-treatment bias
## p. 150

N <- 100
h0 <- rnorm(N, 10, 2)

treatment <- rep(0:1, each=N/2)
fungus <- rbinom(N, size=1, prob=0.5-treatment*0.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)

d <- data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)

m5.13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment + bf*fungus,
    a ~ dnorm(0, 100),
    c(bh, bt, bf) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = d)
precis(m5.13)

m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0, 100),
    c(bh, bt) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = d)
precis(m5.14)


## categorical variables
## code - p. 153

data(Howell1)
d <- Howell1
str(d)

m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + bm*male, 
    a ~ dnorm(178, 100), 
    bm ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ), data = d)
precis(m5.15)

