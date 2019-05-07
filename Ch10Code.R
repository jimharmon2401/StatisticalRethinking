break ## heh, heh

library(rethinking)
data(chimpanzees)
d <- chimpanzees

m10.1 <- map(
  alist(
    pulled_left ~ dbinom(1, p) , 
    logit(p) <- a , 
    a ~ dnorm(0, 10)
  ), 
  data = d)
precis(m10.1)

logistic(0.32)
logistic(c(0.18, 0.46))

m10.2 <- map(
  alist(
    pulled_left ~ dbinom(1, p) , 
    logit(p) <- a + bp * prosoc_left , 
    a ~ dnorm(0, 10) , 
    bp ~ dnorm(0, 10)
  ) , 
  data = d )

m10.3 <- map(
  alist(
    pulled_left ~ dbinom(1, p) , 
    logit(p) <- a + (bp + bpC * condition) * prosoc_left , 
    a ~ dnorm(0, 10) , 
    bp ~ dnorm(0, 10) , 
    bpC ~ dnorm(0, 10)
  ) , 
  data = d )

compare(m10.1, m10.2, m10.3)
plot(compare(m10.1, m10.2, m10.3))

precis(m10.3)

exp(0.61)

logistic(4)

logistic(4 + 0.61)

## model averaging

## dummy data
d.pred <- data.frame(
  prosoc_left = c(0,1,0,1), 
  condition = c(0,0,1,1)
  )

## build prediction ensemble
chimp.ensemble <- ensemble(m10.1, m10.2, m10.3, data = d.pred)

## summarize
pred.p <- apply( chimp.ensemble$link, 2, mean )
pred.p.PI <- apply( chimp.ensemble$link, 2, PI )

## empty plot frame with good axes
plot( 0, 0, type = "n", xlab = "prosoc_left/condition" ,
      ylab = "proportion pulled left" , ylim = c(0,1) , xaxt = "n" ,
      xlim = c(1,4) )
axis( 1 , at = 1:4 , labels = c("0/0", "1/0", "0/1", "1/1") )

#E plot raw data, one trend for each of 7 individual chimpanzees
## will use by() here; see Overthinking box for explanation
p <- by( d$pulled_left , 
         list(d$prosoc_left, d$condition, d$actor) , mean )
for ( chimp in 1:7 )
  lines( 1:4 , as.vector(p[,,chimp]) , col=rangi2 , lwd1=1.5)

## now superimpose posterior predictions
lines(1:4 , pred.p )
shade( pred.p.PI , 1:4 )

## clean NAs from the data
d2 <- d
d2$recipient <- NULL

## re-use map fit to get the formula
m10.3stan <- map2stan( m10.3 , data = d2 , iter = 1e4 , warmup = 1000)
precis(m10.3stan)

pairs(m10.3stan)


m10.4 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p) , 
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left , 
    a[actor] ~ dnorm(0, 10), 
    bp ~ dnorm(0, 10), 
    bpC ~ dnorm(0, 10)
  ), 
  data = d2 , chains = 2 , iter = 2500 , warmup = 500)

precis(m10.4 , depth=2)

post <- extract.samples( m10.4 )
str( post )

dens( post$a[,2] )

## plot posterior predictions

## chimp <- 1 ## made a for-loop instead
for(chimp in 1:7)
{
  d.pred <- list(
    pulled_left = rep(0, 4), 
    prosoc_left = c(0,1,0,1),
    condition = c(0,0,1,1),
    actor = rep(chimp,4)
  )
  
  link.m10.4 <- link( m10.4 , data = d.pred )
  pred.p <- apply( link.m10.4 , 2 , mean )
  pred.p.PI <- apply( link.m10.4 , 2 , PI )
  
  plot( 0 , 0 , type = "n" , xlab = "prosoc_left/condition" , 
        ylab = "proportion pulled left" , ylim = c(0,1) , xaxt = "n" ,
        xlim = c(1,4) , yaxp = c(0,1,2) )
  axis( 1 , at = 1:4 , labels = c("0/0", "1/0", "0/1", "1/1"))
  mtext( paste( "actor" , chimp ) )
  
  p <- by( d$pulled_left , 
           list(d$prosoc_left, d$condition , d$actor ) , mean )
  lines( 1:4 , as.vector(p[,,chimp]) , col = rangi2 , lwd = 2 )
  lines(1:4 , pred.p )
  
  shade( pred.p.PI , 1:4)
}


data(chimpanzees)
d <- chimpanzees
d.aggregated <- aggregate( d$pulled_left , 
                           list(prosoc_left = d$prosoc_left, condition = d$condition , actor = d$actor) ,
                           sum)

m10.5 <- map(
  alist(
    x ~ dbinom(18, p) , 
    logit(p) <- a + (bp + bpC*condition)*prosoc_left , 
    a ~ dnorm(0, 10) , 
    bp ~ dnorm(0, 10) , 
    bpC ~ dnorm(0, 10)
  ) , 
  data = d.aggregated )
precis(m10.5)

##library(rethinking)
data(UCBadmit)
d <- UCBadmit


d$male <- ifelse( d$applicant.gender == "male" , 1 , 0 )
m10.6 <- map(
  alist(
    admit ~ dbinom( applications , p ) ,
    logit(p) <- a + bm*male , 
    a ~ dnorm(0, 10) , 
    bm ~ dnorm(0, 10)
  ), 
  data = d )

m10.7 <- map(
  alist(
    admit ~ dbinom( applications , p ) ,
    logit(p) <- a ,  
    a ~ dnorm(0, 10) 
  ), 
  data = d )

compare( m10.6 , m10.7 )

precis(m10.6)

post <- extract.samples( m10.6 )
p.admit.male <- logistic( post$a + post$bm)
p.admit.female <- logistic( post$a )
diff.admit <- p.admit.male - p.admit.female
quantile( diff.admit , c(0.025, 0.5, 0.975))

dens(diff.admit)

postcheck( m10.6 , n = 1e4)
## draw lines connecting points from same dept
for( i in 1:6) 
{
  x <- 1 + 2*(i-1)
  y1 <- d$admit[x] / d$applications[x]
  y2 <- d$admit[x+1] / d$applications[x+1]
  lines( c(x, x+1) , c(y1, y2) , col = rangi2 , lwd = 2 )
  text( x+0.5 , (y1+y2)/2 + 0.05 , d$dept[x] , cex = 0.8 , col = rangi2 )
}

## make index
d$dept_id <- coerce_index(d$dept)

## model with unique intercept for each dept
m10.8 <- map(
  alist(
    admit ~ dbinom( applications , p ) , 
    logit(p) <- a[dept_id] , 
    a[dept_id] ~ dnorm(0, 10)
  ) , data = d)

m10.9 <- map(
  alist(
    admit ~ dbinom( applications , p ) , 
    logit(p) <- a[dept_id] + bm*male, 
    a[dept_id] ~ dnorm(0, 10) , 
    bm ~ dnorm(0, 10)
  ) , data = d)

compare( m10.6 , m10.7 , m10.8 , m10.9)

precis( m10.9 , depth = 2 )


postcheck( m10.9 , n = 1e4)
## draw lines connecting points from same dept
for( i in 1:6) 
{
  x <- 1 + 2*(i-1)
  y1 <- d$admit[x] / d$applications[x]
  y2 <- d$admit[x+1] / d$applications[x+1]
  lines( c(x, x+1) , c(y1, y2) , col = rangi2 , lwd = 2 )
  text( x+0.5 , (y1+y2)/2 + 0.05 , d$dept[x] , cex = 0.8 , col = rangi2 )
}

m10.9stan <- map2stan( m10.9 , chains = 2 , iter = 2500 , warmup = 500)
precis(m10.9stan, depth = 2)

## skipped some glm code on p. 310

## outcome and predictor almost perfectly associated
y <- c( rep(0, 10) , rep( 1, 10 ))
x <- c( rep(-1, 9) , rep( 1, 11) )
## fit binomial glm
m.bad <- glm(y ~ x , data = list(y = y, x = x) , family = binomial )
precis(m.bad)

m.good <- map(
  alist(
    y ~ dbinom( 1 , p ) , 
    logit(p) <- a + b*x , 
    c(a, b) ~ dnorm(0, 10)
  ), data = list(y = y, x = x) )
precis(m.good)

m.good.stan <- map2stan( m.good )
pairs(m.good.stan)


###############################
## Poisson Regression - p. 311
###############################

y <- rbinom(1e5, 1000, 1/1000)
c(mean(y), var(y))

library(rethinking)
data(Kline)
d<- Kline
d

d$log_pop <- log(d$population)
d$contact_high <- ifelse(d$contact == "high", 1, 0)

m10.10 <- map(
  alist(
    total_tools ~ dpois( lambda ),
    log(lambda) <- a + bp * log_pop + 
      bc * contact_high + bpc * contact_high * log_pop,
    a ~ dnorm(0, 100), 
    c(bp, bc, bpc) ~ dnorm(0,1)
  ), 
  data = d)

precis(m10.10, corr = TRUE)
plot(precis(m10.10))

## run some counterfactuals

post <- extract.samples(m10.10)
lambda_high <- exp( post$a + post$bc + (post$bp + post$bpc)*8)
lambda_low <- exp( post$a + post$bp * 8)

diff <- lambda_high - lambda_low
sum(diff > 0)/length(diff)

dens(diff)
abline(v=0)

plot(post$bc, post$bpc)

pairs(post) ## bc-bpc and a-bp; both pairs are highly correlated

## fit other models

## no interaction
m10.11 <- map(
  alist(
    total_tools ~ dpois( lambda ),
    log(lambda) <- a + bp * log_pop + bc * contact_high,
    a ~ dnorm(0, 100), 
    c(bp, bc) ~ dnorm(0,1)
  ), 
  data = d)

## no contact rate
m10.12 <- map(
  alist(
    total_tools ~ dpois( lambda ),
    log(lambda) <- a + bp * log_pop,
    a ~ dnorm(0, 100), 
    bp ~ dnorm(0,1)
  ), 
  data = d)

## no log-pop
m10.13 <- map(
  alist(
    total_tools ~ dpois( lambda ),
    log(lambda) <- a + bc * contact_high,
    a ~ dnorm(0, 100), 
    bc ~ dnorm(0,1)
  ), 
  data = d)

## intercept only

m10.14 <- map(
  alist(
    total_tools ~ dpois( lambda ),
    log(lambda) <- a,
    a ~ dnorm(0, 100)
  ), 
  data = d)

## compare all using WAIC

( islands.compare <- compare(m10.10, m10.11, m10.12, m10.13, m10.14, n=1e4) )
plot(islands.compare)

## plot of raw data
## pch indicates contact rate
pch <- ifelse( d$contact_high == 1, 16, 1 )
plot(d$log_pop , d$total_tools, col = rangi2 , pch = pch , 
     xlab = "log-population" , ylab = "total tools" )

## sequence of log-population sizes to compute over
log_pop.seq <- seq( from = 6 , to = 13 , length.out = 30 )

## compute trend fo rhigh contact islands
d.pred <- data.frame(
  log_pop = log_pop.seq,
  contact_high = 1
)

lambda.pred.h <- ensemble( m10.10, m10.11, m10.12 , data = d.pred )
lambda.med <- apply( lambda.pred.h$link , 2 , median )
lambda.PI <- apply( lambda.pred.h$link , 2 , PI )

## plot predicted trend for high contact islands
lines( log_pop.seq , lambda.med , col = rangi2 )
shade( lambda.PI , log_pop.seq , col = col.alpha(rangi2,0.2) )

## compute trend for low contact islands
d.pred <- data.frame(
  log_pop = log_pop.seq,
  contact_high = 0
)

lambda.pred.l <- ensemble( m10.10, m10.11, m10.12 , data = d.pred )
lambda.med <- apply( lambda.pred.l$link , 2 , median )
lambda.PI <- apply( lambda.pred.l$link , 2 , PI )

## plot again
lines( log_pop.seq , lambda.med , lty = 2 )
shade( lambda.PI , log_pop.seq , col = col.alpha("black", 0.1) )

m10.10stan <- map2stan( m10.10, iter = 3000, warmup = 1000 , chains = 4)
precis(m10.10stan)

## construct centered predictor
d$log_pop_c <- d$log_pop - mean(d$log_pop)

## re-estimate

m10.10stan.c <- map2stan(
  alist(
    total_tools ~ dpois( lambda ),
    log(lambda) <- a + bp * log_pop_c + 
      bc * contact_high + bcp * contact_high * log_pop_c,
    a ~ dnorm(0, 10), 
    bp ~ dnorm(0,1),
    bc ~ dnorm(0,1),
    bcp ~ dnorm(0,1)
  ), 
  data = d, iter = 3000 , warmup = 1000 , chains = 4)
precis(m10.10stan.c)

post <- extract.samples(m10.10stan.c)
pairs(post) ## MUCH better mixing.  

## simulating exposure and offset - p. 321

num_days <- 30
y <- rpois( num_days, 1.5 ) ## simulated books completed

num_weeks <- 4
y_new <- rpois( num_weeks , 0.5*7 )

y_all <- c(y, y_new)
exposure <- c( rep(1, 30) , rep(7, 4) )
monastery <- c(rep(0, 30) , rep(1, 4) )
d <- data.frame(y = y_all, days = exposure , monastery = monastery)

## compute offset
d$log_days <- log( d$days )

## fit the model
m10.15 <- map(
  alist(
    y ~ dpois( lambda ),
    log(lambda) <- log_days + a + b*monastery,
    a ~ dnorm(0, 100),
    b ~ dnorm(0,1)
  ), 
  data = d)

post <- extract.samples( m10.15 )
lambda_old <- exp( post$a )
lambda_new <- exp( post$a + post$b )
precis( data.frame( lambda_old , lambda_new ) )

##############################
## other count models - p. 322
##############################

## simulate career choices among 500 individuals
N <- 500
income <- 1:3
score <- 0.5 * income
p <- softmax(score[1], score[2], score[3])

## now simulate choice
## outcome career holds event type values, not counts
career <- rep(NA, N) ## empty
for ( i in 1:N )
  career[i] <- sample(1:3, size = 1, prob = p)

## fit the model using dcategorical and softmax

m10.16 <- map(
  alist(
    career ~ dcategorical( softmax(0, s2, s3) ),
    s2 <- b*2,
    s3 <- b*3, 
    b ~ dnorm(0,5)
  ), 
  data = list(career = career) )

precis(m10.16)

N <- 100
family_income <- runif(N)
b <- (1:-1)
career <- rep(NA, N)
for( i in 1:N )
{
  score <- 0.5*(1:3) + b*family_income[i]
  p <- softmax(score[1], score[2], score[3])
  career[i] <- sample( 1:3 , size = 1 , prob = p )
}

m10.17 <- map(
  alist(
    career ~ dcategorical( softmax(0, s2, s3) ),
    s2 <- a2 + b2 * family_income,
    s3 <- a3 + b3 * family_income,
    c(a2, a3, b2, b3) ~ dnorm(0,5)
  ), 
  data = list(career = career, family_income = family_income) )

precis(m10.17)

## multinomials as multiple Poissons
data(UCBadmit)
d <- UCBadmit

m_binom <- map(
  alist(
    admit ~ dbinom(applications,p),
    logit(p) <- a,
    a ~ dnorm(0, 100)
  ), 
  data = d)

## poisson model of overall admission rate and rejection rate
d$rej <- d$reject ## 'reject' is a reserved word
m_pois <- map2stan(
  alist(
    admit ~ dpois(lambda1),
    rej ~ dpois(lambda2),
    log(lambda1) <- a1,
    log(lambda2) <- a2,
    c(a1, a2) ~ dnorm(0, 100)
  ), 
  data = d, chains = 3, cores = 3)

logistic(coef(m_binom))

k <- as.numeric(coef(m_pois))

exp(k[1]) / (exp(k[1]) + exp(k[2])) ## same as 4 lines above

## simulate the geometric distribution
N <- 100
x <- runif(N)
y <- rgeom( N , prob = logistic( -1 + 2*x ) )

## estimate
m10.18 <- map(
  alist(
    y ~ dgeom( p ), 
    logit(p) <- a + b*x,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1)
  ), 
  data = list(y = y, x = x) )
precis(m10.18)

