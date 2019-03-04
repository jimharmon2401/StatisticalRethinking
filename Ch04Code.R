## Ch 4 code

pos <- replicate(1000, sum(runif(16, -1, 1)))

hist(pos)

plot(density(pos)) ## close to normal

library(rethinking)
growth <- replicate( 10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp=TRUE)

big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))
small <- replicate(10000, prod(1 + runif(12, 0, 0.01)))

dens(big, norm.comp = TRUE) ## not so good
dens(small, norm.comp = TRUE) ## much better!

log.big <- replicate(10000, log(prod(1+runif(12,0,0.5))))

dens(log.big, norm.comp = TRUE) ## yes!!

#############################################

library(rethinking) ## done

data(Howell1)

d <- Howell1

str(d) ## structure of d

d2 <- d[ d$age >= 18 , ] ## only keep adults

dens(d2$height) ## not so normal! er, kinda

dens(d2$height[d2$male == 1])
dens(d2$height[d2$male == 0]) ## kinda looks like mixture???  :)

