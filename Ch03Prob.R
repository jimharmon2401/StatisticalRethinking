## Ch. 3 problems

rm(list=ls())

#########################################
## EASY
#########################################


p_grid<-seq( from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

library(rethinking)

dens(samples)

## 3E1
sum(samples < 0.2) / 1e4 ## 5e-04

## 3E2
sum(samples > 0.8) / 1e4 ## 0.1117

## 3E3
1 - 5e-04 - 0.1117 ## 0.8878

## 3E4
quantile(samples, 0.2) ## 0.519

## 3E5
quantile(samples, 0.8) ## 0.7567

## 3E6
HPDI(samples, prob = 0.66) ## 0.520, 0.784

## 3E7
PI(samples, prob = 0.66) ## 0.500500, 0.768

#########################################
## MEDIUM
#########################################

rm(list = ls())

## 3M1
p_grid<-seq( from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(posterior)

## 3M2
set.seed(343)
samples<-sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

dens(samples)

HPDI(samples, prob = 0.9) ## 0.350, 0.735

## 3M3
dummy_w <- rbinom(1e4, size = 15, prob = samples)

simplehist(dummy_w) ## 8 is indeed the most likely value

## 3M4
dummy_w9 <- rbinom(1e4, size = 9, prob = samples)

sum(dummy_w9 == 6) / 1e4 ## 0.1771

simplehist(dummy_w9)

## 3M5
rm(list = ls())

p_grid<-seq( from = 0, to = 1, length.out = 1000)
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(posterior)

set.seed(343)
samples<-sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

dens(samples)

HPDI(samples, prob = 0.9) ## 0.500500, 0.712

dummy_w <- rbinom(1e4, size = 15, prob = samples)

simplehist(dummy_w) ## 9 is now the most likely value

dummy_w9 <- rbinom(1e4, size = 9, prob = samples)

sum(dummy_w9 == 6) / 1e4 ## 0.2312

simplehist(dummy_w9) ## 6 most likely value

#########################################
## HARD
#########################################

rm(list = ls())

library(rethinking)

data(homeworkch3)

sum(birth1) + sum(birth2) ## 111 - check!!

## 1 = male, 0 = female
## birth1 = first born; birth2 = second born
## 100 2-child families

## 3H1

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(111, size = 200, prob = p_grid)
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)

p_grid[which.max(posterior)] ## 0.554 - matches MLE

## 3H2

samples <- sample(p_grid, prob = posterior, size = 1e5, replace = TRUE)

dens(samples)

HPDI(samples, prob = 0.5) ## 0.525, 0.572
HPDI(samples, prob = 0.89) ## 0.496, 0.607
HPDI(samples, prob = 0.97) ## 0.477, 0.627

## 3H3
set.seed(343)
dummy_w <- rbinom(1e4, size = 200, prob = samples)

dens(dummy_w)
abline(v=111) ## looks reasonable

## 3H4
set.seed(343)
dummy_w1 <- rbinom(1e4, size = 100, prob = samples)

dens(dummy_w1)
abline(v = sum(birth1)) ## not so good!, on the low end of the modal values

## 3H5
sum(1 - birth1) ## 49 girls

sum(birth2[which(0 == birth1)]) ## 39 boys following girls

set.seed(343)
dummy_w1 <- rbinom(1e4, size = 49, prob = samples)

simplehist(dummy_w1)
abline(v=39) ## not al ALL likely
## guess that model is not accounting for correlation between first and
## second born children.  