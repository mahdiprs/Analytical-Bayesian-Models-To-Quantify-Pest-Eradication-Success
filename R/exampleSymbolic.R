## example file
source("R/funcsSymbolic.R")
library(calculus)
library(ggplot2)
library(data.table)

# initial population size between 0 and 1
rho = seq(0.1,1,.1)
# population growth rate value is bigger than 1
lambda = seq(1,3,.5)
# survey sensitivity is between 0 and 1
delta = seq(0.1,.9,.1)
# number of surveys
time = seq(1,5,1)

## Model 1
caseM1 = CJ(time,delta,rho)
# Estimate mean and variance of population size after observing t negative surveys
caseM1$mean = mapply(MeanPopulationM1, caseM1$time,caseM1$delta,caseM1$rho) # mean
caseM1$var <- mapply(VarPopulationM1, caseM1$time,caseM1$delta,caseM1$rho ) # var

# Inferred probability of absence after observing t negative surveys
caseM1$extinct = mapply(P0M1, caseM1$time,caseM1$delta,caseM1$rho )

# Probability of P(~D0,~D1,...,~Dt)
caseM1$notDCT = mapply(ProbNotDCTUptoTM1, caseM1$time,caseM1$delta,caseM1$rho)

# Probability of  P(+Dt,~Dt-1,~Dt-2,...~D0)
caseM1$firstDCT = mapply(PFirstDCTwithUptMinusOneNotDCTM1, caseM1$time,caseM1$delta,caseM1$rho )

# Probability of P(~Dt,~Dt-1,~Dt-2,...~D0|Xt>0)
caseM1$notDTCPopNonZero = mapply(ProbNotDCTUpToTCONDPopNotZeroM1, caseM1$time,caseM1$delta,caseM1$rho )

caseM1[is.na(caseM1)] <- NA

## Model 2
caseM2 = CJ(time,delta,lambda,rho)
# Estimate mean and variance of population size after observing t negative surveys
caseM2$mean = mapply(MeanPopulationM2, caseM2$time,caseM2$time,caseM2$delta, caseM2$lambda,caseM2$rho ) # mean
caseM2$var <- mapply(VarPopulationM2, caseM2$time,caseM2$time,caseM2$delta, caseM2$lambda,caseM2$rho ) # var

# Inferred probability of absence after observing t negative surveys
caseM2$extinct = mapply(P0M2, caseM2$time,caseM2$time,caseM2$delta,caseM2$lambda,caseM2$rho )

# Probability of P(~D0,~D1,...,~Dt)
caseM2$notDCT = mapply(ProbNotDCTUptoTM2, caseM2$time, caseM2$delta,caseM2$lambda,caseM2$rho )

# Probability of  P(+Dt,~Dt-1,~Dt-2,...~D0)
caseM2$firstDCT = mapply(PFirstDCTwithUptMinusOneNotDCTM2, caseM2$time, caseM2$delta,caseM2$lambda,caseM2$rho )

# Probability of P(~Dt,~Dt-1,~Dt-2,...~D0|Xt>0)
caseM2$notDTCPopNonZero = mapply(ProbNotDCTUpToTCONDPopNotZeroM2, caseM2$time,caseM2$delta,caseM2$lambda,caseM2$rho )

caseM2[is.na(caseM2)] <- NA


## plot distributions
# Distribution for population size after t negative surveys Model 1
# t =5
t <-5
vec <- taylor(pgfM1(t,'delta','rho','s'), var = "s",list(rho= 0.5,delta=.1), order = 10)
vec <- vec$terms
vec

ggplot(vec, aes(factor(degree),coef,group =1))+
  geom_path()+
  geom_point()+
  labs(x= "Population size",y="Probability mass")


# Distribution for population size after t negative surveys Model 2
# t = 1
t <-1
vec <- taylor(pgfM2(t,t,'delta','lambda','rho','s'), var = "s",list(rho= 0.5,delta=.1,lambda=1.2), order = 10)
vec <- vec$terms
vec
ggplot(vec, aes(factor(degree),coef,group =1))+
  geom_path()+
  geom_point()+
  labs(x= "Population size",y="Probability mass")
