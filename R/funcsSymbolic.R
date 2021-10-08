# function for the theory paper
library(calculus)

# pgf for initial population size
pgfInitial<- function(rho,s){
  val <- paste("exp(",rho,"*","(",s, "-", "1",")",")")
  return(val)
}

#  pgf for detection probability
pgfDCT <- function(delta,s){
  val <- paste("(","1","-",delta,"+",delta,"*",s,")")
  return(val)
}

##############################################################
# Model 1
##############################################################

# pgf for Model 1
pgfM1 <- function(t,delta,rho,s){
  val <- paste0("exp(",rho,"*", "(","1", "-", delta,")","^", "(",t, "+ 1",")", "*", "(",s, "- 1",")",")")
  return(val)
}

# PGF for Model 1 population size conditional on negative survey up to t-1 and first positive detection on t
pgfM1firstDTC <- function(t,delta,rho,s){
  t2 <- pgfM1(t-1, "delta", "rho", "s")
  t5 <- pgfM1(t-1, "delta", "rho", "s * (1 - delta)")
  return(paste0("(",t2, "-", t5,")", "/", "(","1", "-", t5,")"))
  #return(paste0(t2))
}

# Mean population size after t negative surveys Model 1
MeanPopulationM1 <- function(t,delta,rho){
  derv <- derivative(f = pgfM1(t,'delta','rho', 's'), var = "s")
  val = eval(parse(text = derv),list(delta=delta,rho=rho,s=1))
  return(val)
}

# Variance population size Model 1
VarPopulationM1 <- function(t,delta,rho){
  derv1 <- derivative(f = pgfM1(t,'delta','rho', 's'), var = "s")
  derv <- derivative(f = derv1, var = "s")
  val = eval(parse(text = derv),list(delta=delta,rho=rho,s=1))+MeanPopulationM1(t,delta,rho)-MeanPopulationM1(t,delta,rho)^2
  return(val)
}

# Probability of no detection Model 1
P0M1 <- function(t,delta,rho) {
  val = eval(parse(text=pgfM1(t, delta, rho, 0)))
  return(val)
}

# PGF number of detection for Model 1
pgfNbrDCTM1 <- function(t,delta,rho,s){
  t2 <- pgfDCT(delta, s)
  return(pgfM1(t - 1, delta, rho, t2))
}

# p(~Dt|~Dt-1,~Dt-2,...,~D0) for Model 1
PNotDCTCondUptMinusOneM1 <- function(t,delta,rho) {
  val = eval(parse(text = pgfNbrDCTM1(t, delta, rho, 0)))
  return(val)
}

# Probability of P(~D0,~D1,...,~Dt) = p(~Dt|~Dt-1,~Dt-2,...~D0) * p(~Dt-1,~Dt-2,...~D0)
ProbNotDCTUptoTM1 <- function(t,delta,rho){
  if (t == 0){
    return(eval(parse(text = pgfInitial(rho,paste("1","-",delta))))) # exp(-delta * rho)
    }else{
  return(eval(parse(text = PNotDCTCondUptMinusOneM1(t, delta, rho) * ProbNotDCTUptoTM1(t - 1, delta, rho))))
      }
}

# Probability of  P(+Dt|~Dt-1,~Dt-2,...~D0)
PFirstDCTCondUptMinusOneNotDCTM1 <- function(t,delta,rho){
  t1 <- eval(parse(text = PNotDCTCondUptMinusOneM1(t, delta, rho)))
  return(1 - t1)
}

# Probability of  P(+Dt,~Dt-1,~Dt-2,...~D0)
PFirstDCTwithUptMinusOneNotDCTM1 <- function(t,delta,rho){
  if (t == 0){
    return(eval(parse(text = 1 - pgfInitial(rho,paste("1","-",delta))))) # 1- exp(-delta * rho)
  }else{
    return(eval(parse(text = PFirstDCTCondUptMinusOneNotDCTM1(t, delta, rho) * ProbNotDCTUptoTM1(t - 1, delta, rho))))
  }
}


# Probability not detecting up to and including time t conditioning on presence p(~Dt,~Dt-1,~Dt-2,...~D0|Xt>0)
ProbNotDCTUpToTCONDPopNotZeroM1 <- function(t,delta,rho){
  t1 <- ProbNotDCTUptoTM1(t, delta, rho)
  t2 <- P0M1(t, delta, rho)
  t5 <- eval(parse(text = pgfInitial(rho,0))) #exp(-rho)
  return(t1 * (1 - t2) / (1 - t5))
}

#######################################################
#   Model 2
######################################################

# define the population growth phix(s)
phi_x <- function(lambda,s){
  val <- paste("exp(",lambda,"*","(",s,"-1",")",")")#exp(lambda * (s - 1))
  return(val)
}

# pgf population growth
pgfindgrowth <- function(t,lambda,s){
  c1 <- t - 1
  if (t == 0)
    return(s)
  else if (c1 == 0)
    return(phi_x(lambda, s))
  else
    return(phi_x(lambda, pgfindgrowth(t - 1, lambda, s)))
}

# PGF for population size without including zero sighting
pgfPopM2 <- function(t,lambda,rho,s){
  t1 <- pgfindgrowth(t, lambda, s)
  return(phi_x(rho, t1))
}

P0PopM2 <- function(t,lambda,rho) {
  val = eval(parse(text=pgfPopM2(t,lambda,rho, 0)))
  return(val)
}

# PGF for Model 2
pgfM2 <- function(t1,t2,delta,lambda,rho,s){
  c1 <- t1 - t2
  c2 <- t1 - 1 - t2
  if (!(c1 != 0 || t2 != 0 || t1 != 0)){
    return(paste(pgfInitial(rho, paste("(",s, "*", pgfDCT(delta, "0"),")")), "/", "(",pgfInitial(rho, paste("(",pgfDCT(delta, "0"),")")),")"))
  }else if (c2 == 0){
    return(pgfM2(t1 - 1, t2, delta, lambda, rho, phi_x(lambda, s)))
  }else{
    return(paste(pgfM2(t1, t2 - 1, delta, lambda, rho, paste("(",s, '*', pgfDCT(delta, "0"),")")), "/","(", pgfM2(t1, t2 - 1, delta, lambda, rho, pgfDCT(delta, "0")),")"))
  }
}

# Mean population size Model 2
MeanPopulationM2 <- function(t1,t2,delta,lambda,rho){
  derv <- derivative(f = pgfM2(t1,t2,'delta','lambda','rho', 's'), var = "s")
  val = eval(parse(text = derv),list(delta=delta,rho=rho,s=1))
  return(val)
}

# Variance population size Model 2
VarPopulationM2 <- function(t1,t2,delta,lambda,rho){
  derv1 <- derivative(f = pgfM2(t1,t2,'delta','lambda','rho', 's'), var = "s")
  derv <- derivative(f = derv1, var = "s")
  val = eval(parse(text = derv),list(delta=delta,rho=rho,s=1))+MeanPopulationM2(t1,t2,delta,lambda,rho)-MeanPopulationM2(t1,t2,delta,lambda,rho)^2
  return(val)
}

# Probability of no detection Model 2
P0M2 <- function(t1,t2,delta,lambda,rho) {
  val = eval(parse(text = pgfM2(t1,t2,delta,lambda,rho, 0)))
  return(val)
}

# pgf population size conditional on first detection after t-1 negative surveys
pgfPopSzConFrsDCTM2 <- function(t,delta,lambda,rho,s){
  if (t == 0){
    return(paste("(",pgfInitial(rho, s), "-", pgfInitial(rho, paste("(",s, "*", pgfDCT(delta, "0"),")")),")",
                 "/", "(","1", "-", pgfInitial(rho, pgfDCT(delta, "0")),")"))
  }else{
    return(paste("(",pgfM2(t, t - 1, delta, lambda, rho, s), "-", pgfM2(t, t - 1, delta, lambda, rho, paste("(",s, "*", pgfDCT(delta, "0"),")")),")", "/",
                 "(","1", "-", pgfM2(t, t - 1, delta, lambda, rho, pgfDCT(delta, "0")),")"))
  }
}

# pgf for the number of detection M2
pgfNbrDCTM2 <- function(t,delta,lambda,rho,s){
  if (t == 0)
    return(pgfInitial(rho, pgfDCT(delta, s)))
  else
    return(pgfM2(t, t - 1, delta, lambda, rho, pgfDCT(delta, s)))
}

# Probability of p(~Dt|~Dt-1,~Dt-2,...~D0)
PNotDCTCondUptMinusOneM2 <- function(t,delta,lambda,rho) {
  return(eval(parse(text = pgfNbrDCTM2(t, delta, lambda, rho, 0))))
}

#  Probability of P(~D0,~D1,...,~Dt) = p(~Dt|~Dt-1,~Dt-2,...~D0) * p(~Dt-1,~Dt-2,...~D0)
ProbNotDCTUptoTM2 <- function(t,delta,lambda,rho){
  if (t == 0)
    return(eval(parse(text = pgfInitial(rho, pgfDCT(delta, 0)))))
  else
    return(PNotDCTCondUptMinusOneM2(t, delta, lambda, rho) * ProbNotDCTUptoTM2(t - 1, delta, lambda, rho))
}

# Probability of  P(+Dt|~Dt-1,~Dt-2,...~D0)
PFirstDCTCondUptMinusOneNotDCTM2 <- function(t,delta,lambda,rho){
  t1 <- PNotDCTCondUptMinusOneM2(t, delta, lambda, rho)
  return(1 - t1)
}

# Probability of  P(+Dt,~Dt-1,~Dt-2,...~D0)
PFirstDCTwithUptMinusOneNotDCTM2 <- function(t,delta,lambda,rho){
  if (t == 0)
    return(1 - eval(parse(text = pgfInitial(rho, pgfDCT(delta, 0)))))
  else
    return(PFirstDCTCondUptMinusOneNotDCTM2(t, delta, lambda, rho) * ProbNotDCTUptoTM2(t - 1, delta, lambda, rho))

}

# Probability not detecting up to and including time t conditioning on presence P(~Dt,~Dt-1,~Dt-2,...~D0|Xt>0)
ProbNotDCTUpToTCONDPopNotZeroM2 <- function(t,delta,lambda,rho){
  t1 <- ProbNotDCTUptoTM2(t, delta, lambda, rho)
  t2 <- P0M2(t, t, delta, lambda, rho)
  t5 <- P0PopM2(t, lambda, rho)
  return(t1 * (1 - t2) / (1 - t5))
}
