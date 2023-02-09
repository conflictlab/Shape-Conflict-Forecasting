tadda <- function(pred, obs, epsilon){
  this.sign <- rep(0, length(pred))
  this.sign[sign(pred) != sign(obs)] <- 1
  epsCondition <- rep(0, length(pred))
  epsCondition[abs(pred-obs) > epsilon] <- 1
  return(mean(abs(pred - obs) + abs(pred)*this.sign*epsCondition, na.rm=T))
}
