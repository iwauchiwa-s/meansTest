#' @title Significance test for difference between 2 mean values
#' @description \code{meansTest} statistical test for mean values
#'
#' @importFrom stats pt
#' @importFrom stats qt
#' @param av1 mean value of group 1
#' @param sd1 standard deviation of group 1
#' @param nd1 number of group 1
#' @param av2 mean value of group 2
#' @param sd2 standard deviation of group 2
#' @param nd2 number of group 2
#' @return dav stats results
#' @return cohen_d stats results
#' @return t stats results
#' @return pv stats results
#' @return cl_l stats results
#' @return cl_u stats results
#' @return txj stats results
#' @export
#' @examples
#' # meansTest(60, 10, 20, 55, 15, 25)


meansTest <- function(av1, sd1, nd1, av2, sd2, nd2){
  dav <- abs(av1-av2)
  var1 <- sd1^2
  var2 <- sd2^2
  sdpool <- sqrt ( ( nd1*var1 + nd2*var2) / (nd1+nd2) )
  cohen_d <- dav/sdpool
  dof <- round((var1/nd1+var2/nd2)^2/(var1^2/nd1^2/(nd1-1)+var2^2/nd2^2/(nd2-1)))
  t <- (abs(av1-av2))/sqrt(var1/nd1+var2/nd2)
  pv <- pt(-t,df=dof)*2
  vpool <- ((nd1-1)*var1+(nd2-1)*var2)/(nd1+nd2-2)
  cl_l <- (av1-av2)-qt(0.975,nd1+nd2-2)*sqrt(vpool*(1/nd1+1/nd2))
  cl_u <- (av1-av2)+qt(0.975,nd1+nd2-2)*sqrt(vpool*(1/nd1+1/nd2))
  if (pv <= 0.05){
    txj <- 1 # significant
  }
  else{
    txj <- 0 # insignificant
  }
  return(list(dav, cohen_d, t, pv, cl_l, cl_u, txj))
}

