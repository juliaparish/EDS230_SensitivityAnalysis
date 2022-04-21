#' Atmospheric Conductance 
#'
#' Calculate atmospheric conductance (how easily vapor diffuses from vegetation surfaces)
#'
#' @param vm windspeed (units = centimeters/second)
#' @param zm height at which windspeed is measured, typically this value is the height of the vegetation + 200 (units = centimeters) (default = h + 200)
#' @param h vegetation height (units = centimeters)
#' @param k constant 
#' @param kd constant
#'
#' @return
#' @export
#'
#' @examples
atmcon <- function(vm, h, zm = (h + 200), k = 0.1, kd = 0.7){
  
  # how can we include the standard deviation for windpseed and the error for h? 
  
  zd = kd*h
  
  z = k*h
  
  ac = vm / (6.25*(ln((zm - zd)/z))**2)
  
  ac_rounded <- round(ac, 2)
  
  return(paste0("The atmospheric conductance for this vegetation is ", ac_rounded, " {units}."))
  
}