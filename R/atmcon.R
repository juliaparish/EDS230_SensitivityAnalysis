#' Atmospheric Conductance calculation
#'
#' Calculate atmospheric conductance (how easily vapor diffuses from vegetation surfaces) given the windspeed, height of the windspeed measurement, height of the vegetation, and constants. 
#'
#' @param vm windspeed (units = centimeters/second)
#' @param zm height at which windspeed is measured, typically this value is the height of the vegetation + 200 (units = centimeters) (default = h + 200, 200 is added because 200 centimeters above vegetation is the typical height at which windspeed is measured)
#' @param h vegetation height (units = centimeters)
#' @param k constant (default = 0.1)
#' @param kd constant (default = 0.7)
#'
#' @return
#' @export
#'
#' @examples
atmcon <- function(vm, h, zm = (h + 200), k = 0.1, kd = 0.7){
  
  # define the variable zd to use in the atmospheric conductance equation, given `kd` and `h` 
  zd = kd*h
  
  # define the variable z to use in the atmospheric conductance equation, given `k` and `h`
  z = k*h
  
  # atmospheric conductance equation
  ac = vm / (6.25*(ln((zm - zd)/z))**2)
  
  return(list(ac = ac))
  # names the output set equal to itself so you can call the output with LHS
  
}



