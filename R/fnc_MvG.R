#' Mualem-vanGenuchten Function
#'
#' This simple function reproduces the Mualem-vanGenuchten equation without creating another package dependency.
#'
#'
#' @param psi pressure head in hPa
#' @param alpha MvG alpha in 1/cm
#' @param n MvG n, dimensionless
#' @param ThS MvG theta_s
#' @param ThR MvG theta_r
#' @return Value of volumetric water content (theta) for given pressure head (psi)
#' @export
#'
#' @example inst/examples/fnc_MvG_swc_ex.R
#'
fnc_MvG.swc <- function(psi,
                        alpha,
                        n,
                        ThS,
                        ThR,
                        m = 1-1/n){
  wetness <- 1/((1 + (alpha * psi)^n))^(m)
  theta <- wetness * (ThS-ThR) +ThR
  return(theta)
}
