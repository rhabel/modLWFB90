#' Little MvG helper function
#'
#' ...
#'
#' @param SWATi swati from swatday.asc
#' @param soil soil.df
#'
#' @return Returns the desired output to the database directly.
#'
#'
#' @import data.table
#' @export


MvG.swc <- function(
  psi, #pressure head in hPa
  alpha, #MvG alpha
  n, # MvG n
  ThS,
  ThR,
  m = 1-1/n)
{
  wetness <- 1/((1 + (alpha * psi)^n))^(m)
  theta <- wetness * (ThS-ThR) + ThR
  return(theta)
}
