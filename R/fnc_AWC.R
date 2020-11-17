#'Calculation of the plant available water content
#'
#'
#'This function calculates the plant available water content (AWC, nutzbare Feldkapazitaet) from the Mualem-van Genuchten retention curve unsing \code{\link{fnc_MvG.swc}}. The function calculates the AWC per soil layer or for the whole depth at once. The gravel content is already taken into account.
#'
#' @param data is the list of soil properties calculated by \code{\link{fnc_get_soil}}
#' @param nFK indicates whether the AWC is calculated for the whole soil depth (\code{nFK = T}), returning a vector with one vector element for each list element. \code{nFK = F} returns the initial list with the additional columns: \code{fc} (vol. water content at 63 hPa), \code{pwp} (vol. water content at 15000 hPa) and \code{AWC} (AWC in mm per m^2 and depth of the soil layer)
#'
#' @return Returns either the expanded input list of soil properties or a vector containing the AWC for the whole soil depth with one vector element per input list element

fnc_AWC <- function(data, nFK = F) {
  if(nFK == T){
    for (i in c(1 : length(data))) {
      data[[i]]$fc <- fnc_MvG.swc(psi = 63,
                                  alpha = data[[i]]$alpha/100,
                                  ThS = data[[i]]$ths,
                                  ThR = data[[i]]$thr,
                                  n = data[[i]]$npar)

      data[[i]]$pwp <- fnc_MvG.swc(psi = 15000,
                                   alpha = data[[i]]$alpha/100,
                                   ThS = data[[i]]$ths,
                                   ThR = data[[i]]$thr,
                                   n = data[[i]]$npar)

      data[[i]]$AWC <- (((data[[i]]$upper-data[[i]]$lower)*(data[[i]]$fc-data[[i]]$pwp))/0.001)*(1-data[[i]]$gravel)
    }

    nFK <- c()
    for(i in c(1: length(data))){
      sum <- sum(data[[i]]$AWC)
      sum <- if(min(data[[i]]$lower) > -1.0){
                sum(data[[i]]$AWC[data[[i]]$lower >= -1.0]) * min(data[[i]]$lower)*(-1)
             }
             else{
               sum(data[[i]]$AWC[data[[i]]$lower >= -1.0])
             }
      sum <- sum(data[[i]]$AWC[data[[i]]$lower >= -1.0])
      nFK <- append(nFK, sum)
    }
    return(nFK)

  }

  else{

    for (i in c(1 : length(data))) {
      data[[i]]$fc <- fnc_MvG.swc(psi = 63,
                                  alpha = data[[i]]$alpha/100,
                                  ThS = data[[i]]$ths,
                                  ThR = data[[i]]$thr,
                                  n = data[[i]]$npar)

      data[[i]]$pwp <- fnc_MvG.swc(psi = 15000,
                                   alpha = data[[i]]$alpha/100,
                                   ThS = data[[i]]$ths,
                                   ThR = data[[i]]$thr,
                                   n = data[[i]]$npar)

      data[[i]]$AWC <- (((data[[i]]$upper-data[[i]]$lower)*(data[[i]]$fc-data[[i]]$pwp))/0.001)*(1-data[[i]]$gravel)
    }
    return(data)}
}
