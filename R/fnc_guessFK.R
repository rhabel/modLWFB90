#' FK estimate function
#'
#' This simple function guesses field capacity (FK) based on a series of soil moisture values. The function looks for local maxima, takes the values \code{days_passed} after those local maxima and calculates a median of these values.
#'
#'
#' @param vals time series of soil moisture data given as vector
#' @param days_passed there are debates, whether field capacity is reached after 3,4, or 5 days. Can be set here. Default is 3 days.
#' @param perzentil exclude maxima outside of percentile
#' @param with_image whether guessing process shall be visualised. If so, provide a file name with full path.
#'
#' @return Value of volumetric water content (theta) for given pressure head (psi)
#' @export
#'
guess_fk <- function(vals,
                     days_passed = 3,
                     perzentil = 0.95,
                     with_image = NA){

  # local maxima via second derivative
  maxima <- which(diff(sign(diff(vals))) == -2)+1

  # vector with maxima
  testmaxima <- numeric(length = length(vals))
  testmaxima[maxima] <- vals[maxima]
  testmaxima[which(testmaxima == 0)] <- NA
  # subset of maxima to eliminate extreme outliers
  testmaxima[which(testmaxima < quantile(vals, probs = perzentil, na.rm = T))] <- NA

  # values of vals of maxima+dayspassed
  # excluded if maxima+dayspassed is also in maximum vlaues
  final_fks <- (which(!is.na(testmaxima))+days_passed)[!((which(!is.na(testmaxima))+days_passed) %in% which(!is.na(testmaxima)))]
  fk_guess <- vals[final_fks]
  fk_guess.pl <- numeric(length = length(vals))
  fk_guess.pl[which(fk_guess.pl == 0)] <- NA
  fk_guess.pl[final_fks] <- vals[final_fks]

  # mean oder Median
  output <-  median(fk_guess, na.rm = T)

  if(!is.na(with_image)){
    png(with_image ,
        width = 800,height = 600 )
    # Übersicht ob auch die Richtigen Punkte getroffen werden
    plot(vals)
    points(testmaxima, col = "red")
    points(fk_guess.pl, col = "green")
    abline(a= output, b= 0, col = "green")
    #text(output, x = 1, y = output*1.1, adj = 0)
    legend(1, min(vals)*1.3, output,
           xjust = 0)
    dev.off()
  }

  return(output)
}
