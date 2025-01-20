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
  vec.dir.diff <- diff(sign(diff(vals)))
  maxima <- which(vec.dir.diff == -2)+1

  # vector with maxima
  testmaxima <- numeric(length = length(vals))
  testmaxima[maxima] <- vals[maxima]
  testmaxima[which(testmaxima == 0)] <- NA
  # percentile-subset of maxima to eliminate local maxima far from saturation:
  # percentile-subset of maxima to eliminate local maxima far from saturation:
  testmaxima[which(testmaxima < quantile(vals, probs = perzentil, na.rm = T))] <- NA

  # eliminate maxima without enough time for FC to be reached until next maxima arrives:
  testmaxima_pos <- which(testmaxima >= quantile(vals, probs = perzentil, na.rm = T))
  testmaxima[testmaxima_pos[which(diff(testmaxima_pos) < (days_passed+1))]] <- NA

  # eliminate maxima, where derivative is not consistently negative for "days_passed" days after peak

  # first: find position of testmaxima after deleting
  testmaxima_pos <- which(testmaxima >= quantile(vals, probs = perzentil, na.rm = T))

  # Test: is data vector long enough for the last peak to be followed be days_passed days?
  if(testmaxima_pos[length(testmaxima_pos)] + days_passed > length(vec.dir.diff)){
    # delete peak
    testmaxima[testmaxima_pos[length(testmaxima_pos)]] <- NA
    # delete position
    testmaxima_pos <- testmaxima_pos[-length(testmaxima_pos)]
  }

  # sum(second derivative) after -2 must be zero (or: must not change direction again):
  for(i in testmaxima_pos){
    if(sum(abs((vec.dir.diff[i:(i+days_passed)]))) != 0){
      testmaxima[i] <- NA
    }
  }

  # values of vals of maxima+dayspassed
  final_fks <- (which(!is.na(testmaxima))+days_passed)

  fk_guess <- vals[final_fks]
  fk_guess.pl <- numeric(length = length(vals))
  fk_guess.pl[which(fk_guess.pl == 0)] <- NA
  fk_guess.pl[final_fks] <- vals[final_fks]

  # median for a more robust guess
  output <-  round(median(fk_guess, na.rm = T), 3)

  if(is.na(output)){
    message("no FK values found")
    return(NA)
  }else{
    if(!is.na(with_image)){

      png(with_image ,
          width = 900, height = 500 )

      layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(5,2))
      # Übersicht ob auch die Richtigen Punkte getroffen werden
      plot(vals)
      points(testmaxima, col = "red")
      points(fk_guess.pl, col = "green")
      abline(a= output, b= 0, col = "green")
      #text(output, x = 1, y = output*1.1, adj = 0)
      legend(1, min(vals)*1.3, output,
             xjust = 0)

      hist(fk_guess)
      abline(v = output, col = "green")

      dev.off()


    }

    return(output)
  }

}
