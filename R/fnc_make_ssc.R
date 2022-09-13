#' Sand-Silt-Clay from texture class
#'
#' This simple  creates a grain size distribution as a percentage vector from the texture class as found in Bodenkundliche Kartieranleitung (KA5), p142. The distribution is derived from the centre of the respective polygon in the soil texture triangle.
#'
#'
#' @param texture must be one of \code{as.data.frame(soiltexture::TT.classes.tbl("DE.BK94.TT"))[,1]}
#'
#' @return Percentage value of Sand-Silt-Clay in that order.
#'
#' @examples
#' make_ssc("Lt3")
#' @references Ad-Hoc-Arbeitsgruppe Boden (2005). Bodenkundliche Kartieranleitung. 5. Auflage, Hannover.
#'
#' @export
#'
fnc_make_ssc <- function(texture){

  if (!texture %in% as.data.frame(soiltexture::TT.classes.tbl("DE.BK94.TT"))[,1] ){
    stop("Error. Please provide valid soil texture class... See ?make_ssc")
  }

  sicl <-  case_when(texture == "Ss" ~  c(5,    2.5),
                     texture == "Su2" ~ c(17.5, 2.5),
                     texture == "Sl2" ~ c(17.5, 6.5),
                     texture == "Sl3" ~ c(25,    10),
                     texture == "St2" ~ c(5,   11.5),
                     texture == "Su3" ~ c(32.5,   4),
                     texture == "Su4" ~ c(45,     4),
                     texture == "Slu" ~ c(45,  12.5),
                     texture == "Sl4" ~ c(25,  14.5),
                     texture == "St3" ~ c(7.5,   21),
                     texture == "Ls2" ~ c(45,    21),
                     texture == "Ls3" ~ c(35,    21),
                     texture == "Ls4" ~ c(22.5,  21),
                     texture == "Lt2" ~ c(40,    30),
                     texture == "Lts" ~ c(22.5,  35),
                     texture == "Ts4" ~ c(7.5,   30),
                     texture == "Ts3" ~ c(7.5,   40),
                     texture == "Uu" ~  c(90,     4),
                     texture == "Us" ~  c(65,     4),
                     texture == "Ut2" ~ c(78.5,  10),
                     texture == "Ut3" ~ c(76.5,14.5),
                     texture == "Uls" ~ c(57.5,12.5),
                     texture == "Ut4" ~ c(74,    21),
                     texture == "Lu" ~  c(57.5,23.5),
                     texture == "Lt3" ~ c(40,    40),
                     texture == "Tu3" ~ c(57.5,37.5),
                     texture == "Tu4" ~ c(70,    30),
                     texture == "Ts2" ~ c(7.5,   55),
                     texture == "Tl" ~  c(22.5,  55),
                     texture == "Tu2" ~ c(42.5,  55),
                     texture == "Tt" ~  c(17.5,82.5),
                     T ~ c(NA_real_, NA_real_)
  )
  sasicl <- c(100-sum(sicl), sicl)
  names(sasicl) <- c("sand", "silt", "clay")
  return(sasicl)
}
