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
fnc_make_ssc_vct <- function(texture){

  if (!all(texture %in% as.data.frame(soiltexture::TT.classes.tbl("DE.BK94.TT"))[,1] )){
    stop(paste0("Error. Can't use texture class \"", texture, "\". Please provide valid soil texture class... See ?fnc_make_ssc"))
  }

  si <-  case_when(texture == "Ss" ~  5,
                     texture == "Su2" ~ 17.5,
                     texture == "Sl2" ~ 17.5,
                     texture == "Sl3" ~ 25,
                     texture == "St2" ~ 5,
                     texture == "Su3" ~ 32.5,
                     texture == "Su4" ~ 45,
                     texture == "Slu" ~ 45,
                     texture == "Sl4" ~ 25,
                     texture == "St3" ~ 7.5,
                     texture == "Ls2" ~ 45,
                     texture == "Ls3" ~ 35,
                     texture == "Ls4" ~ 22.5,
                     texture == "Lt2" ~ 40,
                     texture == "Lts" ~ 22.5,
                     texture == "Ts4" ~ 7.5,
                     texture == "Ts3" ~ 7.5,
                     texture == "Uu" ~  88.2,
                     texture == "Us" ~  65,
                     texture == "Ut2" ~ 77.5,
                     texture == "Ut3" ~ 76  ,
                     texture == "Uls" ~ 57.5,
                     texture == "Ut4" ~ 72.2,
                     texture == "Lu" ~  57.5,
                     texture == "Lt3" ~ 40,
                     texture == "Tu3" ~ 56.3,
                     texture == "Tu4" ~ 68.3,
                     texture == "Ts2" ~ 7.5,
                     texture == "Tl" ~  22.5,
                     texture == "Tu2" ~ 38.6,
                     texture == "Tt" ~  11.7,
                     T ~ NA_real_)

  cl <-  case_when(texture == "Ss" ~  2.5,
                     texture == "Su2" ~  2.5,
                     texture == "Sl2" ~ 6.5,
                     texture == "Sl3" ~  10,
                     texture == "St2" ~ 11.0,
                     texture == "Su3" ~ 4,
                     texture == "Su4" ~ 4,
                     texture == "Slu" ~ 12.5,
                     texture == "Sl4" ~ 14.5,
                     texture == "St3" ~ 21,
                     texture == "Ls2" ~ 21,
                     texture == "Ls3" ~ 21,
                     texture == "Ls4" ~ 21,
                     texture == "Lt2" ~ 30,
                     texture == "Lts" ~ 35,
                     texture == "Ts4" ~ 30,
                     texture == "Ts3" ~ 40,
                     texture == "Uu" ~  3.7,
                     texture == "Us" ~  4,
                     texture == "Ut2" ~ 9.9,
                     texture == "Ut3" ~ 14.4,
                     texture == "Uls" ~ 12.5,
                     texture == "Ut4" ~ 20.6,
                     texture == "Lu" ~  23.5,
                     texture == "Lt3" ~ 40,
                     texture == "Tu3" ~ 36.3,
                     texture == "Tu4" ~ 28.3,
                     texture == "Ts2" ~ 55,
                     texture == "Tl" ~  55,
                     texture == "Tu2" ~ 52.8,
                     texture == "Tt" ~  76.7,
                     T ~ NA_real_ )

  sicl <- cbind(si, cl)
  sasicl <- as.data.frame(cbind(100 - rowSums(sicl), sicl))
  names(sasicl) <- c("sand", "silt", "clay")
  return(sasicl)
}
