#' Extract BZE information from raster stack
#'
#' A function written by Paul Schmidt-Walter that extracts soil data from a rasterstack of regionalised BZE-Data. If data is missing, it takes most common value within a \code{buff_width} buffer.
#'
#'
#' @param lay rasterstack
#' @param xy Spaitalpointsdataframe containing points in GK3
#' @param buff_width buffer width
#'
#' @return Set of soil information that is further processed in function fnc_soil_bze()
#'
fnc_extract_points <- function(lay, xy, buff_width = 50) {
  val <- raster::extract(lay, xy, method = "simple") # normaler extract
  #NAs & -9999: ziehe die häufigsten Werte im Umkreis von 50 m
  if(sum(is.na(val) | val < 0)) {
    val_near <- raster::extract(lay, xy[which(is.na(val) | val < 0),], method = "simple", buff = buff_width,
                        fun = function(x) {as.integer(names(sort(-table(x[which(!is.na(x) & x >= 0)])))[1])}
    )
    val[is.na(val)] <- val_near
  }
  return(val)
}
