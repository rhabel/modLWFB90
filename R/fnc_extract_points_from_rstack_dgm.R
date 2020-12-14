#' Extract slope and aspect from dgm
#'
#' @param lay rasterstack
#' @param xy Spaitalpointsdataframe containing points in GK3
#' @param buffering should most common value in \code{buff_width} buffer distance be used if \code{NA}
#' @param buff_width buffer width
#'
#' @return Set of soil information that is further processed in function fnc_soil_bze()
#'
fnc_extract_points_dgm <- function(lay,
                               xy,
                               buffering = F,
                               buff_width = 50) {
  val <- raster::extract(lay, xy, method = "simple") # normaler extract
  #NAs & -9999: ziehe die häufigsten Werte im Umkreis von 50 m
  if(any(apply(val, 1, function(x){any(is.na(x) | x < 0 )}))) {
    if(buffering == T ){
      val_near <- raster::extract(lay, xy[which(apply(val, 1, function(x){any(is.na(x) | x < 0 )}) ==T),], method = "simple", buff = buff_width,
                                  fun = function(x) {as.integer(names(sort(-table(x[which(!is.na(x) & x >= 0)])))[1])}
      )
      val[which(apply(val, 1, function(x){any(is.na(x) | x < 0 )}) ==T),] <- val_near
    }else{
      val[which(apply(val, 1, function(x){any(is.na(x) | x < 0 )}) ==T),] <- NA
    }

  }
  return(val)
}
