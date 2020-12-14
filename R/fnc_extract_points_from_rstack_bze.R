#' Extract BZE information from raster stack
#'
#' A function written by Paul Schmidt-Walter that extracts soil data from a rasterstack of regionalised BZE-Data. If data is missing, it takes most common value within a \code{buff_width} buffer.
#'
#'
#' @param lay rasterstack
#' @param xy Spaitalpointsdataframe containing points in UTM25832
#' @param buffering should most common value in \code{buff_width} buffer distance be used if \code{NA}
#' @param buff_width buffer width
#'
#' @return Set of soil information that is further processed in function fnc_soil_bze()
#' @import rgeos
#'
fnc_extract_points_bze <- function(lay,
                               xy,
                               meta.out,
                               buffering = F,
                               buff_width = 50) {


  val <- raster::extract(lay, xy, method = "simple") # normaler extract

  val2 <- as.data.frame(val)
  val_miss <- val2[!complete.cases(val2),]

  #NAs & -9999: ziehe die häufigsten Werte im Umkreis von 50 m
  if(nrow(val_miss) > 0 ){
    which_missing <- which(!complete.cases(val2))

    if(buffering == T ){

      cl <- makeCluster(parallel::detectCores())  #Cluster mit verfügbarer Anzahl von Kernen starten
      registerDoParallel(cl)
      val_miss <- foreach(i = 1:nrow(val_miss),
                          .combine = rbind,
                          .packages = c("sf", "raster", "rgeos", "sp")) %dopar% {

                            sf_test <- sf::st_as_sf(xy[which_missing[i],])

                            ex.df <- as.data.frame(raster::extract(lay,
                                                                   xy[which_missing[i],],
                                                                   method = "simple",
                                                                   buffer = buff_width,
                                                                   small = F,
                                                                   cellnumbers = T))

                            ex.df <- cbind(ex.df, xyFromCell(lay, ex.df[,1]))
                            ex.df <- ex.df[complete.cases(ex.df),]

                            if(nrow(ex.df) != 0){

                              ex.df.dist <- ex.df[c("x", "y")]
                              sp::coordinates(ex.df.dist) <- c("x", "y")
                              UTM25832 <- sp::CRS("+init=EPSG:25832")
                              sp::proj4string(ex.df.dist) <- UTM25832

                              ex.df <- cbind(ex.df,
                                             rgeos::gDistance(as(sf_test, "Spatial"), ex.df.dist, byid=TRUE))
                              colnames(ex.df)[ncol(ex.df)] <- "dist"
                              ex.df <- ex.df[order(ex.df$dist), ]
                              ex.df <- ex.df[complete.cases(ex.df), ]

                              val_miss_out <- ex.df[1, ]

                            } else{
                              val_miss_out <- rep(NA, ncol(ex.df))
                            }
                          }

      stopCluster(cl)

      if(is.na(meta.out) == F){

        val_miss$num <- which_missing

        meta <- xy@data[c("ID_custom", "easting", "northing" )] %>%
          dplyr::mutate(num = 1:nrow( xy@data),
                        buffered = ifelse(num %in% which_missing, T, F)) %>%
          dplyr::left_join(val_miss[c("x", "y", "dist", "num")], by = "num") %>%
          dplyr::mutate(buffer_success = dplyr::case_when(buffered == F ~ NA,
                                                          buffered == T & is.na(dist) == F ~ T,
                                                          T ~ F)) %>%
          dplyr::select(ID_custom, easting, northing, buffered, buffer_success, x, y, dist) %>%
          setNames(c("ID_custom", "easting", "northing", "buffered", "buffer_success", "x_buffcell","y_buffcell", "distance"))

        write_csv(meta, file = meta.out)
      }


      val2[which_missing, ] <- val_miss[which(colnames(val_miss) == "bodtief"):which(colnames(val_miss) == "u4")]
      val2 <- as.data.frame(cbind(xy@data[c("aspect", "slope")],
                                  val2))

    }else{
      val2 <- as.data.frame(cbind(xy@data[c("aspect", "slope")],
                                  val2))
    }



  }

  return(val2)
}
