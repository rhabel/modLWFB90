#' Filling and reducing the BZE_reg-rasters
#'
#' This function takes the BZE-raster and fills potentially missing data to the current extent of forest in BW. Cells are filled with a focal moving window approach. Furthermore, small bits of unconnected forest below 1 ha are deleted. Those are usually parks and hedges. Filled rasters are stored under the same name in a different folder that can be changed if necessary.
#'
#' @param cells_belt numeric. Number of cells of moving window. The bigger, the coarser the mean value of the filled cells.
#' @param distance_thres numeric. Maximum distance that should be covered by gap-filling-process.
#' @param verbose shall current raster in progress be printed to the console
#' @param which_raster selection of rasters to be processed. Must be character string with ".tif" endings. With default \code{NA}, all rasters in \code{path_models_in} will be processed.
#'
#' @return updates the filled BZE_reg rasters
#' @export

fnc_fill_bze_reg <- function(which_rasters = NA,
                             cells_belt = 6,
                             distance_thres = 400,
                             verbose = T){

  if(is.na(which_rasters)){
    files <- list.files(path_models_in, pattern = "*.tif$")
  }else{
    files <- which_rasters
  }

  wald <- sf::st_read(path_bsk_forest, quiet = T)

  for (k in files) {
    if(verbose){
      print(paste0(Sys.time(), ": working on raster ", k))
    }

    cellsperside <- cells_belt * 2 + 1

    raster <- raster::raster(paste0(path_models_in, k))
    raster_shape_area <- fasterize::fasterize(wald, raster)

    raster_shape_vals <- raster::crop(raster, raster_shape_area)
    raster_shape_vals <- raster::mask(raster_shape_vals,
                                      raster_shape_area)


    raster_shape_na9999 <- raster::crop(raster, raster_shape_area)
    raster::values(raster_shape_na9999)[is.na(raster::values(raster_shape_na9999))] <- -9999
    raster_shape_na9999 <- raster::mask(raster_shape_na9999,
                                        raster_shape_area)

    raster_shape_vals_update <- raster_shape_vals
    raster_shape_na9999_update <- raster_shape_na9999
    ids_no_data <- c(NA)
    ids_no_data_new <- NULL
    i <- 1
    distance_filled <- i * cells_belt * 25

    while ((length(ids_no_data) != length(ids_no_data_new)) &
           length(ids_no_data) > 0 &
           distance_thres >= distance_filled) {
      i <- i + 1
      raster_mean <- raster::focal(raster_shape_vals_update,
                                   w = matrix(data = rep(1, cellsperside^2),
                                              nrow = cellsperside, ncol = cellsperside),
                                   na.rm = T, fun = mean, pad = T)

      ids_no_data <- which(raster::values(raster_shape_na9999_update) == -9999)

      raster::values(raster_shape_vals_update)[ids_no_data] <- raster::values(raster_mean)[ids_no_data]

      raster_shape_na9999_update <- raster_shape_vals_update
      raster::values(raster_shape_na9999_update)[is.na(raster::values(raster_shape_na9999_update))] <- -9999
      raster_shape_na9999_update <- raster::mask(raster_shape_na9999_update,
                                                 raster_shape_area)
      ids_no_data_new <- which(raster::values(raster_shape_na9999_update) == -9999)
      distance_filled <- i * cells_belt * 25
    }

    writeRaster(raster_shape_vals_update,
                paste0(path_BZEreg, k),
                overwrite = T)

    rm(list = c( "raster", "raster_mean", "raster_shape_area",
                 "raster_shape_vals", "raster_shape_vals_update",
                 "raster_shape_na9999", "raster_shape_na9999_update"))
    gc()
  }

}



