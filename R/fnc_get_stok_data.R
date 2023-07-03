#' Helper function for selecting STOK_IDs for pre-processed STOK soil data.
#'
#' Other than with regionalised soil data, there is a limited amount of polygons in the Standortskartierung (~500.000). To speed things up for individual applications, all those have been calculated with all three available PTFs. This function checks, which STOK-polyogns are within the boundaries of the test area, selects the respective soil-dfs, and returns a list.
#'
#' @param poly polygon of the test area in EPSG:32632 as full path with file name and ending. Accepts all files that can be read with \code{sf::st_read}
#'
#' @return returns a list of data frames with ID_custom (optional), x- and y- coordinates
#'
#' @export

#poly <- st_read("J:/FVA-Projekte/P01540_WHHKW/Daten/Ergebnisse/Modellierung_ext/BIWAK/input_data/albkreis.shp")
fnc_get_stok_data <- function(poly,
                              PTF){

  stok <- sf::st_read(paste0(path_STOK_pieces, "STOK_IDs.shp"))

  if(crs(stok) != crs(poly)){
    poly <- sf::st_transform(poly, crs = crs(stok))
  }
  STOK_IDs_needed <- sf::st_intersection(poly, stok) %>% unique()

  soils_out <- readRDS(paste0(path_STOK_pieces, "soils_", PTF, ".shp"))
  soils_out <- soils_out[names(soils_out) %in% STOK_IDs_needed]

  return(soils_out)
}
#tm_shape(test) + tm_polygons()
