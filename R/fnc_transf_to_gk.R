#' UTM-GK3 transformation function
#'
#' Some of the data (i.e. DGM, regionalised BZE-data) is stored at the FVA in rasters with GK-3 projection. This function takes a dataframe of Coordinates in UTM (EPSG:32632) and projects them to GK-3 for data extraction of GK-3 rasters.
#'
#'
#' @param df a data frame containing the name of the ids \code{ID}, as well as the UTM-Coordinates in columns named \code{easting} and  \code{northing}.
#'
#'
#' @return A spatialpointsdataframe with the projected points in GK3. Data type changes because of further processing within fnc_soil_bze( ).
#'
#' @example inst/examples/fnc_transf_to_gk_ex.R
#'
fnc_transf_to_gk <- function(df){
  df_gk <- cbind(df,
                      "coords_x" = numeric(nrow(df)),
                      "coords_y" = numeric(nrow(df)))
  df_gk$coords_x <- as.numeric(df$easting) # Koordinaten aus IDs extrahieren
  df_gk$coords_y <- as.integer(df$northing)

  sp::coordinates(df_gk) <- c("coords_x", "coords_y")
  UTM32632 <- sp::CRS("+init=EPSG:32632")
  GK3 <- sp::CRS("+init=EPSG:31467")
  wgs.84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  sp::proj4string(df_gk) <- UTM32632
  df_gk <- sp::spTransform(df_gk, GK3) #Koordinaten-Tranformation

  return(df_gk)
}
