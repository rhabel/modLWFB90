#' UTM-GK3 transformation function
#'
#' Some of the data (i.e. DGM, regionalised BZE-data) is stored at the FVA in rasters with GK-3 projection. This function takes a dataframe of Coordinates in UTM (EPSG:32632) and projects them to GK-3 for data extraction of GK-3 rasters.
#'
#'
#' @param df a data frame containing the name of the ids \code{ID}, as well as the UTM-Coordinates in columns named \code{easting} and  \code{northing}.
#' @param to_crs a string containing the coordinate system to be projected to. Either \code{GK3}, \code{latlon}, or \code{UTM_25832}. \code{GK3} by default
#'
#'
#' @return A spatialpointsdataframe with the projected points in the respective.  Data type changes because of further processing within fnc_soil_bze( ).
#'
#' @example inst/examples/fnc_transf_to_gk_ex.R
#' @export
fnc_transf_crs <- function(df,
                           to_crs = "GK3"){

  sf_df_gk <- st_as_sf(df, coords = c("easting", "northing"), crs = 32632)

  if(to_crs == "GK3" ){
    df_gk <- sf::st_transform(sf_df_gk, 31467) #Koordinaten-Tranformation
  }else if (to_crs == "UTM_25832"){
    df_gk <- sf::st_transform(sf_df_gk, 25832) #Koordinaten-Tranformation
  }else if (to_crs == "latlon"){
    df_gk <- sf::st_transform(sf_df_gk, 4326) #Koordinaten-Tranformation
  }else{
    stop("please provide valid CRS...")
  }
  df_gk <- dplyr::left_join(df_gk, df[,c("ID_custom", "easting", "northing")], by = "ID_custom")
  df_gk <- sf::as_Spatial(df_gk)
  return(df_gk)
}
