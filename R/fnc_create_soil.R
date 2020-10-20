#' Soil-list creation
#'
#' This function is a wrapper of several functions and chunks of code and the main point of access for the final user. It takes a dataframe of coordinates of the points to be modeled and returns a list of soil data frames as required by \code{LWFBrook90R}. Adjustment options exist for the origin of soil data, the PTF to be used, whether MvG-parameters should be limited to a certain range, as well as all options for roots included in \code{\link[LWFBrook90R]{MakeRelRootDens}} and \link{fnc_roots} that can be passed down from here.
#'
#' @param df.ids a data frame containing the following columns: \code{ID} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to. \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' @param soil_option whether BZE or STOK data should be used for modelling. While option \code{BZE} shouldn't create NAs, option \code{STOK} builds on the data of the Standortskartierung Baden-Wuerttemberg that is not available everywhere (i.e. in private forests). The functions asks the used how to deal with NAs, whether they should be excluded, or completed with BZE-data.
#' @param testgebiet at the current stage of development, we're working with test areas that have to be named here as \code{BDS} for Bodensee/07b, or \code{NPS} for National Park Schwarzwald. Eventually, this should be replaced by a BW-wide option. This parameter might be useful if a spatial pre-selection should be included for performance optimization...
#' @param PTF the PTF to be used in the modeling process. Options are \code{HYPRES}, \code{PTFPUH2}, or \code{WESSOLEK}
#' @param limit_MvG should the hydraulic parameters limited to "reasonable" ranges as described in \code{\link{fnc_limit_MvG}}. Default is \code{FALSE}.
#' @param ... further function arguments to be passed down to \code{\link{fnc_roots}}. Includes all adjustment options to be found in \code{\link[LWFBrook90R]{MakeRelRootDens}}.
#'
#' @return Returns a list of soil data frames completely processed to be further used in \code{\link[LWFBrook90R]{msiterunLWFB90}}.
#'
#' @example inst/examples/fnc_create_soil_ex.R
#' @export

fnc_create_soil <- function(df.ids,
                            soil_option,
                            testgebiet,
                            PTF,
                            limit_MvG = F,
                            ...){
  # transformation of ids to GK3 for slope & aspect ---------- ####
  xy_gk <- fnc_transf_to_gk(df = df.ids)

  dgm.stack <- raster::stack(list.files(input_paul, pattern = "aspect.sdat|slope.sdat", full.names=T))
  df.dgm <- cbind("ID" = df.ids$ID,
                  as.data.frame(fnc_extract_points(lay = dgm.stack, xy = xy_gk)))

  # choice of data origin:  ---------------------------------- ####

  if(soil_option == "STOK"){

    # subset currently still active for faster processing - to be expanded to BW in the future
    sf.testgeb <- get(paste0("sf.STOK.", testgebiet))
    df.LEIT <- get(paste0("df.LEIT.", testgebiet))

    sf.ids <- st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632) %>%
      st_join(sf.testgeb) %>%
      st_drop_geometry() %>%
      dplyr::select(ID, RST_F)

    IDs_miss <- sf.ids$ID[is.na(sf.ids$RST_F)]
    IDs_complete <- which(!is.na(sf.ids$RST_F)) # IDs good

    # all IDs mapped by STOKA
    if(length(IDs_miss) == 0){
      ls.soils <- fnc_soil_stok(df = sf.ids,
                                df.LEIT = get(paste0("df.LEIT.", testgebiet)))
      names(ls.soils) <- df.ids$ID

    } else {
      cat("IDs ", IDs_miss, " are not mapped by STOKA. How do you wish to proceed? \nPress \"1\" for not modelling missing IDs.\nPress \"2\" for using regionalised BZE-Data for missing IDs.")
      how_to_proceed <- readline(prompt = "Continue with ")

      if(how_to_proceed == "1"){
        sf.ids <- sf.ids[complete.cases(sf.ids),] # remove missing IDs
        ls.soils <- fnc_soil_stok(df = sf.ids,
                                  df.LEIT = get(paste0("df.LEIT.", testgebiet)))
        names(ls.soils) <- df.ids$ID[IDs_complete]

      }

      if(how_to_proceed == "2"){
        ls.soils[IDs_complete] <- fnc_soil_stok(df = sf.ids[!is.na(sf.ids$RST_F),],
                                                df.LEIT = get(paste0("df.LEIT.", testgebiet)))
        xy_gk_miss <- fnc_transf_to_gk(df = df.ids[is.na(sf.ids$RST_F),])
        ls.soils[ID_miss] <- fnc_soil_bze(df.gk = xy_gk_miss)

        names(ls.soils) <- df.ids$ID

      }

    }

  } else if (soil_option == "BZE") {
    ls.soils <- fnc_soil_bze(df.gk = xy_gk)


  } else {
    stop("Please provide valid soil-option")
  }

  # PTF-application :
  ls.soils <- lapply(ls.soils, FUN = fnc_PTF, PTF_used = PTF)

  # MvG-limitation if desired:
  if(limit_MvG){
    ls.soils <- lapply(ls.soils, FUN = fnc_limit)
  }


  # Roots:
  ls.soils <- lapply(ls.soils, FUN = fnc_roots, ...)


  return(ls.soils)
}
