#' Soil-list creation
#'
#' This function is a wrapper of several functions and chunks of code and the main point of access for the final user. It takes a dataframe of coordinates of the points to be modeled and returns a list of soil data frames as required by \code{LWFBrook90R}. Adjustment options exist for the origin of soil data, the PTF to be used, whether MvG-parameters should be limited to a certain range, as well as all options for roots included in \code{\link[LWFBrook90R]{MakeRelRootDens}} and \link{fnc_roots} that can be passed down from here.
#'
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param soil_option whether BZE or STOK data should be used for modelling. While option \code{BZE} shouldn't create NAs, option \code{STOK} builds on the data of the Standortskartierung Baden-Wuerttemberg that is not available everywhere (i.e. in private forests). The functions asks the used how to deal with NAs, whether they should be excluded, or completed with BZE-data. Third option is \code{OWN}, in which case users can enter their own soil data (i.e. from lab or field experiements). If the option \code{OWN} is selected, the dataframe to be
#' @param testgebiet at the current stage of development, we're working with test areas that have to be named here as \code{BDS} for Bodensee/07b, or \code{NPS} for National Park Schwarzwald. Eventually, this should be replaced by a BW-wide option. This parameter might be useful if a spatial pre-selection should be included for performance optimization...
#' @param PTF_to_use the PTF to be used in the modeling process. Options are \code{HYPRES}, \code{PTFPUH2}, or \code{WESSOLEK}. Alternatively, if MvG parameters have been retrieved elsewhere (i.e. by lab analyses), \code{OWN_PARMS} can be selected to skip this.
#' @param limit_MvG should the hydraulic parameters limited to "reasonable" ranges as described in \code{\link{fnc_limit}}. Default is \code{FALSE}.
#' @param ... further function arguments to be passed down to \code{\link{fnc_roots}}. Includes all adjustment options to be found in \code{\link[LWFBrook90R]{MakeRelRootDens}}.
#' @param df.soils if \code{OWN} is selected at soil_option, a data frame must be given here that contains the following columns
#' \itemize{
#' \item \code{ID} - a unique ID matching the IDs of df.ids
#' \item \code{mat} - number of soil layer starting with 1 counting upwards
#' \item \code{upper} and \code{lower} - upper and lower boundaries of soil layers in cm
#' \item \code{humus} - thickness of the humuslayer in m
#' }
#' Caution:\cr
#' If PTFs are to be applied, the columns required in \code{\link{fnc_PTF}} must be provided. Else, if \code{PTF_to_use} is set to \code{OWN_PARMS}, the following columns must be provided NA-free: \code{ths, thr, alpha, npar, mpar, ksat}, and \code{tort}.\cr
#' If roots are to be calculated, the columns required in \code{\link{fnc_roots}} must be provided. Otherwise they need to be stored in a column called \code{rootden}.
#'
#' @return Returns a list of soil data frames completely processed to be further used in \code{\link[LWFBrook90R]{msiterunLWFB90}} or \code{\link[LWFBrook90R]{runLWFB90}}
#'
#' @example inst/examples/fnc_get_soil_ex.R
#' @export

fnc_get_soil <- function(df.ids,
                         soil_option,
                         testgebiet,
                         PTF_to_use,
                         limit_MvG = F,
                         df.soils = NULL,
                         ...){

  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  # transformation of ids to GK3 for slope & aspect ---------- ####
  xy_gk <- fnc_transf_to_gk(df = df.ids)

  dgm.stack <- raster::stack(list.files(input_paul, pattern = "aspect.sdat|slope.sdat", full.names=T))
  df.dgm <- cbind("ID" = df.ids$ID,
                  as.data.frame(fnc_extract_points(lay = dgm.stack, xy = xy_gk)))

  # initialise list
  ls.soils <- vector("list", length = nrow(df.ids))
  names(ls.soils) <- df.ids$ID_custom

  # choice of data origin:  ---------------------------------- ####

  if(soil_option == "STOK"){

    # subset currently still active for faster processing - to be expanded to BW in the future
    sf.testgeb <- get(paste0("sf.STOK.", testgebiet))
    df.LEIT <- get(paste0("df.LEIT.", testgebiet))

    sf.ids <- sf::st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632) %>%
      sf::st_join(sf.testgeb) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(ID, ID_custom, RST_F)

    IDs_miss <- sf.ids$ID[is.na(sf.ids$RST_F)]
    IDs_complete <- which(!is.na(sf.ids$RST_F)) # IDs good

    # all IDs mapped by STOKA
    if(length(IDs_miss) == 0){
      ls.soils <- fnc_soil_stok(df = sf.ids,
                                df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                PTF_to_use = PTF_to_use,
                                dgm = df.dgm)

      names(ls.soils) <- df.ids$ID_custom

    } else {
      cat("IDs ", as.character(df.ids[IDs_miss, "ID_custom"]), " are not mapped by STOKA. How do you wish to proceed? \nPress \"1\" for not modelling missing IDs.\nPress \"2\" for using regionalised BZE-Data for missing IDs.")
      how_to_proceed <- readline(prompt = "Continue with ")

      if(how_to_proceed == "1"){
        sf.ids <- sf.ids[complete.cases(sf.ids),] # remove missing IDs
        ls.soils.tmp <- fnc_soil_stok(df = sf.ids,
                                  df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                  PTF_to_use = PTF_to_use,
                                  dgm = df.dgm)
        names(ls.soils.tmp) <- unlist(lapply(ls.soils.tmp, function(x) unique(x$ID_custom)))

        ls.soils[match(names(ls.soils.tmp), names(ls.soils))] <- ls.soils.tmp

      }

      if(how_to_proceed == "2"){
        ls.soils[IDs_complete] <- fnc_soil_stok(df = sf.ids[!is.na(sf.ids$RST_F),],
                                                df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                                PTF_to_use = PTF_to_use,
                                                dgm = df.dgm)
        xy_gk_miss <- fnc_transf_to_gk(df = df.ids[is.na(sf.ids$RST_F),])
        ls.soils[IDs_miss] <- fnc_soil_bze(df.gk = xy_gk_miss,
                                           df.assign = df.ids)

        names(ls.soils) <- df.ids$ID_custom

      }

    }

  } else if (soil_option == "BZE") {
    ls.soils <- fnc_soil_bze(df.gk = xy_gk,
                             df.assign = df.ids)


  } else if (soil_option == "OWN") {

    if(!all(df.ids$ID_custom == unique(df.soils$ID_custom))){
      stop("not all ID_custom of df.ids and df.soils are equal")
    } else {
      ls.soils <- df.soils %>%
        dplyr::left_join(df.ids[c("ID_custom", "ID")], by = "ID_custom") %>%
        dplyr::arrange(ID, mat, -upper) %>%
        dplyr::select(ID, ID_custom, everything()) %>%
        dplyr::group_split(ID)
      ls.soils <- lapply(ls.soils, FUN = fnc_depth_disc)
      ls.soils <- lapply(ls.soils, FUN = dplyr::left_join, y = df.dgm, by = "ID")
      ls.soils <- lapply(ls.soils, FUN = dplyr::mutate, upper = upper/-100)
      ls.soils <- lapply(ls.soils, FUN = dplyr::mutate, lower = lower/-100)

      names(ls.soils) <- df.ids$ID_custom
    }

  } else {
    stop("Please provide valid soil-option")
  }

  # PTF-application: ----------------------------------------- ####
  if(PTF_to_use == "OWN_PARMS"){

    # check if all necessary columns are there:
    missingcol <- c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort")[!c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort") %in% names(df.soils)]
    cat(missingcol, "is missing in df.soils for PTF-application of ", PTF_to_use, "\n")
    stop("missing columns")

  } else {
    ls.soils[as.numeric(which(!unlist(lapply(ls.soils, is.null))==T))] <- lapply(ls.soils[as.numeric(which(!unlist(lapply(ls.soils, is.null))==T))], FUN = fnc_PTF, PTF_used = PTF_to_use)
  }

  # MvG-limitation if desired: ------------------------------- ####
  if(limit_MvG){
    ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)], FUN = fnc_limit)
  }


  # Roots:
  ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)], FUN = fnc_roots, ...)


  return(ls.soils)
}