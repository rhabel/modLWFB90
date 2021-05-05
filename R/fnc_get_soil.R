#' Soil-list creation
#'
#' This function is a wrapper of several functions and chunks of code and the main point of access for the final user. It takes a dataframe of coordinates of the points to be modeled and returns a list of soil data frames as required by \code{LWFBrook90R}. Adjustment options exist for the origin of soil data, the PTF to be used, whether MvG-parameters should be limited to a certain range, as well as all options for roots included in \code{\link[LWFBrook90R]{make_rootden}} and \code{\link{fnc_roots}} that can be passed down from here.
#'
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param soil_option whether BZE or STOK data should be used for modelling. While option \code{BZE} with a buffer of 50 shouldn't create many NAs, option \code{STOK} builds on the data of the Standortskartierung Baden-Wuerttemberg that is not available everywhere (i.e. in private forests). \cr Option \code{STOK_BZE} will complete the missing STOK-points with BZE-data. \cr The final option is \code{OWN}, in which case users can enter their own soil data (i.e. from lab or field experiments). If the option \code{OWN} is selected, the dataframes must be passed at \code{df.soils}.
#' @param pth_df.LEIT path to .RData file with soil information from Modul1-DB. Should be the extended version containing a column for humus, currently set to latest location.
#' @param pth_WGB_diss_shp path to dissolved WUCHSGEBIET-shapefile, currently set to latest location.
#' @param pth_STOK_pieces path to Wuchsgeb-STOKA.shapefiles, currently set to latest location.
#' @param PTF_to_use the PTF to be used in the modeling process. Options are \code{HYPRES}, \code{PTFPUH2}, or \code{WESSOLEK}. Alternatively, if MvG parameters have been retrieved elsewhere (i.e. by lab analyses), \code{OWN_PARMS} can be selected to skip this.
#' @param limit_MvG should the hydraulic parameters limited to "reasonable" ranges as described in \code{\link{fnc_limit}}. Default is \code{FALSE}.
#' @param limit_bodtief whether soil-df should be reduced to the depth provided by the BZE-layer "Bodentiefe" (if \code{soil_option = "BZE"}) or to the lowest layer of the "Leitprofil" (if \code{soil_option = "STOK"}).\cr Default is \code{FALSE}. If \code{FALSE}, the soil-df are created down to a depth of 2.50 m to give room for different \code{maxrootdepth} - settings in \code{\link{fnc_get_params}}. If \code{TRUE}, soil depth may be reduced significantly.
#' @param ... further function arguments to be passed down to \code{\link{fnc_roots}}. Includes all adjustment options to be found in \code{\link[LWFBrook90R]{make_rootden}}.
#' @param bze_buffer whether buffer should be used in extracting points from BZE raster files if \code{NAs} occur in {m}, default is \code{NA}
#' @param df.soils if \code{OWN} is selected at soil_option, a data frame must be given here that contains the following columns
#' \itemize{
#' \item \code{ID} - a unique ID matching the IDs of df.ids
#' \item \code{mat} - number of soil layer starting with 1 counting upwards
#' \item \code{upper} and \code{lower} - upper and lower boundaries of soil layers in cm
#' \item \code{humus} - thickness of the humuslayer in m
#' }
#' Caution:\cr
#' If PTFs are to be applied, the columns required in \code{\link{fnc_PTF}} must be provided. Else, if \code{PTF_to_use} is set to \code{OWN_PARMS}, the following columns must be provided NA-free: \code{ths, thr, alpha, npar, mpar, ksat}, and \code{tort}.\cr
#' If roots are to be calculated, the columns required in \code{\link{fnc_roots}} must be provided. Otherwise they need to be stored in a column called \code{rootden}.\cr
#' If the nFK shall be calculated at some point, this will be done for the first 1m depth, so in this case one of the layers should end at 100cm depth.
#'
#' @return Returns a list of soil data frames completely processed to be further used by \code{\link[LWFBrook90R]{run_multisite_LWFB90}} or \code{\link[LWFBrook90R]{run_LWFB90}}
#'
#'
#' @example inst/examples/fnc_get_soil_ex.R
#' @export

fnc_get_soil <- function(df.ids,
                         soil_option,

                         pth_df.LEIT = "H:/FVA-Projekte/P01540_WHHKW/Daten/Ergebnisse/Modellierung_Testregionen/Leitprofile/Modul1DB.Rdata",
                         pth_WGB_diss_shp = "H:/BU/Gis/Themen/Vektor/Wugeb_Dissolve.shp",
                         pth_STOK_pieces = "H:/FVA-Projekte/P01717_DynWHH/Daten/Urdaten/Wuchsgebiete/Wuchsgebiete_red/",

                         PTF_to_use,
                         bze_buffer = NA,
                         limit_MvG = T,
                         df.soils = NULL,
                         meta.out = NA,
                         add_BodenInfo = T,
                         create_roots = T,
                         limit_bodtief = F,
                         ...){

  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)
  df.ids.25832 <- fnc_transf_crs(df = df.ids,
                                 to_crs = "UTM_25832")
  df.ids.25832 <- df.ids.25832@data

  # transformation of ids to GK3 for slope & aspect ---------- ####
  xy_gk <- fnc_transf_crs(df = df.ids)

  dgm.stack <- raster::stack(list.files(input_paul, pattern = "aspect.sdat|slope.sdat", full.names=T))
  df.dgm <- cbind("ID" = df.ids$ID,
                  as.data.frame(fnc_extract_points_dgm(lay = dgm.stack,
                                                       xy = xy_gk)))

  # initialise list
  ls.soils <- vector("list", length = nrow(df.ids))
  names(ls.soils) <- df.ids$ID_custom

  # choice of data origin:  ---------------------------------- ####

  if(stringr::str_detect(soil_option, "STOK")){

    # load df.LEIT
    load(file = pth_df.LEIT)

    # subset currently still active for faster processing - to be expanded to BW in the future

    #sf.testgeb <- get(paste0("sf.STOK.", testgebiet))
    #df.LEIT <- get(paste0("df.LEIT.", testgebiet))

    sf.ids <- sf::st_as_sf(df.ids.25832, coords = c("easting", "northing"), crs = 25832)

    #Due to RAM issues the STOKA-shapefile was divided into 7 parts, each of them comprising a Wuchsbezirk
    #read shapefile with an overview over Wuchsbezirke in BW and test which STOKA-files are required
    sf.wugeb <- sf::st_read(pth_WGB_diss_shp, quiet = T) %>%
      sf::st_transform(crs= 25832)
    wugeb <- sort(paste0(unique(unlist(sf::st_intersects(sf.ids, sf.wugeb), recursive = F)), ".shp"), decreasing = F)
    rm(sf.wugeb); gc()

    #Read required STOKA shapefiles and transform CRS according to sf.ids

      # files <- list.files(pth_STOK_pieces, pattern = ".shp")
      # files <- files[files %in% wugeb]

      # files <- list.files(pth_STOK_pieces)
      # files <- files[!nchar(files) > 5]
      # files <- sort(files[substr(files, 3, 5) == "shp"], decreasing = F)
      # files <- files[files[] == wugeb[]]

    cl <- parallel::makeCluster(ifelse(length(wugeb) < parallel::detectCores(),
                                       length(wugeb), parallel::detectCores()))
    doParallel::registerDoParallel(cl)

    sf.gebiet <- foreach::foreach(i = wugeb,
                     .packages = "sf",
                     .combine = rbind) %dopar% {
                       sf::st_read(paste0(pth_STOK_pieces, i), quiet = T)
                     }
    parallel::stopCluster(cl)


    #Join sf.ids with sf.gebiet
    sf.ids <-  sf.ids %>%
                  sf::st_join(sf.gebiet) %>%
                  sf::st_drop_geometry() %>%
                  dplyr::select(ID, ID_custom, RST_F, OA_ID)

    # clear up space
    rm(sf.gebiet); gc()


    # Identify missing and non-forest RST_F
    #no forest
    RST_noforest <- sf.ids %>%
                        dplyr::filter(OA_ID != 6)  %>%
                        dplyr::pull(RST_F) %>%
                        unique(.)

    #missing RST_F in df.LEIT
    RST_LEIT <- unique(sf.ids$RST_F[!sf.ids$RST_F %in% df.LEIT.BW$RST_F])

    #Swamps
    RST_moor <- df.LEIT.BW %>%
                    dplyr::filter(humusform == "Moor") %>%
                    dplyr::pull(RST_F)%>%
                    as.numeric(.) %>%
                    unique(.)

    RST_miss <- c(RST_noforest, RST_LEIT, RST_moor)
    rm(list = c("RST_noforest", "RST_moor", "RST_LEIT"));


    IDs_miss <- sf.ids$ID[(is.na(sf.ids$RST_F) | sf.ids$RST_F %in% RST_miss)] # remove non-forest-rst_fs
    IDs_good <- sf.ids$ID[!sf.ids$ID %in% IDs_miss] # IDs good

    # all IDs mapped by STOKA
    if(length(IDs_miss) == 0){

      ls.soils <- fnc_soil_stok(df = sf.ids,
                                df.LEIT = df.LEIT.BW,
                                PTF_to_use = PTF_to_use,
                                dgm = df.dgm,
                                limit_bodtief = limit_bodtief)

      names(ls.soils) <- df.ids$ID_custom

    } else {

      if(soil_option == "STOK"){

        cat("\nIDs \n",
            as.character(as.data.frame(df.ids)[IDs_miss, "ID_custom"]),
            " \nare not mapped by STOKA. They will not be modelled.\n\n")

        sf.ids <- sf.ids[-IDs_miss,] # remove missing IDs
        ls.soils.tmp <- fnc_soil_stok(df = sf.ids,
                                      df.LEIT = df.LEIT.BW,
                                      PTF_to_use = PTF_to_use,
                                      dgm = df.dgm,
                                      limit_bodtief = limit_bodtief)
        names(ls.soils.tmp) <- unlist(lapply(ls.soils.tmp, function(x) unique(x$ID_custom)))

        ls.soils[match(names(ls.soils.tmp), names(ls.soils))] <- ls.soils.tmp

      } else if (soil_option == "STOK_BZE"){

        cat("\nIDs \n",
            as.character(as.data.frame(df.ids)[IDs_miss, "ID_custom"]),
            " \nare not mapped by STOKA. They will be modelled using regionlized BZE data.\n\n")

        ls.soils[IDs_good] <- fnc_soil_stok(df = sf.ids[IDs_good,],
                                            df.LEIT = df.LEIT.BW,

                                            PTF_to_use = PTF_to_use,
                                            dgm = df.dgm,
                                            limit_bodtief = limit_bodtief)
        df.ids <- df.ids %>%
          dplyr::left_join(df.dgm, by = "ID")
        xy_gk_miss <- fnc_transf_crs(df = df.ids[IDs_miss,],
                                     to_crs = "UTM_25832")
        ls.soils[IDs_miss] <- fnc_soil_bze(df.utm = xy_gk_miss,
                                           df.assign = df.ids[IDs_miss,],
                                           buffering = (!is.na(bze_buffer)),
                                           buff_width = bze_buffer,

                                           limit_bodtief = limit_bodtief)

        # names(ls.soils) <- df.ids$ID_custom

      }

    }

  } else if (soil_option == "BZE") {
    df.ids <- df.ids %>%
      dplyr::left_join(df.dgm, by = "ID")
    xy_proj <- fnc_transf_crs(df = df.ids,
                              to_crs = "UTM_25832")
    ls.soils <- fnc_soil_bze(df.utm = xy_proj,
                             df.assign = df.ids,
                             buffering = (!is.na(bze_buffer)),
                             buff_width = bze_buffer,

                             limit_bodtief = limit_bodtief,
                             meta.out = meta.out)


  } else if (soil_option == "OWN") {

    if(!all(df.ids$ID_custom == unique(df.soils$ID_custom))){
      stop("\n not all ID_custom of df.ids and df.soils are equal")
    } else {
      ls.soils <- df.soils %>%
        dplyr::left_join(df.ids, by = "ID_custom") %>%
        dplyr::arrange(ID, mat, -upper) %>%
        dplyr::select(ID, ID_custom, everything()) %>%
        dplyr::group_split(ID)
      ls.soils <- lapply(ls.soils, FUN = fnc_depth_disc)
      if(!all(c("slope", "aspect") %in% colnames(df.ids))){
        ls.soils <- lapply(ls.soils, FUN = dplyr::left_join, y = df.dgm, by = "ID")
      }
      ls.soils <- lapply(ls.soils, FUN = dplyr::mutate, upper = upper/-100)
      ls.soils <- lapply(ls.soils, FUN = dplyr::mutate, lower = lower/-100)
      ls.soils <- lapply(ls.soils, function(x){cbind(x[,1:3], "nl" = 1:nrow(x), x[4:ncol(x)])})

      names(ls.soils) <- df.ids$ID_custom
    }

  } else {
    stop("\nPlease provide valid soil-option")
  }

  # PTF-application: ----------------------------------------- ####
  if(PTF_to_use == "OWN_PARMS"){

    # check if all necessary columns are there:
    missingcol <- c("ths", "thr", "alpha", "npar",
                    "mpar", "ksat", "tort")[!c("ths", "thr", "alpha", "npar",
                                               "mpar", "ksat", "tort") %in% names(df.soils)]
    if (length(missingcol) > 0){
      cat("\n", missingcol, "is missing in df.soils for PTF-application of ", PTF_to_use, "\n")
      stop("missing columns")
    }

  } else {
    ls.soils[as.numeric(which(!unlist(lapply(ls.soils, is.null))==T))] <- lapply(ls.soils[as.numeric(which(!unlist(lapply(ls.soils, is.null))==T))],
                                                                                 FUN = fnc_PTF,
                                                                                 PTF_used = PTF_to_use)
  }

  # MvG-limitation if desired: ------------------------------- ####
  if(limit_MvG){
    ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)],
                                                                     FUN = fnc_limit)
  }


  # Roots:
  if(create_roots){
    ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)],
                                                                     FUN = fnc_roots, ...)
                                                                     # FUN = fnc_roots,
                                                                     # rootsmethod = "betamodel",
                                                                     # beta = 0.95)
  }

  # nFK:
  if(add_BodenInfo){
    ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)],
                                                                   fnc_add_nFK)
  }


  return(ls.soils)
}
