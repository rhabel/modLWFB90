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
#' @param soil_option whether BZE or STOK data should be used for modelling. While option \code{BZE} with a buffer of 50 shouldn't create many NAs, option \code{STOK} builds on the data of the Standortskartierung Baden-Wuerttemberg that is not available everywhere (i.e. in private forests). \cr Option \code{STOK_BZE} will complete the missing STOK-points with BZE-data. \cr The final option is \code{OWN}, in which case users can enter their own soil data (i.e. from lab or field experiments). If the option \code{OWN} is selected, the dataframes must be passed at \code{df.soils}. \cr \cr at the moment the combination \code{STOK_BZE} does not work yet with \code{incl_GEOLA}
#' @param create_roots decides whether roots should be created manually (if set to \code{TRUE})
#' @param add_BodenInfo shall further soil info (nFK, PWP, FK, texture ...) be added to the soil-df, default is \code{TRUE}
#' @param add_dummy adds 1m of dummy-soil layer to the bottom of the soil profile. Adding a dummy layer has been observed to improve results. Default is \code{TRUE}
#' @param incl_GEOLA information from the \emph{Geowissenschaftliche Landesaufnahme} will be used to get additional data on soil depth and max root depth, as well as identifying soil types that will be modelled differently to include the effect of groundwater (Gleye / Auenboeden) or alternating Saturation (Stauwasserboeden). Default is \code{TRUE}
#' @param pth_df.LEIT path to .RData file with soil information from Modul1-DB. Should be the extended version containing a column for humus, currently set to latest location.
#' @param pth_WGB_diss_shp path to dissolved WUCHSGEBIET-shapefile, currently set to latest location.
#' @param pth_STOK_pieces path to Wuchsgeb-STOKA.shapefiles, currently set to latest location.
#' @param PTF_to_use the PTF to be used in the modeling process. Options are \code{HYPRES}, \code{PTFPUH2}, or \code{WESSOLEK}. Alternatively, if MvG parameters have been retrieved elsewhere (i.e. by lab analyses), \code{OWN_PARMS} can be selected to skip this.
#' @param limit_MvG should the hydraulic parameters limited to "reasonable" ranges as described in \code{\link{fnc_limit}}. Default is \code{FALSE}.
#' @param limit_bodtief max soil depth, default is \code{NA} and uses max soil depth as defined in \code{df.LEIT}, \code{BZE} or the GEOLA-dataset. If not \code{NA}, soil-dfs are created down to the depth specified here as depth in \code{m}, negative
#' @param ... further function arguments to be passed down to \code{\link{fnc_roots}}. Includes all adjustment options to be found in \code{\link[LWFBrook90R]{make_rootden}}. \cr Only exception is the roots functions' parameter \code{maxrootdepth}, which, if desired, has to be specified here as  \code{roots_max}, because maximal root depth setting according to vegetation parameters will be complemented by root limitations from soil conditions. \cr Settings can be either single values, applied to all soil data frames equally, or vector with the same length as \code{df.ids} specifying the roots setting for each modelling point. see example
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
                         PTF_to_use,

                         bze_buffer = NA,
                         limit_MvG = T,
                         df.soils = NULL,
                         meta.out = NA,
                         add_BodenInfo = T,
                         create_roots = T,
                         limit_bodtief = NA,
                         incl_GEOLA = T,
                         add_dummy = T,

                         ...,

                         pth_df.LEIT = "H:/FVA-Projekte/P01540_WHHKW/Daten/Ergebnisse/Modellierung_Testregionen/Leitprofile/Modul1DB.Rdata",
                         pth_WGB_diss_shp = "H:/BU/Gis/Themen/Vektor/Wugeb_Dissolve.shp",
                         pth_STOK_pieces = "H:/FVA-Projekte/P01717_DynWHH/Daten/Urdaten/Wuchsgebiete/Wuchsgebiete_red/",
                         pth_GEOLA_pieces = "H:/FVA-Projekte/P01717_DynWHH/Daten/Urdaten/Geola/"

                         ){
  argg <- c(as.list(environment()), list(...))

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

    # CRS for GEOLA and Wuchsgebiete
    sf.ids <- sf::st_as_sf(df.ids.25832, coords = c("easting", "northing"), crs = 25832)

    #Due to RAM issues the STOKA-shapefile was divided into 7 parts, each of them comprising a Wuchsbezirk
    #read shapefile with an overview over Wuchsbezirke in BW and test which STOKA-files are required
    sf.wugeb <- sf::st_read(pth_WGB_diss_shp, quiet = T) %>%
      sf::st_transform(crs= 25832)
    wugeb <- sort(paste0(unique(unlist(sf::st_intersects(sf.ids, sf.wugeb), recursive = F)), ".shp"), decreasing = F)
    #Read required STOKA shapefiles for RST_F and OA_ID

    cl <- parallel::makeCluster(ifelse(length(wugeb) < parallel::detectCores(),
                                       length(wugeb), parallel::detectCores()))
    doParallel::registerDoParallel(cl)

    sf.gebiet <- foreach::foreach(i = wugeb,
                     .packages = "sf",
                     .combine = rbind) %dopar% {
                       sf::st_read(paste0(pth_STOK_pieces, i), quiet = T)
                     }

    parallel::stopCluster(cl)


    # join with GEOLA
    if(incl_GEOLA){

      cl <- parallel::makeCluster(ifelse(length(wugeb) < parallel::detectCores(),
                                         length(wugeb), parallel::detectCores()))
      doParallel::registerDoParallel(cl)

      sf.geola <- foreach::foreach(i = wugeb,
                                   .packages = "sf",
                                   .combine = rbind) %dopar% {
                                     sf::st_read(paste0(pth_GEOLA_pieces, i),
                                                 quiet = T)
                                   }

      parallel::stopCluster(cl)

      #spatial join sf.ids with sf.geola
      sf.ids <-  sf.ids %>%
        sf::st_join(sf.geola) %>%
        dplyr::mutate(GRUND_C = as.numeric(GRUND_C))

      sf.ids <- sf.ids[!duplicated(sf.ids), ]
    }

    #spatial join sf.ids with sf.gebiet
    sf.ids <-  sf.ids %>%
                  sf::st_join(sf.gebiet) %>%
                  sf::st_drop_geometry() %>%
                  dplyr::select(-c(HOE, RST_Z1, MOR_Strat1, HU, WHH, WAS, area_ha, WugebNr))
    sf.ids <- sf.ids[!duplicated(sf.ids), ]

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

    # clear up space
    rm(sf.gebiet, sf.geola, sf.wugeb, RST_noforest, RST_moor, RST_LEIT); gc()


    IDs_miss <- sf.ids$ID[(is.na(sf.ids$RST_F) | sf.ids$RST_F %in% RST_miss)] # remove non-forest-rst_fs
    IDs_good <- sf.ids$ID[!sf.ids$ID %in% IDs_miss] # IDs good

    # all IDs mapped by STOKA
    if(length(IDs_miss) == 0){

      ls.soils <- fnc_soil_stok(df = sf.ids,
                                df.LEIT = df.LEIT.BW,
                                PTF_to_use = PTF_to_use,
                                dgm = df.dgm,
                                limit_bodtief = limit_bodtief,
                                incl_GEOLA = incl_GEOLA)

      bodentypen <- unlist(lapply(ls.soils, function(x) unique(x$BODENTYP)))
      dpth_lim_soil <- unlist(lapply(ls.soils, function(x) unique(x$dpth_ini)))

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
                                      limit_bodtief = limit_bodtief,
                                      incl_GEOLA = incl_GEOLA)

        names(ls.soils.tmp) <- unlist(lapply(ls.soils.tmp, function(x) unique(x$ID_custom)))
        ls.soils[match(names(ls.soils.tmp), names(ls.soils))] <- ls.soils.tmp

        bodentypen <- unlist(lapply(ls.soils, function(x) unique(x$BODENTYP)))
        dpth_lim_soil <- unlist(lapply(ls.soils, function(x) unique(x$dpth_ini)))

      } else if (soil_option == "STOK_BZE"){

        cat("\nIDs \n",
            as.character(as.data.frame(df.ids)[IDs_miss, "ID_custom"]),
            " \nare not mapped by STOKA. They will be modelled using regionlized BZE data.\n\n")

        if(length(limit_bodtief) > 1){
          limit_bodtiefSTOK <- limit_bodtief[IDs_good]
          limit_bodtiefBZE <- limit_bodtief[IDs_miss]
        }else{
          limit_bodtiefSTOK <- limit_bodtief
          limit_bodtiefBZE <- limit_bodtief
        }

        ls.soils[IDs_good] <- fnc_soil_stok(df = sf.ids[IDs_good,],
                                            df.LEIT = df.LEIT.BW,

                                            PTF_to_use = PTF_to_use,
                                            dgm = df.dgm,
                                            limit_bodtief = limit_bodtiefSTOK,
                                            incl_GEOLA = incl_GEOLA)
        df.ids <- df.ids %>%
          dplyr::left_join(df.dgm, by = "ID")
        xy_gk_miss <- fnc_transf_crs(df = df.ids[IDs_miss,],
                                     to_crs = "UTM_25832")
        ls.soils[IDs_miss] <- fnc_soil_bze(df.utm = xy_gk_miss,
                                           df.assign = df.ids[IDs_miss,],
                                           buffering = (!is.na(bze_buffer)),
                                           buff_width = bze_buffer,

                                           limit_bodtief = limit_bodtiefBZE,
                                           incl_GEOLA = incl_GEOLA)

        bodentypen <- unlist(lapply(ls.soils, function(x) unique(x$BODENTYP)))
        dpth_lim_soil <- unlist(lapply(ls.soils, function(x) unique(x$dpth_ini)))

        # names(ls.soils) <- df.ids$ID_custom

      }

    }

  } else if (soil_option == "BZE") {

    df.ids <- df.ids %>%
      dplyr::left_join(df.dgm, by = "ID")

    if(incl_GEOLA){
      # join with GEOLA
      sf.ids <- sf::st_as_sf(df.ids.25832,
                             coords = c("easting", "northing"), crs = 25832)
      sf.wugeb <- sf::st_read(pth_WGB_diss_shp, quiet = T) %>%
        sf::st_transform(crs= 25832)

      wugeb <- sort(paste0(unique(unlist(sf::st_intersects(sf.ids,
                                                           sf.wugeb),
                                         recursive = F)), ".shp"),
                    decreasing = F)

      #Read required GEOLA shapefiles
      cl <- parallel::makeCluster(ifelse(length(wugeb) < parallel::detectCores(),
                                         length(wugeb),
                                         parallel::detectCores()))
      doParallel::registerDoParallel(cl)

      sf.geola <- foreach::foreach(i = wugeb,
                                   .packages = "sf",
                                   .combine = rbind) %dopar% {
                                     sf::st_read(paste0(pth_GEOLA_pieces, i),
                                                 quiet = T)
                                   }

      parallel::stopCluster(cl)

      sf.ids <-  sf.ids %>%
        sf::st_join(sf.geola) %>%
        sf::st_drop_geometry()%>%
        dplyr::mutate(GRUND_C = as.numeric(GRUND_C))

      df.ids <- df.ids %>%
        dplyr::left_join(sf.ids, by = c("ID", "ID_custom")) %>%
        distinct(.)
    }

    xy_proj <- fnc_transf_crs(df = df.ids,
                              to_crs = "UTM_25832")

    ls.soils <- fnc_soil_bze(df.utm = xy_proj,
                             df.assign = df.ids,
                             buffering = (!is.na(bze_buffer)),
                             buff_width = bze_buffer,

                             limit_bodtief = limit_bodtief,
                             meta.out = meta.out,
                             incl_GEOLA = incl_GEOLA)

    bodentypen <- unlist(lapply(ls.soils, function(x) unique(x$BODENTYP)))
    dpth_lim_soil <- unlist(lapply(ls.soils, function(x) unique(x$dpth_ini)))

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
      cat("\n", missingcol, "is missing")
      stop("missing columns")
    }else{
      ls.soils <- lapply(ls.soils, FUN = function(x){
        if("humus" %in% colnames(x)){
          humus <- x$humus[1]
          df <- x[0,]
          df[1,] <- NA
          df$mat <- 0; df$ID <- unique(x$ID); df$ID_custom <- unique(x$ID_custom);
          df$gravel <- 0; df$ths <- 0.848; df$thr <- 0; df$alpha <- 98; df$npar <- 1.191; df$mpar <- 0.160;
          df$ksat <- 98000; df$tort <- 0.5; df$upper <- humus; df$lower <- 0

          x <- rbind(df, x)
          for(i in 1:ncol(x)){
            if(is.na(x[1,i])){x[1,i] <- x[2,i]}
          }
          x <- x[,-which(colnames(x) == "humus")]
        }

      })
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

  # Roots: --------------------------------------------------- ####
  if(create_roots){

    non.nas <- which(unlist(lapply(ls.soils, is.null))==F)

    if(soil_option != "OWN"){

      # roots limited by soil conditions and/or vegetation parameters
      if(any(stringr::str_detect(names(argg), "roots_max"))){
        roots_max <- argg[[which(names(argg) == "roots_max")]]
        roots_max_cm <- roots_max*-100

        if(length(roots_max) == 1){
          dpth_lim_veg <- rep(roots_max_cm, length(non.nas))
        }else{
          dpth_lim_veg <- roots_max_cm
        }

        maxdepth <- pmin(dpth_lim_soil, dpth_lim_veg, na.rm = T)/-100

      } else {
        maxdepth <- dpth_lim_soil/-100
      }


      ls.soils[non.nas] <- mapply(FUN = fnc_roots,
                                  ls.soils[non.nas],

                                  roots_max_adj = maxdepth,
                                  # beta = 0.97,
                                  # rootsmethod = "betamodel",
                                  # # maxrootdepth = c(-1,-1.5,-0.5, -1.5,-2),

                                  ...,

                                  SIMPLIFY = F)
    }else{
      ls.soils[non.nas] <- mapply(FUN = fnc_roots,
                                  ls.soils[non.nas],

                                  # rootsmethod = "betamodel",
                                  # beta = 0.97,
                                  # maxrootdepth = -2,
                                  # # maxrootdepth = c(-1,-1.5,-0.5, -1.5,-2),

                                  ...,

                                  SIMPLIFY = F)
    }




  }

  # GEOLA application ---------------------------------------- ####
  if(incl_GEOLA){

    non.nas <- which(unlist(lapply(ls.soils, is.null))==F)

    if(soil_option == "STOK"){

      ls.soils[non.nas] <- mapply(FUN = function(x, bodentyp){
          x$soiltype <- bodentyp
          if(bodentyp == "Gleye/Auenboeden"){
            x[c(nrow(x)-1, nrow(x)), "ksat"] <- 0.0001
          }
          return(x)
        },
        ls.soils[non.nas],
        bodentypen,
        SIMPLIFY = F)

    }

    if(stringr::str_detect(soil_option, "BZE")){
      if(soil_option == "BZE"){

        ls.soils[non.nas] <- mapply(FUN = function(x, bodentyp){
          if(bodentyp == "Stauwasserboeden"){
            mvg <- hydpar_hypres(clay = 30, silt = 70, bd = 2, topsoil = F)
            mvg$ksat <- 10 # Aus Sd-Definition in der KA5
            n_rep <- 3 #

            lastrow <- subset(x, nl == max(nl))

            #df.sd erstellen (letzte Zeile von df.soil, um Bodeninfo zu uebernehmen)
            df.sd <- as.data.frame(lapply(lastrow, rep, n_rep))

            # Veraenderliche Spalten aendern
            if("rootden" %in% colnames(x)){
              df.sd <- df.sd %>% mutate(rootden = 0)
            }
            df.sd <- df.sd %>%
              mutate(mat = mat + 1,
                     nl = nl + c(1: n_rep),
                     lower = lastrow$lower + c(-0.1, -0.2, -0.3),
                     upper = lastrow$lower + c(0, -0.1, -0.2),
                     sand = 0,
                     silt = 70,
                     clay = 30,
                     bd = 2,

                     #MvG-Parameter Ls2 fuer Stauhorizont
                     ths = mvg$ths,
                     thr = mvg$thr,
                     alpha = mvg$alpha,
                     npar = mvg$npar,
                     mpar = mvg$mpar,
                     tort = mvg$tort,
                     ksat = mvg$ksat) %>%
              relocate(names(lastrow))

            # df.stau an df.soils anfuegen
            x <-   rbind(x, df.sd)
            x$soiltype <- bodentyp
            return(x)
          } else if(bodentyp == "Gleye/Auenboeden"){
            # stop water from leaving horizon below 2.60
            x$mat[tail(x$nl, 2)] <- max(x$mat)+1
            x$ksat[tail(x$nl, 2)] <- 0.0001
            x$rootden[tail(x$nl, 2)] <- 0
            x$soiltype <- bodentyp

            return(x)
          }else{
            x$soiltype <- bodentyp
            return(x)
          }
        },
        ls.soils[non.nas],
        bodentyp = bodentypen,
        SIMPLIFY = F)
      }
    }
  }



  # add dummy soil horizons ---------------------------------- ####
  if(add_dummy){
    ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)],
                                                                     FUN = function(x){
                                                                       lastrow <- x[nrow(x),]
                                                                       df.dummy <- as.data.frame(lapply(lastrow, rep, 6)) %>%
                                                                         mutate(mat = mat + 1,
                                                                                nl = nl + c(1:6),
                                                                                upper = lastrow$lower + c(0, -0.1, -0.2, -0.4, -0.6, -0.8),
                                                                                lower = lastrow$lower + c(-0.1, -0.2, -0.4, -0.6, -0.8, -1),
                                                                                ths = 0.1,
                                                                                thr = 0,
                                                                                alpha = 1.3,
                                                                                ksat = 50,
                                                                                tort = -0.5)
                                                                       if("rootden" %in% colnames(x)){
                                                                         df.dummy <- df.dummy %>% mutate(rootden = 0)
                                                                       }
                                                                       df.dummy <- df.dummy %>%
                                                                         relocate(names(x))
                                                                       x <- rbind(x, df.dummy)
                                                                       return(x)
                                                                     })
  }

  # add_BodenInfo: ------------------------------------------- ####
  if(add_BodenInfo){
    ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)],
                                                                     fnc_add_nFK)
  }

  # reduce --------------------------------------------------- ####
  to_2 <- c("sand", "silt","clay", "oc.pct",  "tort")
  to_3 <- c("gravel", "bd", "ths", "thr", "alpha", "npar", "mpar", "rootden" )
  ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)] <- lapply(ls.soils[which(!unlist(lapply(ls.soils, is.null))==T)],
                                                                   FUN = function(x){
                                                                     x <- x %>%
                                                                       dplyr::mutate(across(any_of(to_2), ~round(.x, 2)),
                                                                                     across(any_of(to_3), ~round(.x, 3)))
                                                                   })



  return(ls.soils)
}

