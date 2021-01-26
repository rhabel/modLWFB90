#' @title Soil comparison
#'
#' @description This function enables the user to visually compare modelling options. As the soil input is likely to be very influencial for the model's output, this is also a reason to check for potentially confusing results.
#' @description The two main options are comparing the soil data itself, and comparing what the PTFs do with the data at hand.
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param testgebiet at the current stage of development, we're working with test areas that have to be named here as \code{BDS} for Bodensee/07b, or \code{NPS} for National Park Schwarzwald. Eventually, this should be replaced by a BW-wide option. This parameter might be useful if a spatial pre-selection should be included for performance optimization...
#' @param what_to_test sets the main selection, whether soil data or PTF behaviour should be investigated. \cr Must be one of \code{SOILDATA} or \code{PTFs}.
#' @param depth_to_test the depth in which comparisons should be made in m negative downwards from the surface. Default is \code{c(-0.15, -0.30, -0.60)}.
#' @param soiloption_to_test if \code{what_to_test} is \code{SOILDATA}: \cr which datasets should be compared. At least two of \code{STOK}, \code{BZE}, or \code{OWN} if own soil data is available.
#' @param PTF_to_use which PTF should be applied for creating hydraulic information from soil data in \code{soiloption_to_test}. Must be one of \code{HYPRES}, \code{PTFPUH2}, or \code{WESSOLEK}.
#' @param MvG_own_vals if \code{what_to_test} is \code{SOILDATA} and \code{soiloption_to_test} includes \code{OWN}: \cr will MvG parameters be provided (i.e. from lab analyses) or shall the PTF in \code{PTF_to_use} also be applied to the soil data in \code{df.soils}
#' @param bze_buffer whether buffer should be used in extracting points from BZE raster files if \code{NAs} occur in {m}, default is \code{NA}
#' @param PTF_to_test if \code{what_to_test} is \code{PTFs}: \cr which PTFs should be compared. Must be any combination of \code{HYPRES}, \code{PTFPUH2}, or \code{WESSOLEK} and can include \code{OWN_PARMS} if own MvG parameters are given in \code{df.soils}
#' @param soiloption_to_use which soil data should the PTFs in \code{PTF_to_test} be applied to. Must be one of \code{STOK}, \code{BZE}, or \code{OWN} if data is supplied at \code{df.soils}
#' @param limit_MvG should the hydraulic parameters limited to "reasonable" ranges as described in \code{\link{fnc_limit}}. Default is \code{FALSE}.
#' @param df.soils if \code{OWN} is selected in \code{soiloptions_to_test}, or \code{OWN_PARMS}  is selected in \code{PTF_to_test}, these information must be provided here as a dataframe that contains the following columns:\cr
#' \itemize{
#' \item \code{ID_custom} - a unique custom ID matching the ID_custom of df.ids
#' \item \code{mat} - number of soil layer starting with 1 counting upwards
#' \item \code{upper} and \code{lower} - upper and lower boundaries of soil layers in cm
#' \item \code{humus} - thickness of the humuslayer in m
#' }
#' Caution: \cr If PTFs are to be applied here, the columns \code{sand, silt, clay, oc.pct} in %, and \code{bd} in g m-1 must be provided. \cr Else, if \code{PTF_to_test} includes \code{OWN_PARMS}, the following columns must be provided NA-free: \code{ths, thr, alpha, npar, mpar, ksat}, and \code{tort}.
#' @param output_path path to the folder where images shall be printed to
#'
#' @return Returns one image for each ID in df.ids, showing water retention and conductivity curves if \code{PTFs} are investigated. If soil data is compared, soil texture triangle, bd, oc.pct and gravel are also shown.
#'
#' @example inst/examples/fnc_compare_soil_ex.R
#' @export
#'


fnc_compare_soil <- function(df.ids,
                            testgebiet = "BDS",
                            what_to_test = "PTF",
                            depths_to_test = c(-0.15, -0.30, -0.60),

                            soiloptions_to_test = "STOK",
                            MvG_own_vals = F,
                            PTF_to_use = "HYPRES",

                            PTF_to_test = "HYPRES",
                            soiloption_to_use = "STOK",

                            limit_MvG = F,
                            df.soils = NULL,
                            output_path,
                            bze_buffer = NA,
                            meta.out = NA){

  # add / to path if not given with a / at the end
  if(stringr::str_sub(output_path,-1) != "/"){
    output_path <- paste0(output_path, "/")
  }

  how_to_proceed = 2
  if(nrow(df.ids) >= 10){
    cat("This function will create one PNG-file in ", output_path, " for each point in df.ids.\n" )
    cat("So you expect ", nrow(df.ids), " images to be crated. This might take a while.\n How you wish to proceed? \nPress \"1\" for aborting the process.\nPress \"2\" to continue.")
    how_to_proceed <- readline(prompt = "Continue with ")
  }


  if(how_to_proceed == 1){

    stop("You might want to make a selection of df.ids.")


  } else if (how_to_proceed == 2){

    if(what_to_test == "SOILDATA"){

      # initial tasks ------------------------------------------ ####
      # sort dfs according to IDs
      df.ids$ID <- 1:nrow(df.ids)
      # transformation of ids to GK3 for slope & aspect
      xy_gk <- fnc_transf_crs(df = df.ids)

      dgm.stack <- raster::stack(list.files(input_paul, pattern = "aspect.sdat|slope.sdat", full.names=T))
      df.dgm <- cbind("ID" = df.ids$ID,
                      as.data.frame(fnc_extract_points_dgm(lay = dgm.stack, xy = xy_gk, buffering = T)))

      # create data:  ------------------------------------------ ####

      if(any(stringr::str_detect(soiloptions_to_test, "STOK"))){

        # subset currently still active for faster processing - to be expanded to BW in the future
        sf.testgeb <- get(paste0("sf.STOK.", testgebiet))
        df.LEIT <- get(paste0("df.LEIT.", testgebiet))


        sf.ids <- sf::st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632) %>%
          sf::st_join(sf.testgeb) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(ID, ID_custom, RST_F)

        # no forest
        RST_noforest <- c(39272, 39273, 0, 39343, 42046)
        # swamp
        test <- df.LEIT %>% filter(humusform == "Moor")
        RST_moor <- unique(test$RST_F)
        RST_miss <- c(RST_moor, RST_noforest)
        rm(list = c("RST_noforest", "RST_moor", "test"))

        IDs_miss <- sf.ids$ID[(is.na(sf.ids$RST_F) | sf.ids$RST_F %in% RST_miss)] # remove non-forest-rst_fs
        IDs_complete <- which(!(is.na(sf.ids$RST_F)| sf.ids$RST_F %in% RST_miss)) # IDs good

        # all IDs mapped by STOKA
        if(length(IDs_miss) == 0){
          ls.STOK <- fnc_soil_stok(df = sf.ids,
                                   df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                   PTF_to_use = PTF_to_use,
                                   dgm = df.dgm)
          names(ls.soils) <- df.ids$ID_custom

        } else {
          cat("IDs ", IDs_miss, " are not mapped by STOKA. Those can't be compared using STOK")

          ls.STOK <- vector("list", length = nrow(df.ids))
          ls.STOK[IDs_complete] <- fnc_soil_stok(df = sf.ids[!is.na(sf.ids$RST_F),],
                                                 df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                                 PTF_to_use = PTF_to_use,
                                                 dgm = df.dgm)
          ls.STOK[IDs_miss] <- sapply(IDs_miss, function(x) NULL)

          names(ls.STOK) <- df.ids$ID_custom

        }

      }

      if (any(stringr::str_detect(soiloptions_to_test, "BZE"))) {
        df.ids <- df.ids %>%
          dplyr::left_join(df.dgm, by = "ID")
        xy_proj <- fnc_transf_crs(df = df.ids,
                                  to_crs = "UTM_25832")

        ls.BZE <- fnc_soil_bze(df.utm = xy_proj,
                               df.assign = df.ids,
                               buffering = (!is.na(bze_buffer)),
                               buff_width = bze_buffer,

                               meta.out = meta.out)

      }

      if (any(stringr::str_detect(soiloptions_to_test, "OWN"))) {

        if(!all(df.ids$ID_custom == unique(df.soils$ID_custom))){
          stop("not all ID_custom of df.ids and df.soils are equal")
        }else {
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
          ls.OWN <- lapply(ls.soils, function(x){cbind(x[,1:3], "nl" = 1:nrow(x), x[4:ncol(x)])})

          names(ls.OWN) <- df.ids$ID_custom
        }

      }

      # PTF-application:  -------------------------------------- ####
      if(MvG_own_vals == T){

        # check if all necessary columns are there:
        if(!all(c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort") %in% names(df.soils))){
          missingcol <- c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort")[!c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort") %in% names(df.soils)]
          cat(missingcol, "is missing in df.soils for PTF-application of ", PTF_to_use, "\n")
          stop("missing columns")
        }

        if(any(stringr::str_detect(soiloptions_to_test, "BZE"))){
          ls.BZE[as.numeric(which(!unlist(lapply(ls.BZE, is.null))==T))] <- lapply(ls.BZE[as.numeric(which(!unlist(lapply(ls.BZE, is.null))==T))],
                                                                                     FUN = fnc_PTF, PTF_used = PTF_to_use)
        }
        if(any(stringr::str_detect(soiloptions_to_test, "STOK"))){
          ls.STOK[IDs_complete] <- lapply(ls.STOK[IDs_complete], FUN = fnc_PTF, PTF_used = PTF_to_use)
        }

      } else {
        if(any(stringr::str_detect(soiloptions_to_test, "OWN"))){
          ls.OWN <- lapply(ls.OWN, FUN = fnc_PTF, PTF_used = PTF_to_use)
        }

        if(any(stringr::str_detect(soiloptions_to_test, "BZE"))){
          ls.BZE[as.numeric(which(!unlist(lapply(ls.BZE, is.null))==T))]  <- lapply(ls.BZE[as.numeric(which(!unlist(lapply(ls.BZE, is.null))==T))] ,
                                                                                    FUN = fnc_PTF, PTF_used = PTF_to_use)
        }
        if(any(stringr::str_detect(soiloptions_to_test, "STOK"))){
          ls.STOK[IDs_complete] <- lapply(ls.STOK[IDs_complete], FUN = fnc_PTF, PTF_used = PTF_to_use)
        }
      }

      # MvG-limitation if desired: ----------------------------- ####
      if(limit_MvG){
        if(any(stringr::str_detect(soiloptions_to_test, "OWN"))){
          ls.OWN <- lapply(ls.OWN, FUN = fnc_limit)
        }
        if(any(stringr::str_detect(soiloptions_to_test, "BZE"))){
          ls.BZE[as.numeric(which(!unlist(lapply(ls.BZE, is.null))==T))]  <- lapply(ls.BZE[as.numeric(which(!unlist(lapply(ls.BZE, is.null))==T))] ,
                                                                                    FUN = fnc_limit)
        }
        if(any(stringr::str_detect(soiloptions_to_test, "STOK"))){
          ls.STOK[IDs_complete] <- lapply(ls.STOK[IDs_complete], FUN = fnc_limit)
        }
        # ls.soils <- lapply(ls.soils, FUN = fnc_limit)
      }

      # plotframe creation ------------------------------------- ####
      ls.plotframes <- list()

      for(i in 1:nrow(df.ids)){
        if(length(soiloptions_to_test) == 2){
          df.test1 <- get(paste0("ls.", soiloptions_to_test[1]))[[i]]
          df.test2 <- get(paste0("ls.", soiloptions_to_test[2]))[[i]]

          if(any(sapply(list(df.test1, df.test2), is.null))){
            ls.plotframes[[i]] <- cbind(list(df.test1, df.test2)[[which(sapply(list(df.test1, df.test2), is.null) == F)]],
                                        "option" = soiloptions_to_test[which(sapply(list(df.test1, df.test2), is.null) == F)])
          }else{
            ls.plotframes[[i]] <- rbind(cbind(df.test1,
                                              "option" = soiloptions_to_test[1]),
                                        cbind(df.test2,
                                              "option" = soiloptions_to_test[2]))

          }
        } else if (length(soiloptions_to_test) == 3){
          df.test1 <- get(paste0("ls.", soiloptions_to_test[1]))[[i]]
          df.test2 <- get(paste0("ls.", soiloptions_to_test[2]))[[i]]
          df.test3 <- get(paste0("ls.", soiloptions_to_test[3]))[[i]]

          if(any(sapply(list(df.test1, df.test2, df.test3), is.null))){
            count <- 0
            for( k in which(sapply(list(df.test1, df.test2, df.test3), is.null) == F)){
              count <- count + 1
              df.1 <- cbind(list(df.test1, df.test2, df.test3)[[k]],
                            "option" = soiloptions_to_test[k])
              if(count == 1){
                df.final <- df.1
              }else{
                df.final <- rbind(df.final, df.1)
              }

            }
            ls.plotframes[[i]] <- df.final
          }else{
            ls.plotframes[[i]] <- rbind(cbind(df.test1,
                                              "option" = soiloptions_to_test[1]),
                                        cbind(df.test2,
                                              "option" = soiloptions_to_test[2]),
                                        cbind(df.test3,
                                              "option" = soiloptions_to_test[3]) )

          }

        }

      }

      fnc_to_upper <- function(df){
        colnames(df) <- toupper(colnames(df))
        return(df)
      }

      ls.plotframes <- lapply(ls.plotframes, fnc_filter, depths = depths_to_test)
      ls.plotframes <- lapply(ls.plotframes, fnc_to_upper)
      # plotting ----------------------------------------------- ####

      df.match <- data.frame("OPTION" = c("STOK", "BZE", "OWN"),
                             "color" = c("red", "blue", "green"))
      psi_vals <- 10^(seq(log10(1), log10(100000), length.out = 500))

      for(id in 1:nrow(df.ids)){
        png(filename = paste0(output_path, df.ids$ID_custom[id], ".png"),
            width = 16, height = 9, units = "in", res = 300)

        par(mfrow = c(length(depths_to_test), 4),
            oma = c(1,1,3,1))
        #layout(matrix(c(1,2,3,4,4,5,6,7,8,8,9), 2, 5, byrow = TRUE))

        for(i in depths_to_test){
          df.test <- subset(ls.plotframes[[id]], DEPTH == i)

          soiltexture::TT.plot(class.sys = "DE.BK94.TT",
                               tri.data = df.test,
                               main = "Soil Texture",
                               tri.sum.tst = F,
                               col = as.character(df.match[match(df.test$OPTION, df.match$OPTION), "color"]),
                               cex = 2,
                               pch = 19,
                               new.mar = c(1,5,3,2),
                               cex.axis = 0.8,
                               cex.lab = 0.8,
                               cex.main = 1)

          # MvG
          par(mar = c(5.1, 4.1, 4.1, 2.1))
          thsmax <- max(df.test$THS)+0.1
          ksatmax <- max(log10(df.test$KSAT))
          plot(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                            alpha = df.test$ALPHA[[1]]/100,
                                            n = df.test$NPAR[[1]],
                                            ThS = df.test$THS[[1]],
                                            ThR = df.test$THR[[1]],
                                            m = df.test$MPAR[[1]]),  ylim =c(0,thsmax),
               col = as.character(df.match[match(df.test$OPTION[[1]], df.match$OPTION), "color"]),
               xlab = "pressure head (pF)", ylab = "soil water content",
               main = "Water Retention Curve")
          if(nrow(df.test) > 1){
            points(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                                alpha = df.test$ALPHA[[2]]/100,
                                                n = df.test$NPAR[[2]],
                                                ThS = df.test$THS[[2]],
                                                ThR = df.test$THR[[2]],
                                                m = df.test$MPAR[[2]]),
                   col = as.character(df.match[match(df.test$OPTION[[2]], df.match$OPTION), "color"]))

            if(nrow(df.test) == 3){
              points(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                                  alpha = df.test$ALPHA[[3]]/100,
                                                  n = df.test$NPAR[[3]],
                                                  ThS = df.test$THS[[3]],
                                                  ThR = df.test$THR[[3]],
                                                  m = df.test$MPAR[[3]]),
                     col = as.character(df.match[match(df.test$OPTION[[3]], df.match$OPTION), "color"]))
            }
          }

          y_pl = log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                   par.shp = list(Ks = df.test$KSAT[[1]],
                                                  ths = df.test$THS[[1]],
                                                  thr = df.test$THR[[1]],
                                                  n = df.test$NPAR[[1]],
                                                  alfa = df.test$ALPHA[[1]]/100,
                                                  tau = df.test$TORT[[1]])))

          plot(x = log10(psi_vals),  y = y_pl,
               ylim = c(min(y_pl),ksatmax),
               col = as.character(df.match[match(df.test$OPTION[[1]], df.match$OPTION), "color"]),
               xlab = "pressure head (pF)", ylab = "conductivity [log10 Ku]",
               main = "Conductivity Curve")
          if(nrow(df.test) > 1){
            points(log10(psi_vals), log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                                      par.shp = list(Ks = df.test$KSAT[[2]],
                                                                     ths = df.test$THS[[2]],
                                                                     thr = df.test$THR[[2]],
                                                                     n = df.test$NPAR[[2]],
                                                                     alfa = df.test$ALPHA[[2]]/100,
                                                                     tau = df.test$TORT[[2]]))),
                   col = as.character(df.match[match(df.test$OPTION[[2]], df.match$OPTION), "color"]))

            if(nrow(df.test) == 3){
              points(log10(psi_vals), log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                                        par.shp = list(Ks = df.test$KSAT[[3]],
                                                                       ths = df.test$THS[[3]],
                                                                       thr = df.test$THR[[3]],
                                                                       n = df.test$NPAR[[3]],
                                                                       alfa = df.test$ALPHA[[3]]/100,
                                                                       tau = df.test$TORT[[3]]))),
                     col = as.character(df.match[match(df.test$OPTION[[3]], df.match$OPTION), "color"]))
            }
          }

          plot(.5,.5,
               col = "white",
               xaxt = "n", yaxt = "n", ann = F,
               xlim = c(0,1), ylim = c(0,1),
               axes = F)
          text(0, 0.9, "Other Comparative Values: ", cex = 1.5, font = 2)
          text(-0.5, 0.6, paste0("Soil Option   |\n\n", paste(df.test$OPTION, collapse = "\n")), cex = 1.2)
          text(0, 0.6, paste0("gravel |\n\n", paste(df.test$GRAVEL, collapse = "\n")), cex = 1.2)
          text(0.3, 0.6, paste0(" bd |\n\n", paste(df.test$BD, collapse = "\n")), cex = 1.2)
          text(0.7, 0.6, paste0(" oc.pct\n\n", paste(df.test$OC.PCT, collapse = "\n")), cex = 1.2)
          text(1.2, 0.6, paste0("depth: ", i, " m"), cex = 1.5, srt = -90)

        }

        mtext(paste("comparative graphs for ID:", df.ids$ID_custom[id],
                    "\n ", paste(soiloptions_to_test, collapse = " / "),
                    " in ", paste(df.match[match(soiloptions_to_test, df.match$OPTION), "color"], collapse = " / ") ),
              outer = T)

        dev.off()
      }


    } else if (what_to_test == "PTFs"){

      # initial tasks ------------------------------------------ ####
      # sort dfs according to IDs
      df.ids$ID <- 1:nrow(df.ids)

      # transformation of ids to GK3 for slope & aspect
      xy_gk <- fnc_transf_crs(df = df.ids)

      dgm.stack <- raster::stack(list.files(input_paul, pattern = "aspect.sdat|slope.sdat", full.names=T))
      df.dgm <- cbind("ID" = df.ids$ID,
                      as.data.frame(fnc_extract_points_dgm(lay = dgm.stack, xy = xy_gk, buffering = T)))


      # initialise list
      ls.soils <- vector("list", length = nrow(df.ids))
      names(ls.soils) <- df.ids$ID_custom
      # choice of data origin:  -------------------------------- ####

      if(soiloption_to_use == "STOK"){

        # subset currently still active for faster processing - to be expanded to BW in the future
        sf.testgeb <- get(paste0("sf.STOK.", testgebiet))
        df.LEIT <- get(paste0("df.LEIT.", testgebiet))

        sf.ids <- sf::st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632) %>%
          sf::st_join(sf.testgeb) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(ID, ID_custom, RST_F)

        # no forest
        RST_noforest <- c(39272, 39273, 0, 39343, 42046)
        # swamp
        test <- df.LEIT %>% filter(humusform == "Moor")
        RST_moor <- unique(test$RST_F)
        RST_miss <- c(RST_moor, RST_noforest)
        rm(list = c("RST_noforest", "RST_moor", "test"))

        IDs_miss <- sf.ids$ID[(is.na(sf.ids$RST_F) | sf.ids$RST_F %in% RST_miss)] # remove non-forest-rst_fs
        IDs_complete <- which(!(is.na(sf.ids$RST_F)| sf.ids$RST_F %in% RST_miss)) # IDs good


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
            ls.soils <- fnc_soil_stok(df = sf.ids,
                                      df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                      PTF_to_use = PTF_to_use,
                                      dgm = df.dgm)
            names(ls.soils) <- df.ids$ID_custom[IDs_complete]

            val_IDs <- df.ids$ID_custom[IDs_complete]
          }

          if(how_to_proceed == "2"){
            #ls.soils <- list()
            ls.soils[IDs_complete] <- fnc_soil_stok(df = sf.ids[IDs_complete,],
                                                    df.LEIT = get(paste0("df.LEIT.", testgebiet)),
                                                    PTF_to_use = PTF_to_use,
                                                    dgm = df.dgm)
            df.ids <- df.ids %>%
              dplyr::left_join(df.dgm, by = "ID")
            xy_gk_miss <- fnc_transf_crs(df = df.ids[IDs_miss,],
                                         to_crs = "UTM_25832")
            ls.soils[IDs_miss] <- fnc_soil_bze(df.utm = xy_gk_miss,
                                               df.assign = df.ids[IDs_miss,],
                                               buffering = (!is.na(bze_buffer)),
                                               buff_width = bze_buffer)

            ls.soils <- ls.soils[as.numeric(which(!unlist(lapply(ls.soils, is.null))==T))]

            names(ls.soils) <- unlist(lapply(ls.soils, function(x) unique(x$ID_custom)))
            val_IDs <- names(ls.soils)


          }

        }

      } else if (soiloption_to_use == "BZE") {
        df.ids <- df.ids %>%
          dplyr::left_join(df.dgm, by = "ID")
        xy_proj <- fnc_transf_crs(df = df.ids,
                                  to_crs = "UTM_25832")
        ls.soils <- fnc_soil_bze(df.utm = xy_proj,
                                 df.assign = df.ids,
                                 buffering = (!is.na(bze_buffer)),
                                 buff_width = bze_buffer,

                                 meta.out = meta.out)

        ls.soils <- ls.soils[as.numeric(which(!unlist(lapply(ls.soils, is.null))==T))]

        names(ls.soils) <- unlist(lapply(ls.soils, function(x) unique(x$ID_custom)))
        val_IDs <- names(ls.soils)

      } else if (soiloption_to_use == "OWN") {

        if(!all(df.ids$ID_custom == unique(df.soils$ID_custom))){
          stop("not all ID_custom of df.ids and df.soils are equal")
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

          val_IDs <- df.ids$ID_custom
        }

      } else {
        stop("Please provide valid soil-option")
      }



      # PTF-application: ----------------------------------------- ####
      if(any(stringr::str_detect(PTF_to_test, "OWN_PARMS"))){

        # check if all necessary columns are there:
        if(!all(c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort") %in% names(df.soils))){
          missingcol <- c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort")[!c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort") %in% names(df.soils)]
          cat(missingcol, "is missing in df.soils for PTF-application of ", PTF_to_use, "\n")
          stop("missing columns")
        }else{
          ls.OWN_PARMS <- ls.soils

          # drop the MvG columns so they can be created by fnc_PTF
          ls.soils <- lapply(ls.soils, function(x) x[!(names(x) %in% c("ths", "thr", "alpha", "npar", "mpar", "ksat", "tort"))])

          for (ptf in PTF_to_test[which(PTF_to_test != "OWN_PARMS")]){
            assign(paste0("ls.", ptf), lapply(ls.soils, FUN = fnc_PTF, PTF_used = ptf))
          }
        }


      } else {

        for (ptf in PTF_to_test){
          assign(paste0("ls.", ptf), lapply(ls.soils, FUN = fnc_PTF, PTF_used = ptf))
        }


      }

      # MvG-limitation if desired: ------------------------------- ####
      if(limit_MvG){
        for(ptf in PTF_to_test){
          assign(paste0("ls.", ptf), lapply(get(paste0("ls.", ptf)), FUN = fnc_limit))
        }
      }

      # plotframe creation ------------------------------------- ####

      for(ptf in PTF_to_test){
        assign(paste0("ls.",which(PTF_to_test == ptf)), lapply(get(paste0("ls.", ptf)),cbind,  "OPTION"=ptf))
      }

      if(length(PTF_to_test) == 2){
        ls.plotframes <- lapply(seq_along(ls.1), function(x) rbind(ls.1[[x]], ls.2[[x]]))
      }else if(length(PTF_to_test) == 3){
        ls.plotframes <- lapply(seq_along(ls.1), function(x) rbind(ls.1[[x]], ls.2[[x]], ls.3[[x]]))
      }else if(length(PTF_to_test) == 4){
        ls.plotframes <- lapply(seq_along(ls.1), function(x) rbind(ls.1[[x]], ls.2[[x]], ls.3[[x]], ls.4[[x]]))
      }

      fnc_to_upper <- function(df){
        colnames(df) <- toupper(colnames(df))
        return(df)
      }

      ls.plotframes <- lapply(ls.plotframes, fnc_filter, depths = depths_to_test)
      ls.plotframes <- lapply(ls.plotframes, fnc_to_upper)
      # plotting ----------------------------------------------- ####

      df.match <- data.frame("OPTION" = c("HYPRES", "PTFPUH2", "WESSOLEK", "OWN_PARMS"),
                             "color" = c("red", "blue", "green", "purple"))
      psi_vals <- 10^(seq(log10(1), log10(100000), length.out = 500))

      for(id in 1:length(ls.soils)){
        png(filename = paste0(output_path, val_IDs[id], ".png"),
            width = 12, height = 7, units = "in", res = 300)

        par(oma = c(1,1,3,1))
        layout(matrix(1:(2*length(depths_to_test)), nrow=2, byrow=F))

        for(i in depths_to_test){
          df.test <- subset(ls.plotframes[[id]], DEPTH == i)

          # MvG
          thsmax <- max(df.test$THS)+0.1
          ksatmax <- max(log10(df.test$KSAT))
          plot(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                            alpha = df.test$ALPHA[[1]]/100,
                                            n = df.test$NPAR[[1]],
                                            ThS = df.test$THS[[1]],
                                            ThR = df.test$THR[[1]],
                                            m = df.test$MPAR[[1]]),  ylim =c(0,thsmax),
               col = as.character(df.match[match(df.test$OPTION[[1]], df.match$OPTION), "color"]),
               xlab = "pressure head (pF)", ylab = "soil water content",
               main = paste0("depth: ", i, " m"))
          if(nrow(df.test) > 1){
            points(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                                alpha = df.test$ALPHA[[2]]/100,
                                                n = df.test$NPAR[[2]],
                                                ThS = df.test$THS[[2]],
                                                ThR = df.test$THR[[2]],
                                                m = df.test$MPAR[[2]]),
                   col = as.character(df.match[match(df.test$OPTION[[2]], df.match$OPTION), "color"]))

            if(nrow(df.test) == 3){
              points(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                                  alpha = df.test$ALPHA[[3]]/100,
                                                  n = df.test$NPAR[[3]],
                                                  ThS = df.test$THS[[3]],
                                                  ThR = df.test$THR[[3]],
                                                  m = df.test$MPAR[[3]]),
                     col = as.character(df.match[match(df.test$OPTION[[3]], df.match$OPTION), "color"]))
            }
            if(nrow(df.test) == 4){
              points(log10(psi_vals), fnc_MvG.swc(psi = psi_vals,
                                                  alpha = df.test$ALPHA[[4]]/100,
                                                  n = df.test$NPAR[[4]],
                                                  ThS = df.test$THS[[4]],
                                                  ThR = df.test$THR[[4]],
                                                  m = df.test$MPAR[[4]]),
                     col = as.character(df.match[match(df.test$OPTION[[4]], df.match$OPTION), "color"]))
            }
          }

          y_pl = log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                   par.shp = list(Ks = df.test$KSAT[[1]],
                                                  ths = df.test$THS[[1]],
                                                  thr = df.test$THR[[1]],
                                                  n = df.test$NPAR[[1]],
                                                  alfa = df.test$ALPHA[[1]]/100,
                                                  tau = df.test$TORT[[1]])))

          plot(x = log10(psi_vals),  y = y_pl,
               ylim = c(min(y_pl),ksatmax),
               col = as.character(df.match[match(df.test$OPTION[[1]], df.match$OPTION), "color"]),
               xlab = "pressure head (pF)", ylab = "conductivity [log10 Ku]")
          if(nrow(df.test) > 1){
            points(log10(psi_vals), log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                                      par.shp = list(Ks = df.test$KSAT[[2]],
                                                                     ths = df.test$THS[[2]],
                                                                     thr = df.test$THR[[2]],
                                                                     n = df.test$NPAR[[2]],
                                                                     alfa = df.test$ALPHA[[2]]/100,
                                                                     tau = df.test$TORT[[2]]))),
                   col = as.character(df.match[match(df.test$OPTION[[2]], df.match$OPTION), "color"]))

            if(nrow(df.test) == 3){
              points(log10(psi_vals), log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                                        par.shp = list(Ks = df.test$KSAT[[3]],
                                                                       ths = df.test$THS[[3]],
                                                                       thr = df.test$THR[[3]],
                                                                       n = df.test$NPAR[[3]],
                                                                       alfa = df.test$ALPHA[[3]]/100,
                                                                       tau = df.test$TORT[[3]]))),
                     col = as.character(df.match[match(df.test$OPTION[[3]], df.match$OPTION), "color"]))
            }
            if(nrow(df.test) == 4){
              points(log10(psi_vals), log10(SoilHyP::Ku(suc = psi_vals, FUN.shp = "vGM", modality = "uni", suc.negativ = F,
                                                        par.shp = list(Ks = df.test$KSAT[[4]],
                                                                       ths = df.test$THS[[4]],
                                                                       thr = df.test$THR[[4]],
                                                                       n = df.test$NPAR[[4]],
                                                                       alfa = df.test$ALPHA[[4]]/100,
                                                                       tau = df.test$TORT[[4]]))),
                     col = as.character(df.match[match(df.test$OPTION[[4]], df.match$OPTION), "color"]))
            }
          }

        }

        mtext(paste("comparative graphs (water retention curve (above) and conductivity curve (below)) for ID: ", val_IDs[id],
                    "\n ", paste(PTF_to_test, collapse = " / "),
                    " in ", paste(df.match[match(PTF_to_test, df.match$OPTION), "color"], collapse = " / ") ),
              outer = T)

        dev.off()
      }

    } else {
      stop("Please provide valid what_to_test option.")
    }
  }
}


