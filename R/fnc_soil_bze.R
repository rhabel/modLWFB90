#' Soil-list creation from BZE data
#'
#' This function is a wrapper of several smaller functions and chunks of code, all retrieved from "U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb". Combining all this code into one function, it takes a spatialpointsdataframe of coordinates in GK-3 and returns a list of soil data frames. Those are further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{msiterunLWFB90}}.
#'
#' @param df.gk A spatialpointsdataframe with the desired points in UTM25832.
#' @param df.assign a dataframe containing the corresponding ID_custom for the IDs in \code{df.gk}
#' @param meta.out a string containing a path passed down from \code{fnc_get_soil}. Saving location of metadata.
#' @param ... whether buffer should be used in extracting points from BZE raster files if \code{NAs} occur, options are \code{buffering} as \code{TRUE} or \code{FALSE}, and \code{buff_width} in \code{m}
#'
#' @return Returns a list of soil data frames.
#' @import doParallel parallel foreach
#'
#' @export

fnc_soil_bze <- function(df.utm,
                         df.assign,

                         meta.out,
                         ...){

  load("./data/paths.rda")

  # einlesen aller BZEraster:
  a <- c("bodtief",
         "corg0", "corg1", "corg2", "corg3", "corg4",
         "lof_cm", "oh_cm",
         "trdfb0", "trdfb1", "trdfb2", "trdfb3", "trdfb4",
         "grobv0", "grobv1", "grobv2", "grobv3", "grobv4",
         "s0", "s1", "s2", "s3", "s4",
         "t0", "t1", "t2", "t3", "t4",
         "u0", "u1", "u2", "u3", "u4")

  cl <- parallel::makeCluster(parallel::detectCores())  #Cluster mit verfügbarer Anzahl von Kernen starten
  doParallel::registerDoParallel(cl)
  ls.text <- foreach::foreach(i = a, .combine = cbind, .packages = "raster") %dopar% {
    rs.files <- lapply(paste0(input_bze, i, "_strt/hdr.adf"), raster)
  }
  parallel::stopCluster(cl)

  soilraster <- raster::stack(unlist(ls.text))
  names(soilraster) <- a

  # stechen
  soil <- fnc_extract_points_bze(lay = soilraster,
                                 xy = df.utm,
                                 meta.out = meta.out,
                                 ...)

  # aufbereiten
  #names(soil) <- c("aspect", "slope", names(soilraster)) # Reihenfolge der Listenelemente entspricht Namen der Layers im Rasterstack
  soil <- data.table::as.data.table(soil)
  soil$ID <- df.utm$ID # Reihenfoge der Werte entspricht der Reihenfolge der IDs in xy
  soil[soil ==-9999] <- NA

  data.table::setnames(soil, paste0("trdfb",0:4), paste0("trd",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("grobv",0:4), paste0("gba",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("s",0:4), paste0("sand",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("t",0:4), paste0("schluff",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("u",0:4), paste0("ton",0:4)) # constr_corg umbenennen
  soil[, c("corg0","corg1","corg2","corg3","corg4") := list(corg0/100,corg1/100,corg2/100,corg3/100,corg4/100)]
  soil[, c("trd0","trd1","trd2","trd3","trd4") := list(trd0/100,trd1/100,trd2/100,trd3/100,trd4/100)]
  soil[, c("gba0","gba1","gba2","gba3","gba4") := list(gba0/1000,gba1/1000,gba2/1000,gba3/1000,gba4/1000)]
  soil[, c("sand0","sand1","sand2","sand3","sand4") := list(sand0/10,sand1/10,sand2/10,sand3/10,sand4/10)]
  soil[, c("schluff0","schluff1","schluff2","schluff3","schluff4") := list(schluff0/10,schluff1/10,schluff2/10,schluff3/10,schluff4/10)]
  soil[, c("ton0","ton1","ton2","ton3","ton4") := list(ton0/10,ton1/10,ton2/10,ton3/10,ton4/10)]
  # soil$coords_x <-  as.numeric(df.ids$easting) #Koordinaten
  # soil$coords_y <-  as.numeric(df.ids$northing)

  # discretisation according to distances in fnc_depth_disrc
  thick1 <- c(rep(5,10),rep(10,5), rep(20, 5))
  skltn1 <- data.table::data.table(upper = c(0,cumsum(thick1[1:length(thick1)-1])), lower = cumsum(thick1))

  data.table::setkey(skltn1, upper, lower)
  soilsdiscrete1 <- fnc_MakeSoil_BZE(soil, skltn1)
  data.table::setkey(soilsdiscrete1, ID)

  soilsdiscrete1 <- soilsdiscrete1[order(ID, i.upper),] #sortieren
  data.table::setkey(soilsdiscrete1, ID, i.lower)
  soilsdiscrete1[, i.upper := c(0,i.lower[1:.N-1]), by = ID]
  soilsdiscrete1[, depth := as.numeric(depth)+1] # make room for depth_0 - Humus


  ls.soils.tmp <- tibble::as_tibble(soilsdiscrete1) %>%
    dplyr::mutate(gba = gba*100,
                  i.upper = i.upper/-100,
                  i.lower = i.lower/-100,
                  profile_top = profile_top/100,
                  gba = gba / 100) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(i.lower = case_when((i.lower == min(i.lower)) & (i.lower != max(lower)*-0.01) ~ max(lower)*-0.01, T~i.lower   )) %>%
    dplyr::filter(i.lower >= roots_bottom_rnd*-0.01) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(df.assign[c("ID", "ID_custom")], by = "ID") %>%
    dplyr::mutate(ID_custom = as.character(ID_custom)) %>%
    dplyr::select(ID, ID_custom, depth, i.upper, i.lower, sand, schluff, ton, gba, trd, corg, aspect, slope, profile_top ) %>%
    setNames(c("ID", "ID_custom", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct", "aspect" ,"slope" ,"humus")) %>%
    dplyr::group_split(ID)

  # names correct...
  ls.soils.tmp <- lapply(ls.soils.tmp, as.data.frame, stringsAsFactors = F)
  ls.soils.tmp <- lapply(ls.soils.tmp, function(x){cbind(x[,1:3], "nl" = 1:nrow(x), x[4:ncol(x)])})
  which.na <- which(unlist(lapply(ls.soils.tmp, function(x) any(is.na(x)))==T))
  names(ls.soils.tmp) <- unlist(lapply(ls.soils.tmp, function(x) unique(x$ID_custom)))
  ls.soils.tmp[which.na] <- list(NULL)
  if(length(which.na) != 0){
    message(paste0("ID: ", names(ls.soils.tmp)[which.na], " won't be modelled. There's no BZE_R data at coordinate + set buffer width. \n"))
  }


  return(ls.soils.tmp)
}
