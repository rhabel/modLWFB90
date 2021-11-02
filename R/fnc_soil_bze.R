#' Soil-list creation from BZE data
#'
#' This function is a wrapper of several smaller functions and chunks of code, all retrieved from "U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb". Combining all this code into one function, it takes a spatialpointsdataframe of coordinates in GK-3 and returns a list of soil data frames. Those are further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{run_multisite_LWFB90}}.
#'
#' @param df.gk A spatialpointsdataframe with the desired points in UTM25832.
#' @param df.assign a dataframe containing the corresponding ID_custom for the IDs in \code{df.gk}
#' @param meta.out a string containing a path passed down from \code{fnc_get_soil}. Saving location of metadata.
#' @param limit_bodtief max soil depth, default is \code{NA} and uses max soil depth as defined in \code{df.LEIT}. If not \code{NA} soil-dfs are created down to the depth specified here as depth in \code{m}, negative. Might be used to give room for different \code{maxrootdepth} - settings in \link{fnc_get_params}. In this case, soil depth may be reduced significantly.
#' @param ... whether buffer should be used in extracting points from BZE raster files if \code{NAs} occur, options are \code{buffering} as \code{TRUE} or \code{FALSE}, and \code{buff_width} in \code{m}
#'
#' @return Returns a list of soil data frames.
#' @import doParallel parallel foreach
#'
#' @export

fnc_soil_bze <- function(df.utm,
                         df.assign,

                         meta.out,
                         limit_bodtief = NA,
                         incl_GEOLA,
                         ...){

  input_bze <- input_bze

  # einlesen aller BZEraster:
  a <- c("lof_cm", "oh_cm")
  b <- c("bodtief",
         "corg0", "corg1", "corg2", "corg3", "corg4",
         "trdfb0", "trdfb1", "trdfb2", "trdfb3", "trdfb4",
         "grobv0", "grobv1", "grobv2", "grobv3", "grobv4",
         "s0", "s1", "s2", "s3", "s4",
         "t0", "t1", "t2", "t3", "t4",
         "u0", "u1", "u2", "u3", "u4")

  # raster
  cl <- parallel::makeCluster(parallel::detectCores())  #Cluster mit verfügbarer Anzahl von Kernen starten
  doParallel::registerDoParallel(cl)

  ls.text.alt <- foreach::foreach(i = a, .combine = cbind, .packages = "raster") %dopar% {
    rs.files <- lapply(paste0(input_bze, i, "_strt/hdr.adf"), raster)
  }

  ls.text.neu <- foreach::foreach(i = b, .combine = cbind, .packages = "raster") %dopar% {
    rs.files <- lapply(paste0(input_bze, i, ".tif"), raster)
  }

  parallel::stopCluster(cl)

  # bind together
  ls.text <- append(ls.text.alt, ls.text.neu)
  rm(ls.text.alt, ls.text.neu)

  soilraster <- raster::stack(unlist(ls.text))
  names(soilraster) <- c(a, b)


  # stechen
  soil <- fnc_extract_points_bze(lay = soilraster,
                                 xy = df.utm,
                                 meta.out = meta.out,
                                 ...)

  # aufbereiten
  #names(soil) <- c("aspect", "slope", names(soilraster)) # Reihenfolge der Listenelemente entspricht Namen der Layers im Rasterstack
  soil <- data.table::as.data.table(soil)

  data.table::setnames(soil, paste0("trdfb",0:4), paste0("trd",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("grobv",0:4), paste0("gba",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("s",0:4), paste0("sand",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("t",0:4), paste0("ton",0:4)) # constr_corg umbenennen
  data.table::setnames(soil, paste0("u",0:4), paste0("schluff",0:4)) # constr_corg umbenennen
  soil[, c("corg0","corg1","corg2","corg3","corg4") := list(corg0/100,corg1/100,corg2/100,corg3/100,corg4/100)]
  soil[, c("trd0","trd1","trd2","trd3","trd4") := list(trd0/100,trd1/100,trd2/100,trd3/100,trd4/100)]
  soil[, c("gba0","gba1","gba2","gba3","gba4") := list(gba0/1000,gba1/1000,gba2/1000,gba3/1000,gba4/1000)]
  soil[, c("sand0","sand1","sand2","sand3","sand4") := list(sand0/10,sand1/10,sand2/10,sand3/10,sand4/10)]
  soil[, c("schluff0","schluff1","schluff2","schluff3","schluff4") := list(schluff0/10,schluff1/10,schluff2/10,schluff3/10,schluff4/10)]
  soil[, c("ton0","ton1","ton2","ton3","ton4") := list(ton0/10,ton1/10,ton2/10,ton3/10,ton4/10)]
  # soil$coords_x <-  as.numeric(df.ids$easting) #Koordinaten
  # soil$coords_y <-  as.numeric(df.ids$northing)

  # discretisation according to distances in fnc_depth_disrc
  thick1 <- c(rep(5,10),rep(10,5), rep(20, 10))
  skltn1 <- data.table::data.table(upper = c(0,cumsum(thick1[1:length(thick1)-1])), lower = cumsum(thick1))

  data.table::setkey(skltn1, upper, lower)
  soilsdiscrete1 <- fnc_MakeSoil_BZE(soil, skltn1)
  # soilsdiscrete1 <- lay_long
  data.table::setkey(soilsdiscrete1, ID)
#
#   soilsdiscrete1 <- soilsdiscrete1[order(ID, i.upper),] #sortieren
#   data.table::setkey(soilsdiscrete1, ID, i.lower)
#   soilsdiscrete1[, i.upper := c(0,i.lower[1:.N-1]), by = ID]
  soilsdiscrete1[, mat := as.numeric(depth)+1] # make room for depth_0 - Humus

  soilsdiscrete1[, c("upper", "lower", "profile_top", "aspect", "slope") := list(
    i.upper/-100,
    i.lower/-100,
    profile_top/100,
    round(aspect, 1),
    round(slope, 1)
  )]
  soilsdiscrete1[, "nl" := 1:.N, by = ID]

  # join to get ID_custom
  df.assign <- as.data.table(df.assign[,-which(colnames(df.assign) %in% c("aspect", "slope"))])
  setkey(df.assign, ID)
  ls.soils.tmp <- df.assign[soilsdiscrete1]

  ls.soils.tmp <- split(ls.soils.tmp, by = "ID")
  names(ls.soils.tmp) <- unlist(lapply(ls.soils.tmp, function(x) unique(x$ID_custom)))

  # remove NA-dfs
  which.na <- which(unlist(lapply(ls.soils.tmp, function(x) any(is.na(x)))==T))
  which.non.na <- which(unlist(lapply(ls.soils.tmp, function(x) any(is.na(x)))==F))
  if(length(which.na) != 0){
    ls.soils.tmp[which.na] <- list(NULL)
    message(paste0("ID: ", names(ls.soils.tmp)[which.na], " won't be modelled. There's no BZE_R data at coordinate + set buffer width. \n"))
  }

  # limit to either Dietmar-depth, GEOLA-depth, or limit_bodtief
  if(is.na(limit_bodtief) == T){

    # incl GEOLA
    if(incl_GEOLA){

      ls.soils.tmp[which.non.na] <- lapply(ls.soils.tmp[which.non.na],
                               FUN = function(x){
                                 if(unique(x$BODENTY) == "Gleye/Auenboeden"){
                                   return(x)
                                 }else if(unique(x$BODENTY) == "Stauwasserboeden"){
                                   x <- x[i.upper < as.numeric(unique(x$roots_bottom_rnd))]
                                   return(x)
                                 }else{
                                   whichmax <- as.numeric(max(unique(x$GRUND_C), unique(x$roots_bottom_rnd)))
                                   x <- x[which(x$i.upper < whichmax),]
                                   x$lower[nrow(x)] <- whichmax/-100
                                   return(x)
                                 }
                                 }
                               )

    }else{

      ls.soils.tmp[which.non.na] <- lapply(ls.soils.tmp[which.non.na],
                             FUN = function(x){x[which(x$i.upper < as.numeric(unique(x$roots_bottom_rnd))),]})

    }

  }else{

    # remove all layers below set maxdepth
    ls.soils.tmp[which.non.na] <- mapply(FUN = function(x,limit){
                             x <- x[which(x$i.upper < limit*-100),]
                             x$lower[nrow(x)] <- limit
                             return(x)
                           },
                           ls.soils.tmp[which.non.na],
                           limit = ifelse(length(limit_bodtief) > 1,limit_bodtief[which.non.na] ,limit_bodtief),
                           SIMPLIFY = F)

  }

  if(incl_GEOLA){
    ls.soils.tmp[which.non.na] <- lapply(ls.soils.tmp[which.non.na],
                           FUN = function(x){
                             x <- as.data.frame(x)
                             x <- x[c("ID", "ID_custom", "mat", "nl", "upper", "lower",
                                      "sand", "schluff", "ton", "gba", "trd", "corg",
                                      "aspect", "slope", "profile_top", "BODENTY")]
                             colnames(x) <- c("ID", "ID_custom", "mat", "nl","upper", "lower",
                                              "sand", "silt", "clay", "gravel", "bd", "oc.pct",
                                              "aspect" ,"slope" ,"humus", "BODENTY")
                             x$ID_custom <- as.character(x$ID_custom)
                             return(x)})
  }else{
    ls.soils.tmp[which.non.na] <- lapply(ls.soils.tmp[which.non.na],
                           FUN = function(x){
                             x <- as.data.frame(x)
                             x <- x[c("ID", "ID_custom", "mat", "nl", "upper", "lower",
                                      "sand", "schluff", "ton", "gba", "trd", "corg",
                                      "aspect", "slope", "profile_top")]
                             colnames(x) <- c("ID", "ID_custom", "mat", "nl","upper", "lower",
                                              "sand", "silt", "clay", "gravel", "bd", "oc.pct",
                                              "aspect" ,"slope" ,"humus")
                             x$ID_custom <- as.character(x$ID_custom)
                             return(x)})

  }

  return(ls.soils.tmp)
}
