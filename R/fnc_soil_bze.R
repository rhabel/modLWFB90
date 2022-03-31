#' Soil-list creation from BZE data
#'
#' This function is a wrapper of several smaller functions and chunks of code, all retrieved from "U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb". Combining all this code into one function, it takes a spatialpointsdataframe of coordinates in GK-3 and returns a list of soil data frames. Those are further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{run_multisite_LWFB90}}.
#'
#' @param df.ids a dataframe with \code{ID_customs}, \code{IDs}, \code{aspect}, \code{slope}, \code{easting} and \code{northing}, as created one function up by \code{\link{fnc_get_soil}} May contain further columns
#' @param meta.out a string containing a path passed down from \code{fnc_get_soil}. Saving location of metadata.
#' @param limit_bodtief max soil depth, default is \code{NA} and uses max soil depth as defined in \code{df.LEIT}. If not \code{NA} soil-dfs are created down to the depth specified here as depth in \code{m}, negative. Might be used to give room for different \code{maxrootdepth} - settings in \link{fnc_get_params}. In this case, soil depth may be reduced significantly.
#' @param ... whether buffer should be used in extracting points from BZE raster files if \code{NAs} occur, options are \code{buffering} as \code{TRUE} or \code{FALSE}, and \code{buff_width} in \code{m}
#'
#' @return Returns a list of soil data frames.
#' @import terra sf dplyr data.table
#'
#' @export

fnc_soil_bze <- function(df.ids,

                         meta.out,
                         reduce_to_forest = F,
                         limit_bodtief = NA,
                         incl_GEOLA,
                         buffering = F,
                         buff_width = NA){

  data(paths)

  if(reduce_to_forest){
    # transform to spatVector

    xy <- sf::st_as_sf(df.ids,
                       coords = c("easting", "northing"), crs = 32632) %>%
      sf::st_transform(25832)

    xy_spat <- terra::vect(xy)

    # filter whether wald or not
    rast_wald <- terra::rast(paste0(path_wald, "wald_yn.tif"))
    extr_vals_direkt <- terra::extract(rast_wald, xy_spat) %>% dplyr::filter(wald_yn == 1) %>% dplyr::pull(ID)

    # tolerance of 12m - half a cell
    xy_spat2 <- xy %>% dplyr::filter(!ID %in% extr_vals_direkt) %>%
      sf::st_buffer(., 12) %>%
      terra::vect(.)
    extr_vals_buff <- terra::extract(rast_wald, xy_spat2, touches = T)
    extr_vals_buff$ID <- xy_spat2$ID[extr_vals_buff$ID]
    extr_vals_buff <- extr_vals_buff %>%
      dplyr::filter(wald_yn == 1) %>% dplyr::distinct(ID) %>%  dplyr::pull(ID)

    # forest or non-forest?
    im_wald <- sort(c(extr_vals_direkt, extr_vals_buff))
    nicht_im_wald <- df.ids$ID[!df.ids$ID %in% im_wald]

    message(paste0("ID: ", df.ids$ID_custom[nicht_im_wald], " are not located in the forest. They will not be modelled... \n"))

    # filter on those which are in the forest:
    df.ids <- df.ids %>%
      dplyr::filter(ID %in% im_wald)

    xy <- sf::st_as_sf(df.ids,
                       coords = c("easting", "northing"), crs = 32632) %>%
      sf::st_transform(25832)

    xy_spat <- terra::vect(xy)
  }

  # transform to spatVector
  xy <- sf::st_as_sf(df.ids,
                     coords = c("easting", "northing"), crs = 32632) %>%
    sf::st_transform(25832)

  xy_spat <- terra::vect(xy)

  # exclude peat areas:
  moore <- df.ids$ID[df.ids$BODENTY == "Moor"][!is.na(df.ids$ID[df.ids$BODENTY == "Moor"])]
  if(length(moore) > 0){
    message(paste0("ID: ", df.ids$ID_custom[moore], " lie within peatlands (Moore). They will not be modelled... \n"))
  }

  # filter on those which are in the forest:
  df.ids <- df.ids %>%
    dplyr::filter(!ID %in% moore)

  xy <- sf::st_as_sf(df.ids,
                     coords = c("easting", "northing"), crs = 32632) %>%
    sf::st_transform(25832)

  xy_spat <- terra::vect(xy)

  # einlesen aller BZEraster:
  a <- c("lof_cm", "oh_cm")
  b <- c("bodtief",
         "corg0", "corg1", "corg2", "corg3", "corg4",
         "trdfb0", "trdfb1", "trdfb2", "trdfb3", "trdfb4",
         "grobv0", "grobv1", "grobv2", "grobv3", "grobv4",
         "s0", "s1", "s2", "s3", "s4",
         "t0", "t1", "t2", "t3", "t4",
         "u0", "u1", "u2", "u3", "u4")

  bze_alt <- terra::rast(paste0(path_BZEreg, a, "_strt/hdr.adf"))
  #extr_vals_alt <- terra::extract(bze_alt, xy_spat)
  #colnames(extr_vals_alt) <- c("ID", a)

  bze_neu <- terra::rast(paste0(path_BZEreg, b, ".tif"))
  #extr_vals_neu <- terra::extract(bze_neu, xy_spat, factors = F)
  #colnames(extr_vals_neu) <- c("ID", b)

  bze_complete <- c(bze_alt, bze_neu)
  names(bze_complete)[1:2] <- a
  extr_vals <- as.data.frame(terra::extract(bze_complete, xy_spat, factors = F))
  extr_vals$ID <- xy_spat$ID[extr_vals$ID]

  extr_vals[extr_vals == -9999] <- NA_integer_
  extr_vals[extr_vals == "NaN"] <- NA_integer_

  val_miss <- extr_vals[!complete.cases(extr_vals),]

  #NAs & -9999: ziehe die häufigsten Werte im Umkreis von 50 m
  if(nrow(val_miss) > 0 & buffering == T ){
    which_missing <- val_miss$ID

    # creat sf and spatvector of missing points
    sf_miss <- xy[xy$ID %in% which_missing,]
    sf_miss_spat <- terra::vect(sf_miss)

    # for terra::extract: buffer with buff_width
    # buff_width = 1000
    sf_buffer <- sf::st_buffer(sf_miss, buff_width)

    # buff_data contains values from cells within buffer
    buff_data <- terra::extract(bze_complete, terra::vect(sf_buffer), factors = F,
                                weights = T, cells = T)

    # all data complete through buffering
    buff_data_complete <- as.data.frame(buff_data[complete.cases(buff_data),])
    buff_data_complete$complete <- "yes"

    # missing organic layer only shall not exclude otherwise successfull buffer process
    buff_data[,c(2,3)][is.nan(buff_data[,c(2,3)])] <- 0
    buff_data <- as.data.frame(buff_data[complete.cases(buff_data),])

    buff_data_incomplete <- left_join(buff_data, buff_data_complete) %>%
      group_by(ID) %>%
      filter(all(is.na(complete)))

    buff_data <- rbind(buff_data_complete, buff_data_incomplete) %>% dplyr::arrange(ID) %>% dplyr::select(-complete)

    if(nrow(buff_data) >0 ){
      buff_data$ID <- which_missing[buff_data$ID] # keep ID from above

      # create spatvectors from cellcentres within buffer to perform distance calculation
      buff_cells <- buff_data[,c("ID", "cell")]
      buff_cells <- cbind(buff_cells, as.data.frame(terra::xyFromCell(bze_neu[[1]], buff_data$cell)))
      buff_cells <- split(buff_cells, f = buff_cells$ID)
      buff_cells <- lapply(buff_cells,
                           function(x){terra::vect(x, geom = c("x", "y"), crs = "EPSG:25832")})  # creates a list of all buffercells from missing points

      # remove those points that have no data within buffer
      if(nrow(sf_miss_spat) > 1){
        spat_list <- split(sf_miss_spat, seq(nrow(sf_miss_spat)))    # creates a list of spatvectors from missing points for distance calculation with mapply
        names(spat_list) <- unlist(lapply(spat_list, function(x){terra::values(x)$ID}))
      }else{
        spat_list <- list(sf_miss_spat)
        names(spat_list) <- terra::values(sf_miss_spat)$ID
      }
      spat_list <- spat_list[names(spat_list) %in% names(buff_cells)]


      # perform distance calculation and rank
      buff_cells <- mapply(FUN = function(buffcells, mod_points){
        cbind(terra::values(buffcells),
              "distance" = terra::distance(buffcells, mod_points))},
        buffcells = buff_cells,
        mod_points = spat_list,
        SIMPLIFY = F)
      buff_cells_final <- unlist(lapply(buff_cells, function(x){

        x <- data.table::as.data.table(x)
        x <- data.table::setorder(x, distance)
        x <- x[1,]$cell
      }))

      buff_cells_final <- data.frame("ID" = as.numeric(names(buff_cells_final)),
                                     "cell" = buff_cells_final,
                                     row.names = NULL)
      # buff_data <- setNames(buff_data,
      #          c("ID", paste0("category_", 1:((ncol(buff_data))-3)), "cell", "weight" ))
      buff_data <- inner_join(buff_data, buff_cells_final, by = c("ID", "cell"))
      buff_data <- buff_data[,-which(colnames(buff_data) %in% c( "cell", "weight"))]

      # filter buffer_data for selected cells
      #buff_data <- buff_data[buff_data$cell %in% buff_cells_final,-c(ncol(buff_data)-1,ncol(buff_data))]
      #names(buff_data) <- c("ID", b)
      # buff_data <- cbind("ID" = buff_data$ID,
      #                    lof_cm = rep(0, nrow(buff_data)),
      #                    oh_cm = rep(0, nrow(buff_data)),
      #                    buff_data[,-1])

      succ_buffered <- buff_data$ID

      # include buffered data into dataframe of extracted values
      extr_vals <- extr_vals[-which(extr_vals$ID %in% succ_buffered),]
      extr_vals <- rbind(extr_vals, buff_data)
      extr_vals <- extr_vals[order(extr_vals$ID),]
      rownames(extr_vals) <- NULL

      if(is.na(meta.out) == F){

        buff_cell_meta <- lapply(buff_cells, function(x){

          x <- data.table::as.data.table(x)
          x <- setorder(x, distance)
          x[,distance := round(distance,0)]
          x <- x[1,]
        })
        names(buff_cell_meta) <- NULL
        buff_cell_meta <- as.data.frame(do.call(rbind, buff_cell_meta))

        meta_data <- df.ids[c("ID_custom", "ID","easting", "northing" )] %>%
          dplyr::mutate(buffered = ifelse(ID %in% which_missing, T, F)) %>%
          dplyr::left_join(buff_cell_meta, by = "ID") %>%
          dplyr::mutate(buffer_success = dplyr::case_when(buffered == F ~ NA,
                                                          buffered == T & is.na(distance) == F ~ T,
                                                          T ~ F)) %>%
          dplyr::select(ID_custom, easting, northing, buffered, buffer_success, x, y, distance) %>%
          setNames(c("ID_custom", "easting", "northing", "buffered", "buffer_success", "x_buffcell","y_buffcell", "distance_m"))

        write_csv(meta_data, file = meta.out)
      }

    }else{

      if(is.na(meta.out) == F){

        meta_data <- df.ids[c("ID_custom", "ID","easting", "northing" )] %>%
          dplyr::mutate(buffered = ifelse(ID %in% which_missing, T, F)) %>%
          dplyr::mutate(buffer_success = dplyr::case_when(buffered == F ~ NA,
                                                          T ~ F)) %>%
          dplyr::select(ID_custom, easting, northing, buffered, buffer_success) %>%
          setNames(c("ID_custom", "easting", "northing", "buffered", "buffer_success"))

        write_csv(meta_data, file = meta.out)
      }
    }

  }

  soil <- dplyr::left_join(df.ids[,c("ID", "aspect", "slope")], extr_vals, by = "ID")

  if( "sf" %in% class(xy)){
    soil <- soil %>% sf::st_drop_geometry()
  }
  soil <- data.table::as.data.table(soil)



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
  df.ids <- as.data.table(df.ids[,-which(colnames(df.ids) %in% c("aspect", "slope"))])
  setkey(df.ids, ID)
  ls.soils.tmp <- df.ids[soilsdiscrete1]

  ls.soils.tmp <- split(ls.soils.tmp, by = "ID")
  names(ls.soils.tmp) <- unlist(lapply(ls.soils.tmp, function(x) unique(x$ID_custom)))

  # remove NA-dfs
  which.na <- which(unlist(lapply(ls.soils.tmp, function(x) any(is.na(x[,.SD,.SDcols = !which(colnames(x) %in% c("GRUND_C", "BODENTY"))] )==T))))
  which.non.na <- which(!df.ids$ID_custom %in% names(which.na))
  if(length(which.na) != 0){
    message(paste0("ID: ", names(ls.soils.tmp)[which.na], " won't be modelled. There's no complete BZE_reg data at coordinate + set buffer width. \n"))
    ls.soils.tmp[which.na] <- NULL
  }

  # limit to either Dietmar-depth, GEOLA-depth, or limit_bodtief
  if(is.na(limit_bodtief) == T){

    # incl GEOLA
    if(incl_GEOLA){

     ls.soils.tmp <- lapply(ls.soils.tmp, function(x){
       x <- as.data.frame(x)
        if(!is.na(unique(x$BODENTY)) & unique(x$BODENTY) == "Gleye/Auenboeden"){
          x$dpth_ini <- as.numeric(unique(x$roots_bottom_rnd))
        }else if(!is.na(unique(x$BODENTY)) & unique(x$BODENTY) == "Stauwasserboeden"){
          x <- x[x$i.upper < as.numeric(unique(x$roots_bottom_rnd)),]
          x$dpth_ini <- as.numeric(unique(x$roots_bottom_rnd))
        }else{
          whichmax <- as.numeric(max(unique(x$GRUND_C), unique(x$roots_bottom_rnd), na.rm = T))
          x <- x[which(x$i.upper < whichmax),]
          x$lower[nrow(x)] <- whichmax/-100
          x$dpth_ini <- whichmax
        }
       return(x)
      })

    }else{

      ls.soils.tmp <- lapply(ls.soils.tmp, function(x){

        x[which(x$i.upper < as.numeric(unique(x$roots_bottom_rnd))),]
        x$dpth_ini <- as.numeric(unique(x$roots_bottom_rnd))
        x$BODENTY <- "unknown"
        return(x)
      })
    }

  }else{

    # remove all layers below set maxdepth
    ls.soils.tmp <- mapply(FUN = function(x,limit){

      x$dpth_ini <- as.numeric(unique(x$roots_bottom_rnd))
      x$BODENTY <- "unknown"
      x <- x[which(x$i.upper < limit*-100),]
      x$lower[nrow(x)] <- limit
      return(x)
    },
    ls.soils.tmp,
    limit = ifelse(length(limit_bodtief) > 1,limit_bodtief[which.non.na],limit_bodtief),
    SIMPLIFY = F)



  }

  # sort and rename
  ls.soils.tmp <- lapply(ls.soils.tmp, function(x){

    x <- x[,c("ID", "ID_custom", "mat", "nl", "upper", "lower",
             "sand", "schluff", "ton", "gba", "trd", "corg",
             "aspect", "slope", "profile_top", "BODENTY", "dpth_ini")]
    colnames(x) <- c("ID", "ID_custom", "mat", "nl","upper", "lower",
                     "sand", "silt", "clay", "gravel", "bd", "oc.pct",
                     "aspect" ,"slope" ,"humus", "BODENTYP", "dpth_ini")
    x$ID_custom <- as.character(x$ID_custom)
    return(x)
  })

  return(ls.soils.tmp)
}
