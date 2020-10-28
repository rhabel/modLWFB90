#' Soil-list creation from BZE data
#'
#' This function is a wrapper of several smaller functions and chunks of code, all retrieved from "U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb". Combining all this code into one function, it takes a spatialpointsdataframe of coordinates in GK-3 and returns a list of soil data frames. Those are further processed in \code{\link{fnc_create_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{msiterunLWFB90}}.
#'
#' @param df.gk A spatialpointsdataframe with the desired points in GK-3.
#'
#' @return Returns a list of soil data frames.
#' @export

fnc_soil_bze <- function(df.gk){
  # einlesen aller BZEraster:
  raster::rasterOptions(tmpdir = getwd())
  grid.files <- list.files(input_paul, pattern = ".sdat",full.names=T)
  soilraster <- raster::stack(grid.files)

  # stechen
  soil <- fnc_extract_points(lay = soilraster, xy = df.gk)
  # aufbereiten
  names(soil) <- names(soilraster) # Reihenfolge der Listenelemente entspricht Namen der Layers im Rasterstack
  soil <- data.table::as.data.table(soil)
  soil$ID <- df.gk$ID # Reihenfoge der Werte entspricht der Reihenfolge der IDs in xy
  soil[soil ==-9999] <- NA
  data.table::setnames(soil, paste0("constr_corg",0:4), paste0("corg",0:4)) # constr_corg umbenennen
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


  ls.soils <- tibble::as_tibble(soilsdiscrete1) %>%
    dplyr::mutate(gba = gba*100,
                  i.upper = i.upper/-100,
                  i.lower = i.lower/-100,
                  profile_top = profile_top/100) %>%
    dplyr::select(ID, depth, i.upper, i.lower, sand, schluff, ton, gba, trd, corg, aspect, slope, profile_top ) %>%
    setNames(c("ID", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct", "aspect" ,"slope" ,"humus")) %>%
    dplyr::group_split(ID)

  return(ls.soils)
}
