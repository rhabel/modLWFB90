#' Central Application
#'
#' Work in progress-version of a function that makes the application extremely easy for final users.
#'
#'
#' @param df.ids a data frame containing the name of the ids \code{ID}, as well as the UTM-32632-Coordinates in columns named \code{easting} and  \code{northing}.
#' @param how_many_points_at_once depending on the user's RAM, the function runs 1000 points at once by default (proved useful on a 8GB RAM machine). Can be changed manually if more RAM is available.
#' @param start_date start date as \code{Date}-object
#' @param end_date end date as \code{Date}-object
#' @param treespecies name of the tree species to be modelled with. Either a single species name that is then used for all points in \code{df.ids}, or a vector of the same length as \code{df.ids$ID} if the main tree species of each point is known and differs. Must be one of \code{beech}, \code{oak}, \code{spruce}, \code{pine}, \code{larch}, \code{douglasfir}.
#' @param layercols a sting containing the desired output products. Full list of possible output columns can be find on the help page of \code{\link[LWFBrook90R]{runLWFB90}} under \code{Layer outputs}
#' @param dailycols same as layercols but for daily output. For possible options see \code{\link[LWFBrook90R]{runLWFB90}}
#' @param db_name name and file path of the SQL-database
#' @param meta \code{NA} by default, a location where meta-information can be written. Will then be written into the database
#' @param ... all additional information that can be set in \code{\link{fnc_get_soil}}. See help page for all options that can be selected
#'
#'
#' @return An SQLite Database with the tables soil, and daily and/or layer outputs.
#'
#' @import LWFBrook90R
#' @export
fnc_app <- function(df.ids,
                    how_many_points_at_once = 1000,

                    start_date,
                    end_date,
                    treespecies,

                    wanted_daily = NA,
                    wanted_layer = NA,
                    db_name,
                    meta = NA,
                    ...
                    ){

  x <- 1:nrow(df.ids)
  df.ids.test <- split(df.ids, ceiling(x/how_many_points_at_once))

  # set options
  options.b90 <- setoptions_LWFB90(budburst.method = "Menzel",
                                   leaffall.method = "vonWilpert",
                                   startdate = start_date,
                                   enddate = end_date,
                                   root.method = "soilvar")

  for(i in 1:length(df.ids.test)){

    # create data
    ls.parms <- fnc_get_params(df.ids = df.ids.test[[i]],
                               tree_species = treespecies)

    ls.clim <- fnc_get_clim(df.ids = df.ids.test[[i]],
                            mindate = options.b90$startdate,
                            maxdate = options.b90$enddate)
    ls.soil <- fnc_get_soil(df.ids = df.ids.test[[i]],
                            ...)
#
#                             soil_option = "STOK",
#                             testgebiet = "BDS",
#                             PTF_to_use = "WESSOLEK",
#                             humus_roots = T,
#                             meta.out = meta)

    ls.clim <- ls.clim[which(!unlist(lapply(ls.soil, is.null))==T)]
    ls.parms <- ls.parms[which(!unlist(lapply(ls.soil, is.null))==T)]
    ls.soil <- ls.soil[which(!unlist(lapply(ls.soil, is.null))==T)]


    LWFBrook90R::msiterunLWFB90(options.b90 = options.b90,
                                param.b90 = ls.parms,
                                climate = ls.clim,
                                soil = ls.soil,

                                output = -1,
                                verbose = F,
                                output.log = F,
                                all_combinations = F,
                                cores = ifelse(nrow(df.ids.test[[i]]) > parallel::detectCores(),
                                                       parallel::detectCores()-1,
                                                       nrow(df.ids.test[[i]])),
                                rtrn.output = F,

                                output_fun = fnc_reduce,
                                dailycols = c("yr", "mo", "da", wanted.daily),
                                layercols = c("yr", "mo", "da", "nl", wanted.layer),
                                db_name = db_name)

    if(!is.na(meta)){
      meta <- read_csv(meta)
      con <- RSQLite::dbConnect(RSQLite::SQLite(),
                                dbname = db_name)
      RSQLite::dbWriteTable(con,
                            "meta",
                            meta,
                            append = T, overwrite = F, row.names=F)
      RSQLite::dbDisconnect(con)
    }


    rm(list = c("ls.clim", "ls.parms", "ls.soil"))
    gc()

  }

}
