#' Function to reduce data output from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{runLWFB90}} and \code{\link[LWFBrook90R]{msiterunLWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it to a database. This is what this function is made for.
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{runLWFB90}} or  \code{\link[LWFBrook90R]{msiterunLWFB90}}, which is further processed internally. Can't be adjusted.
#' @param layercols a sting containing the desired output products. Full list of possible output columns can be find on the help page of \code{\link[LWFBrook90R]{runLWFB90}} under \code{Layer outputs}
#' @param dailycols same as layercols but for daily output. For possible options see \code{\link[LWFBrook90R]{runLWFB90}}
#' @param db_name name and file path of the SQL-database
#'
#' @return Returns the desired output to the database directly.
#'
#' @import data.table dplyr RSQLite


fnc_reduce <- function(x, dailycols, layercols, db_name){
  # soil
  soil.df <- get("soil", envir = parent.frame(3))
  id_run <- get("soil", envir = parent.frame(3))$id_custom[1]

  if(!any(is.na(dailycols))){
    # daily
    daily <- as.data.frame(x$daily_output[,..dailycols])
    daily <- cbind("ID_custom" = rep(id_run, nrow(daily)),
                   daily)
  }

  # layer
  if(!any(is.na(layercols))){
    layer <- x$layer_output[,..layercols]
    soil.cmp <- data.table::as.data.table(soil.df[c("nl", "upper", "lower")])
    #setkeys for joiningdata.table
    data.table::setkey(layer,nl)
    data.table::setkey(soil.cmp,nl)
    # join
    layer <- layer[soil.cmp]
    layer <- data.table::setorder(layer, yr, mo, da, nl)
    layer <- as.data.frame(layer)
    layer <- cbind("ID_custom" = rep(id_run, nrow(layer)),
                   layer)
    layer <- dplyr::select(layer,
                           "ID_custom", "yr", "mo", "da", "nl", "upper", "lower", dplyr::everything() )
  }

  # write to db
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_name)
  rest <- RSQLite::dbSendQuery(con, "PRAGMA busy_timeout=5000;")
  RSQLite::dbClearResult(rest)

  on.exit(RSQLite::dbDisconnect(con))

  repeat {
    rv <- try({
      RSQLite::dbWriteTable(con,
                            "soils",
                            soil.df[,-1],
                            append=T, overwrite = F, row.names=F)
      if(!any(is.na(dailycols))){
        RSQLite::dbWriteTable(con,
                              "daily",
                              daily,
                              append=T, overwrite = F, row.names=F)
      }

      if(!any(is.na(dailycols))){
        RSQLite::dbWriteTable(con,
                              "layer",
                              layer,
                              append=T, overwrite = F, row.names=F)
      }

    })
    if(!is(rv, "try-error")) break
  }
  rv
}
