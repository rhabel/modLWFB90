#' Function to write temporary Outputfiles to SQLite-DB
#'
#' Parallel writing is not supported for SQLite. To end with a SQLite-DB \code{\link{fnc_write}} and \code{\link{fnc_write_agg}} now create temporary .rds files that this function reads, writes to an SQLite-Database, and then deletes. \cr For every folder in \code{dir_tmp}, one table will be added to the SQL-Database, with tables named after folder names in \code{dir_tmp}. This procedure has been selected due to the default settings of \code{\link{fnc_write_agg}} and \code{\link{fnc_write}}, which create one folder for each aggregation. For example, meta files and soil-dfs can be stored accordingly (see example).
#'
#' @param dir_tmp path to temporary files
#' @param db_name name and file path of the SQL-database
#' @param del_tmp optional, shall tmp-files be deleted. Default is T
#'
#' @return writes the output to a database
#' @example inst/examples/fnc_write_to_sql_ex.R
#'
#' @import RSQLite
#' @export

fnc_write_to_sql <- function(dir_tmp,
                             db_name,
                             del_tmp = T){


  # load and write function
  load_and_write <- function(x){
    filename <- readRDS(x)
    RSQLite::dbWriteTable(con,
                          i,
                          filename,
                          append = T,
                          overwrite = F,
                          row.names = F)
  }

  # which tables exist?
  table.names <- dir(dir_tmp)

  # take each one and write to db
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbname = db_name)
  # i <- table.names[3]
  for (i in table.names){
    files_to_write <- list.files(paste0(dir_tmp, i),
                                 full.names = T)

    lapply(files_to_write,
           FUN = load_and_write)
  }

  # dbListTables(con)
  # dbListFields(con, "vegper")
  # test <- dbGetQuery(con, "SELECT * FROM vegper ")

  RSQLite::dbDisconnect(con)

  if(del_tmp){
    unlink(paste0(dir_tmp, "*"), recursive = T, force = T)
    unlink(dir_tmp, recursive = T, force = T)
  }
}
