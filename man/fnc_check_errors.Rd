% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_check_errors.R
\name{fnc_check_errors}
\alias{fnc_check_errors}
\title{Save error messages and simulation time}
\usage{
fnc_check_errors(res, meta = NA)
}
\arguments{
\item{res}{results of a \code{run_multisite_LWFB90} execution, saved by an assignment operator, see example}

\item{meta}{a data frame with the following columns
\itemize{
\item \code{ID_custom} - unique matching the IDs of df.ids
\item \code{modelled} - status whether modelled or not. All \code{0} in the beginning. Will be changed to \code{1} if successful
\item \code{err.code} - potential error codes will be stored here. Recommended to be set to \code{no error}
\item \code{sim_dur_s} - an empty column of /code{NA_integer_}
}
Will be created if not provided.}
}
\value{
returns a the meta-dataframe with error messages and simulation times added
}
\description{
When running multiple points with \code{\link[LWFBrook90]{run_multisite_LWFB90}} and storing the results in an sqlite-database with \code{\link{fnc_write_to_sql}}, you lose the information on how long each simulation took (which is an indicator for flawed input data), and which ones failed and why. This function helps storing this information.
}
\examples{
df.ids <- test_ids_bze

meta <- data.frame("ID_custom" = df.ids$ID_custom,
                   modelled = 0,
                   err.code = "no error",
                   sim_dur_s = NA_integer_)

ls.soil <- fnc_get_soil(df.ids,
                        soil_option = "BZE",
                        PTF_to_use = "WESSOLEK")

ls.parms <- fnc_get_params(df.ids = df.ids,
                           tree_species = "spruce")

ls.clim <- fnc_get_clim(df.ids = df.ids,
                        mindate = as.Date("2002-01-01"),
                        maxdate = as.Date("2003-12-31"))

# keep info which points have no soil info...
no_soil_data <- which(unlist(lapply(ls.soil, is.null))==T)
with_soil_data <- which(unlist(lapply(ls.soil, is.null))==F)
meta[no_soil_data, "err.code"] <- "no soil data"

# ...but remove for modelling...
ls.parms <- ls.parms[with_soil_data]
ls.soil <- ls.soil[with_soil_data]
ls.clim <- ls.clim[with_soil_data]

res <- run_multisite_LWFB90(options_b90 = set_optionsLWFB90(),

                            param_b90 = ls.parms,
                            soil = ls.soil,
                            climate = ls.clim,

                            output = -1,

                            cores = parallel::detectCores()-1,

                            rtrn_output = F,
                            rtrn_input = F,

                            timelimit = 15,

                            output_fun = fnc_write,
                            dailycols = c("rfal", "tran"),
                            dir_name= "./tmp/")

meta <- fnc_check_errors(res = res, meta = meta)

# can be written to sql as follows (see fnc_write_to_sql)
if(!dir.exists("./tmp/meta/")){
  dir.create("./tmp/meta/", recursive = T)
}

saveRDS(meta, file = "./tmp/meta/meta.rds")

# write to db
fnc_write_to_sql(db_name = "./tmp/testdb.sqlite",
                 dir_tmp = "./tmp/")

# check results
con <- dbConnect(drv = RSQLite::SQLite(), dbname =  "./tmp/testdb.sqlite")
dbListTables(conn = con)
dbGetQuery(con,
           "SELECT * FROM meta")
dbDisconnect(con)

# # delete example tmp file
# unlink("./tmp/", recursive = T)
}
