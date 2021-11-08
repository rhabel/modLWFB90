df.ids <- test_ids_bze

meta <- data.frame("ID_custom" = df.ids$ID_custom,
                   modelled = 0,
                   err.code = "no error")

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
