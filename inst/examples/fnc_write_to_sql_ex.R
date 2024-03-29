# set df.output
df.output <- set_outputLWFB90()
df.output[,] <- 0L
df.output[c("Budg","Evap", "Swat", "Flow"), c("Day")] <- 1
df.output[c("Flow"), c("Mon")] <- 1

# set soil-list
ls.soil <- fnc_get_soil(df.ids = test.ids.bds,
                        soil_option = "BZE",
                        PTF_to_use = "WESSOLEK")

# run multiple points
res <- run_multisite_LWFB90(options_b90 = set_optionsLWFB90(),
                            param_b90 = fnc_get_params(df.ids = test.ids.bds,
                                                       tree_species = "spruce"),
                            soil = ls.soil,

                            climate = fnc_get_clim(df.ids = test.ids.bds,
                                                   mindate = as.Date("2002-01-01"),
                                                   maxdate = as.Date("2003-12-31")),

                            output = df.output,

                            all_combinations = F,
                            cores = parallel::detectCores()-1,

                            rtrn_output = F,
                            rtrn_input = F,

                            output_fun = fnc_write_agg,
                            aggr_tp = "vegper",
                            col_select_vp = c("tran", "ptran"),
                            dir_name= "./tmp/")

ls.soil <- bind_rows(ls.soil)

# create dirs
if(!dir.exists("./tmp/soils/")){
  dir.create("./tmp/soils/", recursive = T)
}

saveRDS(ls.soil, file = "./tmp/soils/soils.rds")

# write to db
fnc_write_to_sql(db_name = "./tmp/testdb2.sqlite",
                 dir_tmp = "./tmp/")

# check how the result looks:
con <- dbConnect(drv = RSQLite::SQLite(), dbname =  "./tmp/testdb.sqlite")
dbListTables(conn = con)
dbGetQuery(con, "SELECT * FROM soils WHERE ID_custom = 'A'")
dbGetQuery(con, "SELECT * FROM vegper WHERE ID_custom = 'A'")
dbGetQuery(con, "SELECT * FROM vegper")
dbDisconnect(con)

# # delete example tmp file
# unlink("./tmp/", recursive = T)
