# set df.output
df.output <- set_outputLWFB90()
df.output[,] <- 0L
df.output[c("Evap", "Swat", "Flow"), c("Day")] <- 1
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
                            col_select_vp = c("id_custom", "yr", "tran", "ptran"),
                            dir_name= "./tmp/")

ls.soil <- bind_rows(ls.soil)

# create dirs
if(!dir.exists(paste0(output_path_final, "./tmp/soils/"))){
  dir.create(paste0(output_path_final, "./tmp/soils/"), recursive = T)
}

save(soils, file = paste0(output_path_final, "tmp/soils/soils.RData"))

# write to db
fnc_write_to_sql(db_name = "./tmp/testdb.sqlite",
                 dir_tmp = "./tmp/")
