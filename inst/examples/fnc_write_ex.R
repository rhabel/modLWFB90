# create simple sample data:
param_b90 <- set_paramLWFB90()
options_b90 <- set_optionsLWFB90()

# Set start and end Dates for the simulation
options_b90$startdate <- as.Date("2003-01-01")
options_b90$enddate <- as.Date("2006-12-31")

# Derive soil hydraulic properties from soil physical properties
# using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_tab(slb1_soil$texture))

# Run LWF-Brook90
x <- run_LWFB90(

  # run_LWFB90-function arguments:
  options_b90 = options_b90,
  param_b90 = param_b90,
  climate = slb1_meteo,
  soil = soil,

  # keep it easy to read
  rtrn_input = F,
  rtrn_output = F,

  output_fun = fnc_write,
  # fnc_write-function arguments:

  aggr_tp = c("daily_monthly_vegper_yearly_complete"),                            # all aggregation options for demonstration purposes
  out_tables = c("outputs_layer"),                                                # all output sources for demonstration purposes
  col_select_outputs =  c("yr", "mo", "da", "doy", "stres", "relawat", "vrfln"),  # subset from both mean and sum-aggregation
  col_select_layer = c("yr", "mo", "da", "doy", "nl", "theta", "swati", "tran"),  # subset from both mean and sum-aggregation
  depths = c(10,30,600),                                                          # last one won't be calculated
  dir_name = NA,                                                                  # return to console
  soil_nm = "test_soil"
)



# multiple Plots:
# set soil-list
ls.soil <- fnc_get_soil(df.ids = test.ids.bds,
                        soil_option = "BZE",
                        PTF_to_use = "WESSOLEK",
                        add_BodenInfo = F)
ls.parms <- fnc_get_params(df.ids = test.ids.bds,
                           tree_species = "spruce")
ls.clim <- fnc_get_clim(df.ids = test.ids.bds,
                        mindate = as.Date("2002-01-01"),
                        maxdate = as.Date("2004-12-31"))

# run multiple points back to console:
results <- run_multisite_LWFB90(options_b90 = options_b90,
                            param_b90 = ls.parms,
                            soil = ls.soil,

                            climate = ls.clim,


                            all_combinations = F,
                            cores = 5,

                            rtrn_output = F,
                            rtrn_input = F,


                            output_fun = fnc_write,

                            aggr_tp = c("daily_monthly_vegper_yearly_complete"),                            # all aggregation options for demonstration purposes
                            out_tables = c("outputs_layer"),                                                # all output sources for demonstration purposes
                            col_select_outputs =  c("yr", "mo", "da", "doy", "stres", "relawat", "vrfln"),  # subset from both mean and sum-aggregation
                            col_select_layer = c("yr", "mo", "da", "doy", "nl", "theta", "swati", "tran"),  # subset from both mean and sum-aggregation
                            depths = c(10,30,600),                                                          # last one won't be calculated

                            dir_name= NA                                                            # write to ./tmp/

                            )
results[[1]]

# run multiple points and write output
run_multisite_LWFB90(options_b90 = options_b90,
                            param_b90 = ls.parms,
                            soil = ls.soil,

                            climate = ls.clim,


                            all_combinations = F,
                            cores = 5,

                            rtrn_output = F,
                            rtrn_input = F,


                            output_fun = fnc_write,

                            aggr_tp = c("daily_monthly_vegper_yearly_complete"),                            # all aggregation options for demonstration purposes
                            out_tables = c("outputs_layer"),                                                # all output sources for demonstration purposes
                            col_select_outputs =  c("yr", "mo", "da", "doy", "stres", "relawat", "vrfln"),  # subset from both mean and sum-aggregation
                            col_select_layer = c("yr", "mo", "da", "doy", "nl", "theta", "swati", "tran"),  # subset from both mean and sum-aggregation
                            depths = c(10,30,600),                                                          # last one won't be calculated

                            dir_name= "./tmp/"                                                              # write to ./tmp/

                            )


# delete example tmp file
unlink("./tmp/", recursive = T)



