# create folder for testfiles
dir.create("./testfiles_PTF/")
dir.create("./testfiles_SOIL/")
output1 <- paste0(getwd(), "/testfiles_SOIL/")
output2 <- paste0(getwd(), "/testfiles_PTF/")
# PTFs
fnc_compare_soil(df.ids = test.ids.bds,
                 testgebiet = "BDS",
                 what_to_test = "SOILDATA",
                 depths_to_test = c(-0.15, -0.30, -0.60),

                 soiloptions_to_test = c("STOK", "BZE", "OWN"),
                 MvG_own_vals = F,
                 PTF_to_use = "HYPRES",

                 limit_MvG = F,
                 df.soils = df.own.test,
                 output_path = output)
fnc_compare_soil(df.ids = test.ids.bds,
                 testgebiet = "BDS",
                 what_to_test = "PTFs",
                 depths_to_test = c(-0.15, -0.30, -0.60),

                 PTF_to_test = c("HYPRES", "PTFPUH2", "WESSOLEK"),
                 soiloption_to_use = "STOK",

                 limit_MvG = F,
                 df.soils = NULL,
                 output_path = output)
