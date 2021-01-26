# create folder for testfiles
dir.create("H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/testfiles/SOIL/", recursive = T)
dir.create("H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/testfiles/PTF/", recursive = T)
output1 <- "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/testfiles/SOIL/"
output2 <- "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/testfiles/PTF/"

# comparing the different soil data provided by STOK, BZE and OWN with HYPRES as PTF
fnc_compare_soil(df.ids = test.ids.bds,
                 testgebiet = "BDS",
                 what_to_test = "SOILDATA",
                 depths_to_test = c(-0.15, -0.30, -0.60),

                 soiloptions_to_test = c("STOK", "BZE", "OWN"),
                 MvG_own_vals = F,
                 PTF_to_use = "HYPRES",

                 limit_MvG = F,
                 df.soils = df.own.test,
                 output_path = output1)

# comparing how the three PTF options implemented in LWFBrook90 perform with the soil data from STOK
fnc_compare_soil(df.ids = test.ids.bds,
                 testgebiet = "BDS",
                 what_to_test = "PTFs",
                 depths_to_test = c(-0.15, -0.30, -0.60, -0.9),

                 PTF_to_test = c("HYPRES", "PTFPUH2", "WESSOLEK"),
                 soiloption_to_use = "BZE",

                 limit_MvG = F,
                 df.soils = NULL,
                 output_path = output2)
