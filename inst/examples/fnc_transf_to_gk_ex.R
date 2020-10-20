test.ids.utm <- data.frame("ID" = c(1,2, 3, 4, 5),
                           "easting" = c(493497, 503000, 515138, 501000, 503330),
                           "northing" = c(5281811, 5292500, 5289355, 5293600, 5291700))

fnc_transf_to_gk(df = test.ids.utm)
