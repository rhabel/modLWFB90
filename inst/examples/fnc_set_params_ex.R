test.ids.bds <- data.frame("ID" = c(1,2, 3, 4, 5),
                           "easting" = c(493497, 503000, 515138, 501000, 503330),
                           "northing" = c(5281811, 5292500, 5289355, 5293600, 5291700))

ls.param <- fnc_set_params(df.ids = test.ids.bds,
                            tree_species = c("beech", "spruce", "pine", "douglasfir", "beech"))
# as we're working with a list of long lists. For comparative reasons, this function returns the n-th object of each list element
fun1 <- function(lst, n){
  sapply(lst, `[`, n)
}

# tree-species
fun1(ls.param, which(names(ls.param[[1]]) == "budburst.species"))
fun1(ls.param, which(names(ls.param[[1]]) == "glmax"))
fun1(ls.param, which(names(ls.param[[1]]) == "coords_x"))

# all oaks, but change height and age of stands at begin of simulation (parameter "height" & "age.ini")
df.infos <- data.frame("ID" = c(1,2,3,4,5),
                  "height" = c(10, 15, 30, 20, 20),
                  "age.ini" = c(10, 20, 25, 70, 90))

ls.param <- fnc_set_params(df.ids = test.ids.bds,
                           tree_species = "oak",
                           df.ind.info = df.infos)
# tree-species
fun1(ls.param, which(names(ls.param[[1]]) == "budburst.species"))
fun1(ls.param, which(names(ls.param[[1]]) == "height"))
fun1(ls.param, which(names(ls.param[[1]]) == "age.ini"))

