% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_get_clim_sgl.R
\name{fnc_get_clim_sgl}
\alias{fnc_get_clim_sgl}
\title{Reading data directly from climate db}
\usage{
fnc_get_clim_sgl(
  id,
  easting,
  northing,
  mindate = as.Date("2010-01-01"),
  maxdate = as.Date("2011-10-31"),
  path_std = "R:/klima/whh/brook90_input/locations",
  path_climdb = "R:/klima/whh/brook90_input/db/"
)
}
\arguments{
\item{id}{ID_custom of the point}

\item{easting}{UTM32632 easting of respective point}

\item{northing}{UTM32632 northing of respective point}

\item{mindate}{first day of modelling time period as \code{Date}- object}

\item{maxdate}{last day of modelling time period as \code{Date}- object}

\item{path_std}{path to standard locations directory}

\item{path_climdb}{path to climate-db directory}
}
\value{
creates climate data on-the-fly for \code{\link[LWFBrook90R]{run_multisite_LWFB90}} by returning a correct clim-df
}
\description{
This function returns climate data from the main FVA-Climate Databases in the format required by \code{\link[LWFBrook90R]{run_LWFB90}}. \code{\link[LWFBrook90R]{run_multisite_LWFB90}} can read climate data directly via \code{climate} and \code{climate_args} without loading data into the session's memory. \cr\cr See example for how to set \code{climate_args} correctly.
}
\examples{
df.ids <- test.ids.bds

ls.parms <- fnc_get_params(df.ids, tree_species = "spruce")
ls.soil <- fnc_get_soil(df.ids, soil_option = "BZE", testgebiet = "NPS", PTF_to_use = "HYPRES", limit_MvG = T, rootsmethod = "betamodel" )

options.b90 <- set_optionsLWFB90(startdate = as.Date("2008-01-01"),
                                 enddate = as.Date("2009-06-01"))


clim_args <-
  lapply(split(df.ids, seq(nrow(df.ids))),
         function(x) list(id = as.character(x$ID_custom),
                          easting = x$easting,
                          northing = x$northing,
                          mindate = options.b90$startdate,
                          maxdate = options.b90$enddate))
names(clim_args) <- as.character(df.ids$ID_custom)

res <- run_multisite_LWFB90(options_b90 = options.b90,
                            soil = ls.soil,
                            param_b90 = ls.parms,

                            climate = fnc_get_clim_sgl,
                            climate_args = clim_args)
}
