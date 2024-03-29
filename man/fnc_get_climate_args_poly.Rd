% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_get_climate_args_poly.R
\name{fnc_get_climate_args_poly}
\alias{fnc_get_climate_args_poly}
\title{Function to create list of climate arguments for polygons as input}
\usage{
fnc_get_climate_args_poly(df.ids, mindate, maxdate, store_as = NA)
}
\arguments{
\item{df.ids}{data frame with \code{ID_custom}, \code{id_standard}, \code{tranche} (if \code{df.ids} is created by \code{\link{fnc_create_IDs}}, these columns are already there. Must otherwise be created with \code{\link{fnc_relateCoords}}), and \code{lower}}

\item{mindate}{used for filtering climate data to certain dates. Date-object, recommended to set to \code{options.b90$startdate}.}

\item{maxdate}{used for filtering climate data to certain dates. Date-object, recommended to set to \code{options.b90$enddate}.}

\item{store_as}{when the file should be stored somewhere, a complete path with file ending \code{".rds"} must be provided, else if \code{NA}, list will be returned to the console}
}
\value{
list of climate arguments as needed by \code{\link{fnc_read_climdb_poly}}
}
\description{
When climate data, as needed in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, isn't provided as a list (which takes up ram if too many points are modelled), but as a list of climate arguments to be read out by \code{\link{fnc_read_climdb_poly}}, this function creates the necessary argument list. Polygons usually need the input of several Climate-Cells. Which ones and how much weight each cell contributes to the climate within the polygons is determined here.
}
