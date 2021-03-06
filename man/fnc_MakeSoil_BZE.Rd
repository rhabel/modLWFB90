% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_depth_discr_BZE.R
\name{fnc_MakeSoil_BZE}
\alias{fnc_MakeSoil_BZE}
\title{Creating soil dataframes from BZE Data}
\usage{
fnc_MakeSoil_BZE(soil, skltn)
}
\arguments{
\item{soil}{soil information in a certain form}

\item{skltn}{discretisation information in a certain form}
}
\value{
Returns a longer data.table that already includes an earlier version of ls.soils. Further processed in \code{fnc_soil_bze( )}.
}
\description{
This function has been merged from several steps of Paul Schmidt-Walter's documentation on soil parametrisation from BZE data ("U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb".). In short, it is the equivalent to fnc_depth_disc, just for the data structure of the data extracted from the BZE-rasterstack using data.table and some merging and melting steps.
}
