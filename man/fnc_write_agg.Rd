% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_write_agg.R
\name{fnc_write_agg}
\alias{fnc_write_agg}
\title{Function to aggregate and write data from automated LWFB90-Runs}
\usage{
fnc_write_agg(
  x,
  aggr_tp,
  col_select_day = NA,
  col_select_mon = NA,
  col_select_vp = NA,
  col_select_yr = NA,
  dir_name = NA
)
}
\arguments{
\item{x}{one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.}

\item{aggr_tp}{a string containing the desired aggregation time period. Can be \code{daily},  \code{monthly}, \code{vegper}, \code{yearly}, or any combination of the four (i.e. \code{monthly_vegper}). The latter creates results for all aggregation terms detectable within the string.}

\item{col_select_day}{a string containing the desired columns from the daily-aggregation (see details)}

\item{col_select_mon}{a string containing the desired columns from the monthly-aggregation (see details)}

\item{col_select_vp}{a string containing the desired columns from the vegperiod-aggregation (see details)}

\item{col_select_yr}{a string containing the desired columns from the yearly-aggregation (see details)}

\item{dir_name}{directory for tmp files, if \code{NA} as in default, results are returned to console}
}
\value{
Returns the desired output to .rds files, or, the console
}
\description{
LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it to a database. This is what this function is made for. \cr Since v.0.5.0 LWFBrook90R changed the output/default settings. \cr \code{\link{fnc_write}} reduces and aggregates all columns returned by \code{\link[LWFBrook90R]{run_LWFB90}} (see help page). \cr \code{fnc_write_agg}  enables an even more detailed selection of drought indices. See detail section for a full list. Because many indices are calculated, this function will be significantly slower, so check, if you can get the necessary information from the standard list of output variables. \cr Both functions write .rds files with the desired output for each point. \code{\link{fnc_write_to_sql}} can be used to convert these files into a SQLite-DB. This "step-in-between" is necessary because SQLite does not support parallel writing.
}
\section{Output column selection}{

IT IS HIGHLY RECOMMENDED TO MAKE A SUBSELECTION, OR THERE WILL BE A LOT (>100) OF COLUMNS.
For a complete list of possible output types plus description, see \code{"U:/db_brook90_output/whh_db_documentation"}
}

\examples{
# for full example, see help page of ?fnc_write_to_sql

}
