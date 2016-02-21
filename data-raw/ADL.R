#' Construct system data for statsnbaR package
#'
#' Generally this is run via the base directory. Delete R/sysdata.R and
#' then use \code{source('data-raw/ADL.R')} to generate the sysdata file.
#'
#'
#' @author Stephen Wade
NULL

library(yaml)
library(devtools)

ADL_data <- yaml.load_file('data-raw/ADL.yaml')

statsnbaR.ADL.endpoints <- ADL_data$endpoints
statsnbaR.ADL.filters <- ADL_data$filters
statsnbaR.ADL.data <- ADL_data$data
statsnbaR.ADL.host <- ADL_data$host

use_data(statsnbaR.ADL.endpoints,
         statsnbaR.ADL.filters,
         statsnbaR.ADL.data,
         statsnbaR.ADL.host,
         pkg='.',
         internal=TRUE)
