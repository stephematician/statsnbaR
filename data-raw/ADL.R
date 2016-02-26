#' Construct system data for statsnbaR package
#'
#' Generally this is run via the base directory. Delete R/sysdata.R and
#' then use \code{source('data-raw/ADL.R')} to generate the sysdata file.
#'
#'
#' @author Stephen Wade
#' @keywords internal
NULL

library(yaml)
library(devtools)

ADL_data <- yaml.load_file('data-raw/ADL.yaml')

# perform a basic check that all defaults are contained

statsnbaR.ADL.endpoints <- ADL_data$endpoints
statsnbaR.ADL.filters <- ADL_data$filters
statsnbaR.ADL.data <- ADL_data$data
statsnbaR.ADL.host <- ADL_data$host

source('./R/utils.R')
# check that all data has a class

valid_data <- sapply(statsnbaR.ADL.data,
                     function(x) x$class %in% names(type_converters))
if (!all(valid_data))
    stop(paste0('invalid classes for data items in ADL.yaml; ',
                paste0('\'',
                       names(statsnbaR.ADL.data)[!valid_classes],
                       '\'',
                       collapse=', '),
                '.'))

valid_spec <- sapply(statsnbaR.ADL.filters,
                     function(x) c('class', 'default') %in% names(x))
if (!all(valid_spec))
    stop(paste0('missing class or default for filters in ADL.yaml; ',
                paste0('\'',
                       names(statsnbaR.ADL.filters)[!valid_spec],
                       '\'',
                       collapse=', '),
                '.'))
                
# check that the filters match the data
valid_defaults <- sapply(statsnbaR.ADL.filters,
                         function(x) 
                            ifelse(!is.null(x$mapping),
                                   x$default %in% names(x$mapping),
                                   TRUE))
if (!all(valid_defaults))
    stop(paste0('invalid defaults for following filters in ADL.yaml; ',
                paste0('\'',
                       names(statsnbaR.ADL.filters)[!valid_defaults],
                       '\'',
                       collapse=', '),
                '.'))
                

valid_classes <- sapply(statsnbaR.ADL.filters,
                        function(x) x$class %in% names(type_converters))
if (!all(valid_classes))
    stop(paste0('invalid classes for filters in ADL.yaml; ',
                paste0('\'',
                       names(statsnbaR.ADL.filters)[!valid_classes],
                       '\'',
                       collapse=', '),
                '.'))

# to-do
# check that every endpoint has a api.name, api.path, api.filters, api.results

# check that every endpoint specifices valid data in the result

# and every endpoint specifies valid filters in the filters

                
use_data(statsnbaR.ADL.endpoints,
         statsnbaR.ADL.filters,
         statsnbaR.ADL.data,
         statsnbaR.ADL.host,
         pkg='.',
         internal=TRUE)
