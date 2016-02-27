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

# Need to check for any bad anchors
# _yaml.bad-anchor_
any_bad_anchor <- function(x) {
  if (is.list(x)) {
      any(names(x)=='_yaml.bad-anchor_') ||
      any(sapply(x, any_bad_anchor))
  } else FALSE
}

if (any_bad_anchor(ADL_data)) {
    paste_bad_anchors <- function(x_name, x) {
        if (is.list(x)) {
            ind <- sapply(x, any_bad_anchor)
            if (any(ind)) {
                paste(lapply(which(ind), 
                             function(j)
                                paste_bad_anchors(paste(x_name,
                                                        names(x)[j],
                                                        sep='$'),
                                                  x[[j]])), collapse='\n')
            } else x_name
        }
    }
    stop(paste('bad anchors detected for:\n',
               paste_bad_anchors('ADL_data', ADL_data)))
}
    

source('./R/utils.R')
# check that all data has a class

valid_data <- sapply(statsnbaR.ADL.data,
                     function(x) x$class %in% names(type_converters))
if (!all(valid_data))
    stop(paste0('invalid classes for data items in ADL.yaml; ',
                paste0('\'',
                       names(statsnbaR.ADL.data)[!valid_data],
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

# check that every endpoint has a api.name, api.path, api.filters, api.results
valid_endpoints <- sapply(statsnbaR.ADL.endpoints,
                          function(x) c('api.name',
                                        'api.path',
                                        'api.filters',
                                        'api.results') %in% names(x))
if (!all(valid_endpoints))
    stop(paste0('missing api.filters/name/path/results for endpoints',
                'in ADL.yaml; ',
                paste0('\'',
                       names(statsnbaR.ADL.endpoints)[!valid_endpoints],
                       '\'',
                       collapse=', '),
                '.'))

# check that every endpoint specifices valid data in the result
# this should be true provided results are specified by tags!
valid_ep_results <- sapply(statsnbaR.ADL.endpoints,
                           function(x) {
                               results <- do.call(c,
                                                  lapply(x$api.results,
                                                         names))
                               all(results %in%
                                   names(statsnbaR.ADL.data))
                           })

if (!all(valid_ep_results)) {
    stop(paste0('invalid result data for endpoints in ADL.yaml; ',
                paste0(lapply(which(!valid_ep_results),
                              function(j) {
                                  x <- statsnbaR.ADL.endpoints[[j]]
                                  name <- names(statsnbaR.ADL.endpoints)[j]
                                  results <- do.call(c,
                                                     lapply(x$api.results,
                                                            names))
                                  z <- !(results %in% names(statsnbaR.ADL.data))
                                  paste0('\'', name,
                                         '$',  results[z],
                                         '\'', collapse=', ')
                              }), 
                      collapse=', '),
                '.'))
}

# and every endpoint specifies valid filters in the filters
# this should be true provided results are specified by tags!
valid_ep_filters <- sapply(statsnbaR.ADL.endpoints,
                           function(x) {
                               filters <- names(x$api.filters)
                               all(filters %in%
                                   names(statsnbaR.ADL.filters))
                           })

if (!all(valid_ep_filters)) {
    stop(paste0('invalid filters for endpoints in ADL.yaml; ',
                paste0(lapply(which(!valid_ep_filters),
                              function(j) {
                                  x <- statsnbaR.ADL.endpoints[[j]]$api.filters
                                  name <- names(statsnbaR.ADL.endpoints)[j]
                                  filters <- names(x)
                                  z <- !(filters %in%
                                         names(statsnbaR.ADL.filters))
                                  paste0('\'', name,
                                         '$',  filters[z],
                                         '\'', collapse=', ')
                              }), 
                      collapse=', '),
                '.'))
}


# any(names(x)) == '_yaml.bad-anchor_'

use_data(statsnbaR.ADL.endpoints,
         statsnbaR.ADL.filters,
         statsnbaR.ADL.data,
         statsnbaR.ADL.host,
         pkg='.',
         internal=TRUE)
