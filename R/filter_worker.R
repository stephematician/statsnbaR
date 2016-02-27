#' Filter worker for all queries
#'
#' @name filter_worker
#' @include statsnbaR-package.R
#' @keywords internal
NULL

#' Worker function to generate filters for an endpoint data query.
#'
#' @description
#' All the filter functions are wrappers for this worker, which merges the
#' default values from and endpoint with the arguments specified.
#'
#' @details
#' Returns a named list of key-value pairs which are given either by the
#' arguments to the function, or by default values specified by statnbaR
#' internal YAML data. Ensures that all the filters specified have a value,
#' but performs no checks on the values themselves.
#'
#' @param endpoint List containing stats.nba.com API endpoint specification as
#'   given by statsnbaR YAML data.
#' @param ... List of key-value pairs to use as a filters.
#'
#' @return A list of key-value pairs for passing  as the filter argument to the
#'   query functions
#'
#' @seealso \code{\link{filter_bio}}  \code{\link{filter_per_player}}
#'   \code{\link{filter_per_player_clutch}}
#' @keywords internal
#' @author Stephen Wade
filter_construction_worker <- function(endpoint, ...) {

    in_filters   <- list(...)

    filter_names <- names(endpoint$api.filters)

    if (!all(names(in_filters) %in% filter_names)) {
        invalid_filters <- !(names(in_filters) %in% filter_names)
        stop(paste0('[statsnbaR filter_construction_worker] invalid filters (',
                    paste(names(in_filters)[invalid_filters],
                          collapse=', '),
                    ') specified for query to endpoint.'))
    }

    filters <- lapply(filter_names,
                      function(name) {
                          in_filter <- in_filters[[name]]
                          if (!is.null(in_filter)) {
                              in_filter
                          } else statsnbaR.ADL.filters[[name]]$default
                      })
    names(filters) <- filter_names

    return(filters)
}
