#' Scrape stats.nba.com API and return statsnbaR data
#'
#' @return A data frame or a list of data frames depending on the number of
#' result sets returned by the JSON API.
#'
#' @include utils.R
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom httr accept
#' @importFrom httr modify_url
api_scrape <- function(endpoint, filters) {
    # Provides some sanity checking and translates names of api endpoints and
    # the names of the filter before running the query, then simply return the
    # resultSets, which have a 'name', 'headers', and then the data

    if (!is.character(endpoint) ||
        !(endpoint %in% names(statsnbaR.ADL.endpoints)))
        stop(paste('[statsnbaR scrape]',
                   endpoint,
                   'is not a recognised reference to an endpoint of',
                   'stats.nba.com'))

    ADL.endpoint <- statsnbaR.ADL.endpoints[[endpoint]]

    # Don't accept coercible data, must be a list.
    if (!is.list(filters))
        stop(paste('[statsnbaR scrape] filters must be a list'))

    # Check that all filters are valid for this endpoint, and that all the
    # filters required for the endpoint are specified.
    if (!all(names(filters) %in% names(ADL.endpoint$api.filters))) {
        flagged <- !(names(filters) %in% names(ADL.endpoint$api.filters))
        stop(paste('[statsnbaR scrape] invalid filters -',
                   paste(names(filters)[flagged],
                         collapse=' '),
                   '- specified for stats.nba.com endpoint',
                   ADL.endpoint$api.name))
    }
    
    if (!all(names(ADL.endpoint$api.filters) %in% names(filters))) {
        flagged <- !(names(ADL.endpoint$api.filters) %in% names(filters))
        stop(paste('[statsnbaR scrape] missing filters -',
                   paste(names(ADL.endpoint$api.filters)[flagged],
                         collapse=' '),
                   '- for stats.nba.com endpoint',
                   ADL.endpoint$api.name))
    }

    ADL.filters <- map_filters(filters, ADL.endpoint)


    # Let this fail if we don't get JSON data.
    api_response <- GET(url=modify_url(statsnbaR.ADL.host,
                                       path=c(ADL.endpoint$api.path,
                                              ADL.endpoint$api.name)),
                        query=ADL.filters,
                        accept('application/json'), 
                        add_headers(Referer=ADL.endpoint$api.referrer))

    json_data <- content(api_response)

    untyped_data <- lapply(json_data$resultSets,
                           flatten_json_rowSet)
    names(untyped_data) <- sapply(json_data$resultSets,
                                  function(x) x$name)

    lapply(names(ADL.endpoint$api.results),
           function(result_name) {

               map_results(untyped_data[[result_name]],
                           ADL.endpoint$api.results[[result_name]])
           })

}


