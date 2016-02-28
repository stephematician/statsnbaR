#' Main scrape function
#'
#' @name scrape
#' @include utils.R
#' @include statsnbaR-package.R
#' @keywords internal
NULL

#' Scrape stats.nba.com API and return statsnbaR data
#'
#' Perform a query on a recognised endpoint of stats.nba.com, and
#' format the output as a data.frame with inferred data types for each
#' attribute based on statsnbaR's internal YAML specifications. 
#'
#' The endpoints, data types returned and filter specifications are given in the
#' internal YAML which is available on
#' \url{https://www.github.com/stephematician/statsnbaR/tree/master/data-raw/ADL.yaml}
#'
#' The YAML specifies four main things, the;
#' \itemize{
#'   \item host (stats.nba.com);
#'   \item names of accepted filters and their mappings to stats.nba.com;
#'   \item result data conversion parameters (such as class or mapping); and
#'   \item the endpoint APIs, what filters they accept and what data they
#'   should return.
#' }
#'
#' How the last three items are specified is briefly described in the following
#' sections.
#'
#' @section \sQuote{statsnbaR filters} YAML:
#'
#' \preformatted{
#' filters  :
#'     league :
#'         mapping : \{
#'             NBA      : "00",    # NBA
#'             D-league : "20"     # D-League
#'             \}
#'         default : NBA
#' }
#'
#' This is hopefully fairly self-explanatory:
#' \itemize{
#'   \item The mapping is from values that statsnbaR uses internally such
#'     as \code{NBA}, to the value that stats.nba.com expects.
#'   \item Default values are specified for each filter.
#' }
#'
#' @section \sQuote{statsnbaR datatype} YAML:
#'
#' The desired properties of the returned data types and/or a way to
#' map them to desired types is specified in the YAML in a fashion similar
#' to the following example:
#'
#' \preformatted{
#' data    :
#'     person_id :
#'         class   : numeric
#'   &active roster_status :
#'        class   : logical
#'        mapping : \{
#'            0 : FALSE,    # not active
#'            1 : TRUE      # active player
#'            \}
#' }
#'
#' Things to note are:
#' \itemize{
#'   \item The class of the data-type will refer to a specific type
#'     converter from \code{\link{type_converters}}.
#'   \item THe mapping for a data type may not guarantee the correct
#'     type and so the type converter is employed after the mapping.
#' }
#'
#'
#' @section End-point YAML:
#'
#' The endpoint defined in the YAML might look like
#'
#' \preformatted{
#' endpoints :
#'     PlayerCommon :
#'         api.name     : "commonallplayers"
#'         api.path     : "stats"
#'         api.referrer : "http://stats.nba.com/players"
#'         api.filters  :
#'             league       : "LeagueID"
#'         api.results :
#'             CommonAllPlayers : \{
#'                 person_id    : "person_id",
#'                 \}
#' }
#'
#' \itemize{
#'   \item The first three items under PlayerCommon are passed to
#'     \code{httr::GET}.
#'   \item The \code{api.filters} item is a mapping from filter names that
#'     statsnbaR uses internally to filter names that the stats.nba.com
#'     endpoint uses.
#'   \item The \code{api.results} item maps the attribute names of the data
#'     returned by stats.nba.com to names that statsnbaR uses internally.
#'     see the section \sQuote{Data specification YAML}.
#' }
#'
#' @param endpoint Character string which identifies the statsnbaR endpoint
#' @param filters Named list of statsnbaR key-value filters for the query
#' @return A list of data.frames of the resultSets returned by stats.nba.com
#'   with inferred data types.
#'
#' @include utils.R
#' @include statsnbaR-package.R
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom httr accept
#' @importFrom httr modify_url
#' @export
api_scrape <- function(endpoint, filters) {

    if (!is.character(endpoint) ||
        !(endpoint %in% names(statsnbaR.ADL.endpoints)))
        stop(paste('[statsnbaR scrape]',
                   endpoint,
                   'is not a recognised reference to an endpoint of',
                   'stats.nba.com'))

    ADL.endpoint <- statsnbaR.ADL.endpoints[[endpoint]]

    if (!valid_filters(filters))
        stop(paste('[statsnbaR scrape] filters must be a valid key-pair',
                   'list recognised by statsnbaR'))
        
        

    # Check that all filters are valid for this endpoint, and that all the
    # filters required for the endpoint are specified.
    if (!all(names(filters) %in% names(ADL.endpoint$api.filters)) ||
        length(filters) != length(ADL.endpoint$api.filters)) {
        invalid_filters <- !(names(filters) %in%
                             names(ADL.endpoint$api.filters))
        missing_filters <- !(names(ADL.endpoint$api.filters) %in%
                             names(filters))
        MFmsg <- ''
        if (any(missing_filters))
            MFmsg <- paste0(
                'missing filters (',
                paste(names(ADL.endpoint$api.filters)[missing_filters],
                      collapse=', '),
                ') for stats.nba.com endpoint ',
                ADL.endpoint$api.name, '\n'
            )
        IFmsg <- ''
        if (any(invalid_filters))
            IFmsg <- paste0('invalid filters (',
                            paste(names(filters)[invalid_filters],
                                  collapse=', '),
                            ') specified for stats.nba.com endpoint ',
                            ADL.endpoint$api.name, '\n')
        stop(paste('[statsnbaR scrape]', paste0(MFmsg, IFmsg)))
    }

    # Get the stats.nba.com form of the filters
    ADL.filters <- map_filters(filters, ADL.endpoint)

    # Let this fail if we don't get JSON data.
    api_response <- GET(url=modify_url(statsnbaR.ADL.host,
                                       path=c(ADL.endpoint$api.path,
                                              ADL.endpoint$api.name)),
                        query=ADL.filters,
                        accept('application/json'), 
                        add_headers(Referer=ADL.endpoint$api.referrer))

    json_data <- content(api_response)

    if (!valid_results(json_data))
        stop(paste('[stats_nba scrape] invalid result returned by',
                   'stats.nba.com endpoint'))

    # flatten the data into a nice data.frame, unfortunately there is an
    # edge case where the resultSets are not returned as a list of resultSets
    # and so we need to account for this.
    untyped_data <- NULL
    if (is.null(names(json_data$resultSets))) {
        untyped_data <- lapply(json_data$resultSets,
                               flatten_json_rowSet)
        names(untyped_data) <- sapply(json_data$resultSets,
                                      function(x) x$name)
    } else {
        untyped_data <- list(flatten_json_rowSet(json_data$resultSets))
        names(untyped_data) <- json_data$resultSets$name
    }

    # Convert to 'useful' data types
    lapply(names(ADL.endpoint$api.results),
           function(result_name) {
               map_results(untyped_data[[result_name]],
                           ADL.endpoint$api.results[[result_name]])
           })

}


