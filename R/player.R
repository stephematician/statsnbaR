#' Player data
#'
#' @name player
#' @include scrape.R
#' @include utils.R
#' @keywords internal
NULL

#' Retrieve current and historical player data
#'
#' Retrieves current and historical player data from stats.nba.com
#'
#' @examples \dontrun{
#'     df <- get_players(league=2015)
#'     tail(df)
#' }
#'
#' @param league Character value of league (e.g. 'NBA')
#' @param season Numeric value of the base season (e.g. 2015 for the 2015-2016
#'   season)
#' @export
get_players <- function(league, season) {

    filters <- list(only_current=statsnbaR.ADL.filters$only_current$default,
                    season=statsnbaR.ADL.filters$season$default,
                    league=statsnbaR.ADL.filters$league$default)

    if (!missing(season)) {
        if(!(season %in% names(statsnbaR.ADL.filters$season$mapping)))
            stop(paste('[statsnbaR get_players]',
                        season,
                        'is not a valid season.'))
         filters$season <- season
    }

    if (!missing(league)) {
        if(!(league %in% names(statsnbaR.ADL.filters$league$mapping)))
            stop(paste('[statsnbaR get_players]',
                        league,
                        'is not a valid league code.'))
         filters$league <- league
    }

    r <- api_scrape('player_registry', filters=filters)

    if (length(r) != 1) stop(paste('[statsnbaR get_players] unexpected number',
                                   'of result sets returned by stats.nba.com'))
    return(r[[1]])
}

#'
get_player_bios <- function() {

}

#' Generate filters for a \sQuote{per player} data query.
#'
#' @param ... List of key-value pairs to use as filters in a query to the
#'   per-player data endpoint of stats.nba.com
#'
#' \describe{
#'     \item{college}{}
#'     \item{conference}{}
#'     \item{country}{}
#'     \item{date_from}{}
#'     \item{date_to}{}
#'     \item{division}{}
#'     \item{draft_pick}{}
#'     \item{draft_year}{}
#'     \item{game_scope}{}
#'     \item{game_segment}{}
#'     \item{height_segment}{}
#'     \item{last_n}{}
#'     \item{league}{}
#'     \item{location}{}
#'     \item{month}{}
#'     \item{opponent_team}{}
#'     \item{win_loss}{}
#'     \item{playoff_round}{}
#'     \item{pace_adjust}{}
#'     \item{per}{}
#'     \item{period}{}
#'     \item{experience}{}
#'     \item{position}{}
#'     \item{plus_minus}{}
#'     \item{rank}{}
#'     \item{season}{}
#'     \item{season_segment}{}
#'     \item{season_type}{}
#'     \item{shot_clock}{}
#'     \item{starting_status}{}
#'     \item{team_id}{}
#'     \item{opponent_conference}{}
#'     \item{opponent_division}{}
#'     \item{weight_segment}{}
#' }
#'
#' @return A list of key-value pairs for passing to the per-player query
#'   functions
#'
#' @seealso \code{\link{per_player_base}} \code{\link{per_player_advanced}}
#'   \code{\link{per_player_miscellaneous}} \code{\link{per_player_scoring}}
#'   \code{\link{per_player_usage}}
#'
#' @export
#' @author Stephen Wade
per_player_filters <- function(...) {

    in_filters   <- list(...)
    if ('measurement' %in% names(in_filters)) {
        stop(paste('[statsnbaR per_player_filters] cannot specify',
                   '"measurement" as an argument in this filter constructor'))
    }

    filter_names <- names(statsnbaR.ADL.endpoints$per_player_base$api.filters)

    if (!all(names(in_filters) %in% filter_names)) {
        invalid_filters <- !(names(in_filters) %in% filter_names)
        stop(paste0('[statsnbaR per_player_filters] invalid filters (',
                    paste(names(in_filters)[invalid_filters],
                          collapse=', '),
                    ') specified for per player query.'))
                            
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

                           
#' Retrieve \sQuote{per player} base data
per_player_base <- function(filters=per_player_filters(),
                            clutch=FALSE) {

    filters$measurement <- 'base'

    r <- api_scrape(ifelse(!clutch,
                           'per_player_base',
                           'per_player_base_clutch'), 
                    filters=filters)

        if (length(r) != 1) stop(paste('[statsnbaR aggregated_player_data]',
                                       'unexpected number of result sets',
                                       'returned by stats.nba.com'))
    return(r[[1]])
}

#' Retrieve \sQuote{per player} advanced data
per_player_advanced <- function(filters=per_player_filters(),
                                clutch=FALSE) {

    filters$measurement = 'advanced'
    
    r <- api_scrape(ifelse(!clutch,
                           'per_player_advanced',
                           'per_player_advanced_clutch'), 
                    filters=filters)

        if (length(r) != 1) stop(paste('[statsnbaR aggregated_player_data]',
                                       'unexpected number of result sets',
                                       'returned by stats.nba.com'))
    return(r[[1]])
}

#' Retrieve \sQuote{per player} miscellaneous data
per_player_miscellanous <- function(filters=per_player_filters(),
                                    clutch=FALSE) {

    filters$measurement = 'miscellaneous'

    r <- api_scrape(ifelse(!clutch,
                           'per_player_miscellaneous',
                           'per_player_miscellaneous_clutch'), 
                    filters=filters)
    
        if (length(r) != 1) stop(paste('[statsnbaR aggregated_player_data]',
                                       'unexpected number of result sets',
                                       'returned by stats.nba.com'))
    return(r[[1]])
}

#' Retrieve \sQuote{per player} scoring data
per_player_scoring <- function(filters=per_player_filters(),
                               clutch=FALSE) {

    filters$measurement = 'scoring'

    r <- api_scrape(ifelse(!clutch,
                           'per_player_scoring',
                           'per_player_scoring_clutch'), 
                    filters=filters)
    
        if (length(r) != 1) stop(paste('[statsnbaR aggregated_player_data]',
                                       'unexpected number of result sets',
                                       'returned by stats.nba.com'))
    return(r[[1]])
}

#' Retrieve \sQuote{per player} usage data
per_player_usage <- function(filters=per_player_filters(),
                             clutch=FALSE) {

    filters$measurement = 'usage'

    r <- api_scrape(ifelse(!clutch,
                           'per_player_usage',
                           'per_player_usage_clutch'), 
                    filters=filters)
    
        if (length(r) != 1) stop(paste('[statsnbaR aggregated_player_data]',
                                       'unexpected number of result sets',
                                       'returned by stats.nba.com'))
    return(r[[1]])
}


