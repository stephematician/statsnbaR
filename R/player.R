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
#' 
#'
#' @examples \dontrun{
#'     df <- get_players(league=2015)
#'     tail(df)
#' }
#'
#' @param league Character value of league (e.g. 'NBA')
#' @param season Numeric value of the base season (e.g. 2015 for the 2015-2016
#'   season)
#' @return A data.frame with names of current and historical players and the
#'   fields
#'   \describe{
#'      \item{person_id}{integer - ID of the player}
#'      \item{first_name}{character - the player's first name}
#'      \item{last_name}{character - the player's last name}
#'      \item{roster_status}{logical - currently rostered in season selected}
#'      \item{year_start}{integer - year player entered NBA}
#'      \item{year_end}{integer - year player exited NBA}
#'      \item{team_id}{integer - ID of the player's current team (if roster_status is true)}
#'      \item{team_city}{character - city of current team (if roster_status is true)}
#'      \item{team_name}{character - name of current team (if roster_status is true)}
#'      \item{team_abbr}{character - abbrev. name of current team (if roster_status is true)}
#'      \item{has_played}{logical - did the player record at least one game in NBA}
#'   }
#'
#' @export
get_players <- function(league, season) {

    filters <- list(only_current=statsnbaR.ADL.filters$only_current$default,
                    season=statsnbaR.ADL.filters$season$default,
                    league=statsnbaR.ADL.filters$league$default)

    if (!missing(season)) {
        # todo - need to check if its numeric etc
        mapping <- statsnbaR.ADL.filters$season$mapping
        mapped_season <- season
        if (!is.null(mapping))
            mapped_season <- mapping[[season]]
        tc <- type_converters[[statsnbaR.ADL.filters$season$class]]
        value_season <- tc(mapped_season)
        if (is.null(value_season))
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
#' @param endpoint List containing stats.nba.com API endpoing specification.
#' @param ... List of key-value pairs to use as filters in a query to the
#'   per-player data endpoint of stats.nba.com
#'
#' @return A list of key-value pairs for passing to the per-player query
#'   functions
#'
#' @keywords internal
#' @author Stephen Wade
filter_per_player_worker <- function(endpoint, ...) {

    in_filters   <- list(...)
    if ('measurement' %in% names(in_filters)) {
        stop(paste('[statsnbaR filter_per_player_worker] cannot specify',
                   '\'measurement\' as an argument to the filter constructor'))
    }
    if ('clutch' %in% names(in_filters)) {
        stop(paste('[statsnbaR filter_per_player_worker] cannot specify',
                   '\'clutch\' as an argument to the filter constructor'))
    }
    filter_names <- names(endpoint$api.filters)

    if (!all(names(in_filters) %in% filter_names)) {
        invalid_filters <- !(names(in_filters) %in% filter_names)
        stop(paste0('[statsnbaR filter_per_player] invalid filters (',
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

#' Generate filters for a \sQuote{per player} data query.
#'
#' 
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
#' @param ... List of key-value pairs to use as filters in a query to the
#'   per-player data endpoint of stats.nba.com
#'
#' @return A list of key-value pairs for passing to the per-player query
#'   functions
#'
#' @seealso \code{\link{per_player_base}} \code{\link{per_player_advanced}}
#'   \code{\link{per_player_miscellaneous}} \code{\link{per_player_scoring}}
#'   \code{\link{per_player_usage}}
filter_per_player <- function(...) {

    filter_per_player_worker(statsnbaR.ADL.endpoints$per_player_base,
                             ...)
    
}

#' Generate filters for a \sQuote{per player} data query.
#'
#' Generates a list of filters with default values for per player data queries.
#'
#' This function is the same as the \code{\link{per_player_filters}} function
#' with the addition of the filters to determine what \sQuote{clutch} time is.
#'
#'
#' @param ... List of key-value pairs to use as filters in a query to the
#'   per-player data endpoint of stats.nba.com. The additional clutch-related
#'   key-value pairs are described (with their default values indicated at the
#'   end in brackets.
#'   \describe{
#'     \item{clutch_time}{Number of seconds left in game: 300, 240, ..., 60, 30, 10 (300)}
#'     \item{lead}{Whether the team is ahead or behind (incl. tied): 'any', 'ahead', 'behind' ('any')}
#'     \item{point_diff}{The size of the point differential: 1--5 (5)}
#'   }
#'
#' @return A list of key-value pairs for passing to the per-player query
#'   functions
#'
#' @seealso \code{\link{per_player_filters}}
filter_per_player_clutch <- function(...) {

    filter_per_player_worker(statsnbaR.ADL.endpoints$per_player_base_clutch,
                             ...)
    
}
                           
#' Retrieve \sQuote{per player} data
#'
#' Retrieve the aggregated data for each player from stats.nba.com
#'
#' @return A data.frame with the 
per_player_data <- function(filters=filter_per_player(),
                            clutch=FALSE,
                            measurement='base') {

    if (!valid_filters(filters))
        stop(paste('[statsnbaR per_player_data] filters must be a valid',
                   'key-pair list recognised by statsnbaR'))

    if (!is.logical(clutch))
        stop('[statsnbaR per_player_data] \'clutch\' must be logical') 

    clutch_str <- ''
    if (clutch) {
        # need to remove whatever measurement has been set
        filters <- filters[-which(names(filters) == 'measurement')]
        filters <- do.call('filter_per_player_clutch',
                           filters)
        clutch_str <- '_clutch'
    }
    
    if(!(measurement %in% c('base',          'advanced',
                            'miscellaneous', 'scoring',
                            'usage')))
            stop(paste('[statsnbaR per_player_data] invalid \'measurement\'',
                       'type specified'))

    filters$measurement <- measurement
    if(filters$measurement == 'scoring') {
        if(filters$per != 'game') {
            warning(paste('[statsnbaR per_player_data] coercing filter to',
                          '(per=\'game\')'))
        }
        filters$per <- 'game'
    }
    
    if(filters$measurement == 'usage') {
        if(filters$per != 'total') {
            warning(paste('[statsnbaR per_player_data] coercing filter to',
                          '(per=\'total\')'))
        }
        filters$per <- 'total'
    }

    r <- api_scrape(paste0('per_player_', measurement, clutch_str),
                    filters=filters)

    if (length(r) != 1)
        stop(paste('[statsnbaR per_player_data] unexpected number of',
                   'result sets returned by stats.nba.com'))

    return(r[[1]])
}
