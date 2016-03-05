#' Player data
#'
#'TODO shooting data
#'
#' @name player
#' @include filter_player.R
#' @include utils.R
#' @include scrape.R
#' @include statsnbaR-package.R
#' @keywords internal
NULL

#' Retrieve current and historical player data
#'
#' @description
#' Retrieves current and historical player data from stats.nba.com
#'
#' @details
#' Retrieves basic information about players that stats.nba.com contains in
#' their dataset for a given \code{league} and  given \code{season}. Basic
#' information is the player's current team, and their first and final
#' years in the league and so on. The \code{team_id} and \code{person_id} values
#' returned should be unique for each team and player respectively, so these are
#' useful keys to other data.
#'
#' @examples \dontrun{
#'     df <- get_players(season=2014, league='d-league')
#'     tail(df)
#' }
#'
#' @param league A character value of league, current recognised values are:
#'   \describe{
#'     \item{'nba'}{NBA player info}
#'     \item{'d-league}{D-League player info}
#'   }
#' @param season Numeric value of the base season, e.g. 2015 for the 2015-2016
#'   season and so on.
#' @return A data.frame with names of current and historical players and the
#'   fields
#'   \describe{
#'      \item{person_id}{integer - ID of the player, used in many other
#'        datasets}
#'      \item{first_name}{character - the player's first name}
#'      \item{last_name}{character - the player's last name}
#'      \item{roster_status}{logical - currently rostered in the selected
#'        season}
#'      \item{year_start}{integer - year player entered NBA}
#'      \item{year_end}{integer - year player exited NBA}
#'      \item{team_id}{integer - ID of the player's team in the season (if 
#'        roster_status is true)}
#'      \item{team_city}{character - city of the player's team in the season (if
#'        roster_status is true)}
#'      \item{team_name}{character - name of the player's team in the season
#'        (if roster_status is true)}
#'      \item{team_abbr}{character - abbrev. name of player's team (if
#'        roster_status is true)}
#'      \item{has_played}{logical - did the player record at least one game in
#'        NBA at any time (even after the selected \code{season})}
#'   }
#'
#' @export
get_players <- function(league, season) {

    filters <- list(only_current=statsnbaR.ADL.filters$only_current$default,
                    season=statsnbaR.ADL.filters$season$default,
                    league=statsnbaR.ADL.filters$league$default)

    if (!missing(season)) {
        if (!valid_filters(list(season=season),
                           allow_na=FALSE))
            stop(paste('[statsnbaR get_players]',
                       season,
                       'is not a valid season.'))
        filters$season <- season
    }

    if (!missing(league)) {
        if (!valid_filters(list(league=league)))
            stop(paste('[statsnbaR get_players]',
                       league,
                       'is not a valid league name.'))
        filters$league <- league
    }

    r <- api_scrape('player_registry', filters=filters)

    if (length(r) != 1) stop(paste('[statsnbaR get_players] unexpected number',
                                   'of result sets returned by stats.nba.com'))
    return(r[[1]])
}

#' Get player biography data
#'
#' @description
#' Retrieves the player biography data from stats.nba.com subject to the
#' filters recognised by statsnbaR, and returns it as a data.frame.
#'
#' @details
#' Provides player data given as per-season measurements. For a given league and
#' season, this will return basic and advanced statistics for all the active
#' players.
#'
#' The query filters are constructed by the \code{\link{filter_bio}} function.
#' The list of filters available is given in the documentation for that
#' constructor. The units that the values/data is aggregated into is determined
#' by the \code{per} filter, which can be set to either \code{total} for the
#' total values or \code{game} for per-game values. This won't affect the
#' percentage statistics returned.
#' 
#' The filters may include season segments, such as the playoffs,
#' date filters or versus opponents and so on, and the values returned will
#' reflect the performance and data collected in only those selected periods
#' or whatever filter is applied.
#'
#' @examples \dontrun{
#'     df <- get_bio(filter_bio(season=2014))
#'     tail(df)
#' }
#'
#' @param filters A named list of key-value filters constructed by 
#'   \code{\link{filter_bio}}. Full list of avaiable filters that statsnbaR
#'   recognises is given in the documentation the filter constructor.
#' @return A data.frame containing the player biography data with
#'   columns converted to the data types specified by statsnbaR's internal
#'   YAML.
#'   \describe{
#'      \item{person_id}{integer - ID of the player, used in many other
#'        datasets}
#'      \item{player_name}{character - the player's first and last name}
#'      \item{team_id}{integer - the \code{team_id} of the team the player plays for
#'        according to the season selected (see \code{\link{filter_bio}})}
#'      \item{team_abbr}{character - abbreviated team name}
#'      \item{age}{integer - age of player in the season selected (see
#'        \code{\link{filter_bio}})}
#'      \item{height}{integer - listed player height in inches}
#'      \item{weight}{integer - listed player weight in lbs}
#'      \item{college}{character - name of college or prior affiliation}
#'      \item{country}{character - name of country of origin of player}
#'      \item{draft_year}{integer - year player was drafted  (NA if undrafted)}
#'      \item{draft_round}{integer - round player was drafted in  (NA if undrafted)}
#'      \item{draft_number}{integer - number player drafted at (NA if undrafted)}
#'      \item{games}{numeric - games played within the selected filters}
#'   }
#' All other returned columns are described in the glossary provided at
#' \url{http://stats.nba.com/help/glossary}.
#'
#' @seealso
#' \url{http://stats.nba.com/help/glossary/}
#' 
#' @export
get_bio <- function(filters=filter_bio()) {

    if (!valid_filters(filters))
        stop(paste('[statsnbaR get_bio] filters must be a valid',
                   'key-pair list recognised by statsnbaR'))

    r <- api_scrape('player_bio',
                    filters=filters)

    if (length(r) != 1)
        stop(paste('[statsnbaR get_bio] unexpected number of',
                   'result sets returned by stats.nba.com'))

    return(r[[1]])
}

                           
#' Retrieve \sQuote{per player} aggregated data
#'
#' Retrieve the aggregated data for each player from stats.nba.com
#'
#' Collects player 'play' statistics as averages or other aggregates over games
#' in different units such as total values, per game values, per possession
#' values and so forth.
#'
#' The statistics are grouped into various categories which are determined
#' by the value of the \code{measurement} argument. The categories are 
#' \sQuote{base}, \sQuote{advanced}, \sQuote{miscellaneous}, \sQuote{scoring}
#' and \sQuote{usage}.
#'
#' The query filters are constructed by either \code{\link{filter_per_player}} if
#' \code{clutch=FALSE} or \code{\link{filter_per_player_clutch}} if
#' \code{clutch=TRUE}. The list of potential filters are documented in those
#' functions. The units that the values/data is aggregated into is determined
#' by the \code{per} filter. The value of \code{clutch} argument is used to
#  indicate if additional filters are applied that restrict the data collected
#' to plays that occured during clutch time.
#'
#' @param filters A named list of key-value filters constructed by either
#'   \code{\link{filter_per_player}} if \code{clutch=FALSE} or
#'   \code{\link{filter_per_player_clutch}} if \code{clutch=TRUE}. Full list of
#'   avaiable filters that statsnbaR recognises is given in the documentation
#'   of those two filter constructor functions.
#' @param clutch A logical value indicating whether to extract the
#'   \sQuote{clutch} data, i.e. to only include plays that occurred during
#'   \sQuote{clutch} time in the aggregation.
#' @param measurement A character string representing the desired dataset
#'   \describe{
#'     \item{base}{The traditional player statistics such as field goals,
#'       rebounds, assists etc.}
#'     \item{advanced}{The advanced player statistics such as offensive and
#'       defensive ratings, effective field goal %age, PIE and pace, and
#'       others.}
#'     \item{miscellaneous}{Miscellaneous player statistics such as points in
#'       the paint, fastbreak points, opponent points in the paint, blocks,
#'       fouls, etc.}
#'     \item{scoring}{Scoring player statistics such as the percentage
#'       of points off turnovers, in the paint, points that were assisted on
#'       etc. by that player.}
#'     \item{usage}{Usage statistics which are roughly the traditional
#'       statistics but measured as the players percentage contribution of the
#'       teams values while they are on court.}
#' }
#'
#' @return A data.frame containing the aggregated player data retrieved with
#'   columns converted to the data types specified by statsnbaR's internal
#'   YAML.
#' \describe{
#'      \item{person_id}{integer - ID of the player, used in many other
#'        datasets}
#'      \item{player_name}{character - the player's first and last name}
#'      \item{team_id}{integer - the \code{team_id} of the team the player plays for
#'        according to the season selected (see \code{\link{filter_bio}})}
#'      \item{team_abbr}{character - abbreviated team name}
#'      \item{age}{integer - age of player in the season selected}
#'      \item{games}{numeric - games played within the selected filters}
#'      \item{wins}{numeric - wins in the player's games within the
#'        selected filters}
#'      \item{wins}{numeric - losses in the player's games within the
#'        selected filters}
#'      \item{mins}{numeric - minutes player was on court in game}
#' }
#' All other returned columns are described in the glossary provided at
#' \url{http://stats.nba.com/help/glossary}.

#' @examples \dontrun{
#'   # Get data to compare clutch vs regular performance.
#'   ppd <- per_player_agg(filter_per_player(season=2014))
#'   ppdc <- per_player_agg(filter_per_player_clutch(season=2014))
#' }
#'
#' @seealso \code{\link{filter_per_player}}
#'   \code{\link{filter_per_player_clutch}}
#'   \url{http://stats.nba.com/help/glossary}
#'
#' @export
per_player_agg <- function(filters=filter_per_player(),
                            clutch=FALSE,
                            measurement='base') {

    if (!valid_filters(filters))
        stop(paste('[statsnbaR per_player_agg] filters must be a valid',
                   'key-pair list recognised by statsnbaR'))

    if (!is.logical(clutch))
        stop('[statsnbaR per_player_agg] \'clutch\' must be logical') 

    clutch_str <- ''
    if (clutch) {
        # need to remove whatever measurement has been set
        filters <- filters[-which(names(filters) == 'measurement')]
        chk_length <- length(filters)
        filters <- do.call('filter_per_player_clutch',
                           filters)
        if(chk_length != length(filters)) 
            warning(paste('[statsnbaR per_player_agg] using default clutch',
                          'definition, to avoid this warning use',
                          'filter_player_clutch() to construct filters when',
                          '\'clutch=TRUE\''))
        clutch_str <- '_clutch'
    }
    
    if(!(measurement %in% c('base',          'advanced',
                            'miscellaneous', 'scoring',
                            'usage')))
            stop(paste('[statsnbaR per_player_agg] invalid \'measurement\'',
                       'type specified'))

    filters$measurement <- measurement
    if(filters$measurement == 'scoring') {
        if(filters$per != 'game') {
            warning(paste('[statsnbaR per_player_agg] coercing filter to',
                          '(per=\'game\')'))
        }
        filters$per <- 'game'
    }
    
    if(filters$measurement == 'usage') {
        if(filters$per != 'total') {
            warning(paste('[statsnbaR per_player_agg] coercing filter to',
                          '(per=\'total\')'))
        }
        filters$per <- 'total'
    }

    r <- api_scrape(paste0('per_player_', measurement, clutch_str),
                    filters=filters)

    if (length(r) != 1)
        stop(paste('[statsnbaR per_player_agg] unexpected number of',
                   'result sets returned by stats.nba.com'))

    return(r[[1]])
}
