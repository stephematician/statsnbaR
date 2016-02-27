#' Game data functions
#' TODO play by play
#'
#' @name games
#' @include utils.R
#' @include scrape.R
#' @include statsnbaR-package.R
#' @keywords internal
NULL



#' Retrieve player game logs
#'
#' @description
#' Retrieves all player game logs from stats.nba.com for a given season and
#' league.
#'
#' @details
#' TODO This function returns per-player game data, which is mainly useful for
#' retrieving \code{game_id} values if per-game shot charts are desired.
#'
#' @examples \dontrun{
#'     df <- get_game_log(league='nba',
#'                        season=2013,
#'                        season_type='playoffs')
#'     tail(df)
#' }
#'
#' @param league A character value of league e.g. 'nba', 'd-league'.
#' @param season A numeric value of the base season, e.g. 2015 for the 2015-2016
#'   season and so on.
#' @param season_type A character value for the type of season, valid types are
#'   \code{regular}, \code{allstar}, \code{playoffs} and \code{preseason}.
#' @return TODO A data.frame with names of current and historical players and the
#'   fields
#'   \describe{
#'      \item{person_id}{integer - ID of the player}
#'      \item{first_name}{character - the player's first name}
#'      \item{last_name}{character - the player's last name}
#'      \item{roster_status}{logical - currently rostered in season selected}
#'      \item{year_start}{integer - year player entered NBA}
#'      \item{year_end}{integer - year player exited NBA}
#'      \item{team_id}{integer - ID of the player's current team (if 
#'        roster_status is true)}
#'      \item{team_city}{character - city of current team (if roster_status
#'        is true)}
#'      \item{team_name}{character - name of current team (if roster_status is
#'        true)}
#'      \item{team_abbr}{character - abbrev. name of current team (if
#'        roster_status is true)}
#'      \item{has_played}{logical - did the player record at least one game in
#'        NBA}
#'   }
#'
#' @seealso \code{\link{team_game_logs}}
#' @export
player_game_logs <- function(league,
                             season,
                             season_type) {

    # At the moment, I figure if the user wants to sort the data another way,
    # they can just use any one of R's awesome functions for that.
    filters <- list(counter     = statsnbaR.ADL.filters$counter$default,
                    desc        = TRUE,
                    league      = statsnbaR.ADL.filters$league$default,
                    log_for     = "player",
                    season      = statsnbaR.ADL.filters$season$default,
                    season_type = statsnbaR.ADL.filters$season_type$default,
                    sort_on     = statsnbaR.ADL.filters$sort_on$default)

    if (!missing(season_type)) {
        if (!valid_filters(list(season_type=season_type)))
            stop(paste('[statsnbaR player_game_logs]',
                       season_type,
                       'is not a valid season type.'))
        filters$season_type <- season_type
    }

    if (!missing(season)) {
        if (!valid_filters(list(season=season),
                           allow_na=FALSE))
            stop(paste('[statsnbaR player_game_logs]',
                       season,
                       'is not a valid season.'))
        filters$season <- season
    }

    if (!missing(league)) {
        if (!valid_filters(list(league=league)))
            stop(paste('[statsnbaR player_game_logs]',
                       league,
                       'is not a valid league name.'))
        filters$league <- league
    }

    r <- api_scrape('game_log', filters=filters)

    if (length(r) != 1) stop(paste('[statsnbaR player_game_logs] unexpected',
                                   'number of result sets returned by',
                                   'stats.nba.com'))
    return(r[[1]])
}

#' Retrieve player game logs
#'
#' @description
#' Retrieves all player game logs from stats.nba.com for a given season and
#' league. 
#'
#' @details
#' TODO
#'
#' @param league TODO
#' @param season TODO
#' @param season_type TODO
#' @returns TODO
#' @seealso \code{\link{player_game_logs}}
#' @export
team_game_logs <- function(league,
                           season,
                           season_type) {

    # At the moment, I figure if the user wants to sort the data another way,
    # they can just use any one of R's awesome functions for that.
    filters <- list(counter     = statsnbaR.ADL.filters$counter$default,
                    desc        = TRUE,
                    league      = statsnbaR.ADL.filters$league$default,
                    log_for     = "team",
                    season      = statsnbaR.ADL.filters$season$default,
                    season_type = statsnbaR.ADL.filters$season_type$default,
                    sort_on     = statsnbaR.ADL.filters$sort_on$default)

    if (!missing(season_type)) {
        if (!valid_filters(list(season_type=season_type)))
            stop(paste('[statsnbaR team_game_logs]',
                       season_type,
                       'is not a valid season type.'))
        filters$season_type <- season_type
    }

    if (!missing(season)) {
        if (!valid_filters(list(season=season),
                           allow_na=FALSE))
            stop(paste('[statsnbaR team_game_logs]',
                       season,
                       'is not a valid season.'))
        filters$season <- season
    }

    if (!missing(league)) {
        if (!valid_filters(list(league=league)))
            stop(paste('[statsnbaR team_game_logs]',
                       league,
                       'is not a valid league name.'))
        filters$league <- league
    }

    r <- api_scrape('game_log', filters=filters)

    if (length(r) != 1) stop(paste('[statsnbaR team_game_logs] unexpected',
                                   'number of result sets returned by',
                                   'stats.nba.com'))
    return(r[[1]])
}
