#' Game data functions
#'
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
#' This function returns a set of player measurements for each game in a
#' given \code{league}, \code{season} and \code{season_type} which indicates
#' whether to query the regular season, playoffs, allstar game or preseason.
#'
#' Each game returned has a unique \code{game_id} which can be used for queries
#' to play-by-play or shot-chart datasets.
#'
#' The player statistics returned are given as total values for the game.
#'
#' @examples \dontrun{
#'     df <- player_game_logs(league='nba',
#'                            season=2013,
#'                            season_type='playoffs')
#'     tail(df)
#' }
#'
#' @param league A character value of league e.g. 'nba', 'd-league'.
#' @param season A numeric value of the base season, e.g. 2015 for the 2015-2016
#'   season and so on.
#' @param season_type A character value for the type of season, valid types are
#'   \code{regular}, \code{allstar}, \code{playoffs} and \code{preseason}.
#' @param method The function to use for retrieving data, must accept two
#'               arguments, \code{url} and \code{headers} .....
#'
#' @param ... Further arguments to pass on to the \code{method} function.
#' @return A data.frame containing the data for each game and each player with
#'   columns converted to the data types specified by statsnbaR's internal
#'   YAML.
#'   \describe{
#'      \item{game_season}{integer - an id code for the season, e.g. 22015 for
#'        the 2015-16 regular season, 12015 for the pre-season and so forth}
#'      \item{person_id}{integer - ID of the player}
#'      \item{player_name}{character - the player's first and last name}
#'      \item{team_abbr}{character - abbrev. name of the team}
#'      \item{team_name}{character - name of team that player belonged to in the
#'        game}
#'      \item{game_id}{numeric - unique numeric identifier of the game}
#'      \item{game_date}{date - date game was played (or scheduled)}
#'      \item{game_at_home}{logical - home (true) or away game (false)}
#'      \item{win}{logical - win for player (true) or loss (false)}
#'      \item{mins}{numeric - minutes player was on court in game}
#'   }
#' All other returned columns are described in the glossary provided at
#' \url{http://stats.nba.com/help/glossary}.
#'
#' @seealso \code{\link{team_game_logs}}
#' \url{http://stats.nba.com/help/glossary}
#'
#' @export
player_game_logs <- function(league, season, season_type, method=NULL, ...) {

    if (!is.null(method))
        if (!is.function(method))
            stop(paste('[statsnbaR player_game_logs] method should be',
                       'supplied as a function.'))

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
            stop(paste('[statsnbaR player_game_logs]', season_type, 'is not',
                       'a valid season type.'))
        filters$season_type <- season_type
    }

    if (!missing(season)) {
        if (!valid_filters(list(season=season), allow_na=FALSE))
            stop(paste('[statsnbaR player_game_logs]', season, 'is not a',
                       'valid season.'))
        filters$season <- season
    }

    if (!missing(league)) {
        if (!valid_filters(list(league=league)))
            stop(paste('[statsnbaR player_game_logs]', league, 'is not a',
                       'valid league name.'))
        filters$league <- league
    }

    r <- api_scrape('player_game_log', filters=filters, method=method, ...)

    if (!(length(r) == 1))
        stop(paste('[statsnbaR player_game_logs] unexpected number of result',
                   'sets returned by stats.nba.com'))

    return(r[[1]])

}

#' Retrieve team game logs
#'
#' @description
#' Retrieves all team game logs from stats.nba.com for a given season and
#' league. 
#'
#' @details
#' This function returns a set of measurements of a team for each game in a
#' given \code{league}, \code{season} and \code{season_type} which indicates
#' whether to query the regular season, playoffs, allstar game or preseason.
#' All games will be returned twice, one for the home team entry and the other
#' for the away team.
#'
#' Each game returned has a unique \code{game_id} which can be used for queries
#' to play-by-play or shot-chart datasets.
#'
#' The team statistics returned are given as total values for the game.
#'
#' @examples \dontrun{
#'     df <- player_game_logs(league='nba',
#'                            season=2013,
#'                            season_type='playoffs')
#'     tail(df)
#' }
#'
#' @param league A character value of league e.g. 'nba', 'd-league'.
#' @param season A numeric value of the base season, e.g. 2015 for the 2015-2016
#'   season and so on.
#' @param season_type A character value for the type of season, valid types are
#'   \code{regular}, \code{allstar}, \code{playoffs} and \code{preseason}.
#' @param method Optional user-supplied function to retrieve JSON from 
#'               stats.nba.com
#' @return A data.frame containing the data for each game and team with
#'   columns converted to the data types specified by statsnbaR's internal
#'   YAML.
#'   \describe{
#'      \item{game_season}{integer - an id code for the season, e.g. 22015 for
#'        the 2015-16 regular season, 12015 for the pre-season and so forth}
#'      \item{team_id}{integer - unique identifier of the team}
#'      \item{team_abbr}{character - abbrev. name of the team}
#'      \item{team_name}{character - name of team that player belonged to in the
#'        game}
#'      \item{game_id}{numeric - unique numeric identifier of the game}
#'      \item{game_date}{date - date game was played (or scheduled)}
#'      \item{game_at_home}{logical - home (true) or away game (false)}
#'      \item{win}{logical - win for the team (true) or loss (false)}
#'      \item{mins}{numeric - minutes the team played on court in game}
#'   }
#' All other returned columns are described in the glossary provided at
#' \url{http://stats.nba.com/help/glossary}.
#'
#' @seealso \code{\link{player_game_logs}}
#' \url{http://stats.nba.com/help/glossary}
#'
#' @export
team_game_logs <- function(league, season, season_type, method=NULL, ...) {

    if (!is.null(method))
        if (!is.function(method))
            stop(paste('[statsnbaR team_game_logs] method should be supplied',
                       'as a function.'))

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
            stop(paste('[statsnbaR team_game_logs]', season_type, 'is not a',
                       'valid season type.'))
        filters$season_type <- season_type
    }

    if (!missing(season)) {
        if (!valid_filters(list(season=season), allow_na=FALSE))
            stop(paste('[statsnbaR team_game_logs]', season, 'is not a valid',
                       'season.'))
        filters$season <- season
    }

    if (!missing(league)) {
        if (!valid_filters(list(league=league)))
            stop(paste('[statsnbaR team_game_logs]', league, 'is not a valid',
                       'league name.'))
        filters$league <- league
    }

    r <- api_scrape('team_game_log', filters=filters, method=method, ...)

    if (!(length(r) == 1))
        stop(paste('[statsnbaR team_game_logs] unexpected number of result',
                   'sets returned by stats.nba.com'))

    return(r[[1]])

}

