#' Player data
#'
#' @name player
#' @include scrape.R
#' @keywords internal
NULL

#' Retrieve current and historical player data
#'
#' Retrieves the player data from stats.nba.com
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
                        'is not a valid season'))
         filters$season <- season
        
    }

    if (!missing(league)) {
        if(!(league %in% names(statsnbaR.ADL.filters$league$mapping)))
            stop(paste('[statsnbaR get_players]',
                        league,
                        'is not a valid league code'))
         filters$league <- league
    }

    api_scrape('PlayerCommon',
               filters=filters)
}
