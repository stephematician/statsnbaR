#' Named type conversion functions
#'
#' Descriptiony
#'
#' 
type_converters = list(
    'date'=function(x) {
               if (!is.null(attr(x, 'tzone'))) {
                 return(as.Date(x, tz=attr(x, 'tzone')))
               }
               as.Date(x)
             },
    'last_name'=function(x) {
                    sub('^(.*),[\\s]*(.*)$', '\\1', x, perl=TRUE)
                 },
    'first_name'=function(x) {
                    sub('^(.*),[\\s]*(.*)$', '\\2', x, perl=TRUE)
                 },
    'posix_date'=as.POSIXct,
    'integer'=as.integer,
    'hexmode'=function(x) {
                  if (!length(x)) {
                      return(character(0))
                  }
                  as.hexmode(x)
              },
    'logical'=as.logical,
    'numeric'=as.numeric,
    'double'=as.double,
    'character'=as.character,
    'factor'=factor,
    'ordered'=ordered
)

#' Translate key-value pairs to the stats.nba.com API key-value pairs
#'
#' Translates the key-values in a named list of statsnbaR key-value pairs to
#' key-value pairs that the stats.nba.com API recognises.
#'
#' @param filters The statsnbaR filter key-value pairs.
#' @return Named list of key-values mapped to stats.nba.com API key-value
#' pairs.
map_filters <- function(filters,
                        api.endpoint) {

    api.filters <- map_filter_values(filters)

    names(api.filters) <- api.endpoint$api.filters[names(filters)]

    return(api.filters)
}

#' Translate values (only) to stats.nba.com API values
#'
#' Translates the values in a named list of statsnbaR key-value pairs to
#' values that the stats.nba.com API recognises.
#'
#' @param filters The statsnbaR filter key-value pairs
#' @return Named list of key-values with values mapped to stats.nba.com API
#' values
map_filter_values <- function(filters) {

    if (!all(names(filters) %in% names(statsnbaR.ADL.filters))) {
        flagged <- !(names(filters) %in% names(statsnbaR.ADL.filters))
        
        stop(paste('[statsnbaR scrape] invalid filters -',
                   paste(names(filters)[flagged], collapse=' '),
                   '- specified for statsnbaR data layer'))
    }


    mappings <- lapply(names(filters),
                       function(name) 
                           statsnbaR.ADL.filters[[name]]$mapping)

    # Only map values for which a mapping exists.
    mapped <- lapply(1:length(mappings),
                     function(j) {
                         if (!is.null(mappings[[j]])) {
                             mappings[[j]][[as.character(filters[[j]])]]
                         } else {
                             filters[[j]]
                         }
                     })
    names(mapped) <- names(filters)
    print(mapped)
    return(mapped)  
}

#' Flatten JSON result sets
#'
#' Takes a list provided by JSON and flattens the rowSet element to a
#' data.frame. This is acheived by forcing any NULL values to NA, and then
#' applying the unlist function. The data.frame is then returned with the
#' attribute names set to the headers specified in the JSON data.
#' 
#' @param json_result A list returned by JSON containing the rowSet and headers 
#' @return A data.frame for the flattened JSON rowSet
flatten_json_rowSet <- function(json_result) {
    a <- json_result$rowSet
    a <- lapply(a, 
                function(b) {
                    b[sapply(b, is.null)] <- NA
                    return(b)
                })
    a <- data.frame(matrix(unlist(a), nrow=length(a), byrow=TRUE))
    names(a) <- tolower(unlist(json_result$headers))

    return(a)
}

#' Map flattened NBA API data to statsnbaR data
#'
#' Here's a description.
#'
#' @param df Flattened NBA API data
#' @param api.mapping The mapping from API data to statsnbaR data
#' @return The data frame with mapped values
map_results <- function(df,
                        api.mapping) {

    ADL_df <- data.frame(df[, unlist(api.mapping)])

    names(ADL_df) <- names(api.mapping)

    map_result_values(ADL_df)

}

#' Title of this thing
#' 
#' A description of this thing
#'
#' 
map_result_values <- function(df) {

    mapped_df <- data.frame()

    mapped_df <- do.call(cbind,
            lapply(names(df),
                   function(k) {
                       mapping <- statsnbaR.ADL.data[[k]]$mapping
                       tc <- type_converters[[statsnbaR.ADL.data[[k]]$class]]

                       nc <- df[,k]
                       if (!is.null(mapping))
                           nc <- mapping[as.character(nc)]
                       if (!is.null(tc))
                           nc <- tc(nc)
                       return(data.frame(nc))
                   })
            )

    names(mapped_df) <- names(df)

    return(mapped_df)
}



