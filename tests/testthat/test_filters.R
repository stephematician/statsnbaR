library(statsnbaR)
context('filter_functions')

test_that(
    'per_player_filter constructor creates a non-empty list',
    {
        expect_is(per_player_filters(), 'list')
    })

test_that(
    'per_player_filter constructor assigns default values',
    {
        default_filters <- per_player_filters()
        expect_true(length(default_filters) > 0)
        expect_true('league' %in% names(default_filters) > 0)
        expect_equal(default_filters$league,
                     statsnbaR.ADL.filters$league$default)
    })

test_that(
    'per_player_filter constructor recognises valid filters',
    {
        expect_is(per_player_filters(college=NULL),
                  'list')
        expect_error(per_player_filters(collage=NULL),
                     paste('\\[statsnbaR per_player_filters\\] invalid filters',
                           '\\(collage\\) specified for per player query\\.'))
        expect_error(per_player_filters(college=NULL,
                                        collage=NULL),
                     paste('\\[statsnbaR per_player_filters\\] invalid filters',
                           '\\(collage\\) specified for per player query\\.'))      
    })

