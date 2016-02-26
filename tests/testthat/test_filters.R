library(statsnbaR)
context('filter functions work')

test_that(
    'filter_per_player constructor creates a non-empty list',
    {
        expect_is(filter_per_player(), 'list')
    })

test_that(
    'filter_per_player constructor assigns default values',
    {
        default_filters <- filter_per_player()
        expect_true(length(default_filters) > 0)
        expect_true('league' %in% names(default_filters))
        expect_equal(default_filters$league,
                     statsnbaR.ADL.filters$league$default)
    })

test_that(
    'filter_per_player constructor checks for valid filter names',
    {
        expect_is(filter_per_player(college=NULL),
                  'list')
        expect_equal(filter_per_player(college='foo')$college,'foo')
        expect_error(filter_per_player(collage=NULL),
                     paste('\\[statsnbaR filter_per_player\\] invalid filters',
                           '\\(collage\\) specified for per player query\\.'))
        expect_error(filter_per_player(college=NULL,
                                       collage=NULL),
                     paste('\\[statsnbaR filter_per_player\\] invalid filters',
                           '\\(collage\\) specified for per player query\\.'))      
    })
    
test_that(
    'filter_per_player constructor fails if measurement or clutch is given',
    {
        expect_error(filter_per_player(measurement=NULL),
                     paste('\\[statsnbaR filter_per_player_worker\\] cannot',
                            'specify \'measurement\' as an argument to the',
                            'filter constructor'))
        expect_error(filter_per_player(clutch=NULL),
                     paste('\\[statsnbaR filter_per_player_worker\\] cannot',
                            'specify \'clutch\' as an argument to the filter',
                            'constructor'))   
    })
    
test_that(
    'filter_per_player_clutch constructor creates a non-empty list',
    {
        expect_is(filter_per_player_clutch(), 'list')
    })

test_that(
    'filter_per_player_clutch constructor assigns clutch values',
    {
        default_filters <- filter_per_player_clutch()
        expect_true(length(default_filters) > 0)
        expect_true('clutch_time' %in% names(default_filters))
        expect_equal(default_filters$clutch_time,
                     statsnbaR.ADL.filters$clutch_time$default)
 
    })

test_that(
    'filter_per_player_clutch constructor check for valid filter names',
    {
        expect_is(filter_per_player_clutch(point_diff=5),
                  'list')
        expect_equal(filter_per_player_clutch(point_diff='foo')$point_diff,
                     'foo')
        expect_error(filter_per_player_clutch(collage=NULL),
                     paste('\\[statsnbaR filter_per_player\\] invalid filters',
                           '\\(collage\\) specified for per player query\\.'))
    })
    
    
test_that(
    'filter_per_player_clutch constructor fails if measurement or clutch is given',
    {
        expect_error(filter_per_player_clutch(measurement=NULL),
                     paste('\\[statsnbaR filter_per_player_worker\\] cannot',
                            'specify \'measurement\' as an argument to the',
                            'filter constructor'))
        expect_error(filter_per_player_clutch(clutch=NULL),
                     paste('\\[statsnbaR filter_per_player_worker\\] cannot',
                            'specify \'clutch\' as an argument to the filter',
                            'constructor'))
    })
