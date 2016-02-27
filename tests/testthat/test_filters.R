library(statsnbaR)
context('filter functions work')

test_that(
    'filter_bio constructor creates a non-empty list',
    {
        expect_is(filter_bio(), 'list')
    })

test_that(
    'filter_bio constructor assigns default values',
    {
        default_filters <- filter_bio()
        expect_true(length(default_filters) > 0)
        expect_true('season' %in% names(default_filters))
        expect_equal(default_filters$season,
                     statsnbaR.ADL.filters$season$default)
    })
    
test_that(
    'filter_bio constructor checks for valid filter names',
    {
        expect_is(filter_bio(college=NULL),
                  'list')
        expect_equal(filter_bio(country='foo')$country,'foo')
        expect_error(filter_bio(countyr=NULL),
                     paste('invalid filters',
                           '\\(countyr\\) specified for query to endpoint\\.'))
        expect_error(filter_bio(country=NULL,
                                countyr=NULL),
                     paste('invalid filters',
                           '\\(countyr\\) specified for query to endpoint\\.'))      
    })

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
                     paste('invalid filters',
                           '\\(collage\\) specified for query to endpoint\\.'))
        expect_error(filter_per_player(college=NULL,
                                       collage=NULL),
                     paste('invalid filters',
                           '\\(collage\\) specified for query to endpoint\\.'))      
    })
    
test_that(
    'filter_per_player constructor fails if measurement or clutch is given',
    {
        expect_error(filter_per_player(measurement=NULL),
                     paste('\\[statsnbaR filter_per_player\\] cannot',
                            'specify \'measurement\' as an argument to the',
                            'filter constructor'))
        expect_error(filter_per_player(clutch=NULL),
                     paste('\\[statsnbaR filter_per_player\\] cannot',
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
        # said with new zealand accent
        expect_error(filter_per_player_clutch(point_duff=NULL),
                     paste('invalid filters',
                           '\\(point_duff\\) specified for query to',
                           'endpoint\\.'))
    })
    
