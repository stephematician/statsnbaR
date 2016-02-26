library(statsnbaR)
context('player data functions parse arguments')

test_that(
    'get_players fails when invalid season is entered',
    {
        expect_error(get_players(season=0.1),
                     paste('\\[statsnbaR get_players\\]',
                           0.1,
                           'is not a valid season.'))
    })

test_that(
    'get_players fails when league=\'foo\'',
    {
        expect_error(get_players(league='foo'),
                     paste('\\[statsnbaR get_players\\]',
                           'foo',
                           'is not a valid league code.'))
    })

test_that(
    'per_player_data checks inputs',
    {
        expect_error(per_player_data(clutch='foo'),
                     paste('\\[statsnbaR per_player_data\\] \'clutch\' must',
                           'be logical'))
        expect_error(per_player_data(measurement='foo'),
                     paste('\\[statsnbaR per_player_data\\] invalid',
                           '\'measurement\' type specified'))
        expect_error(per_player_data(filters=NULL),
                     paste('\\[statsnbaR per_player_data\\] filters must be a',
                           'valid key-pair list recognised by statsnbaR'))
        expect_error(per_player_data(filter_per_player(league='foo')),
                     paste('\\[statsnbaR per_player_data\\] filters must be a',
                           'valid key-pair list recognised by statsnbaR'))
    })

test_that(
    'per_player_data checks inputs when clutch is true',
    {
        expect_error(per_player_data(filter_per_player(league='foo'),
                                     clutch=TRUE),
                     paste('\\[statsnbaR per_player_data\\] filters must be a',
                           'valid key-pair list recognised by statsnbaR'))
        expect_warning(per_player_data(filter_per_player(),
                                       clutch=TRUE),
                       paste('\\[statsnbaR per_player_data\\] using default',
                             'clutch definition, to avoid this warning use',
                             'filter_player_clutch\\(\\) to construct filters',
                             'when \'clutch=TRUE\''))
    })

test_that(
    'per_player_data fails when filters are clutch but clutch is false',
    {
        expect_error(per_player_data(filters=filter_per_player_clutch()),
                     paste('invalid filters.+specified for stats\\.nba\\.com',
                           'endpoint.+'))
    })

