library(statsnbaR)
context('get_player parses input')

test_that(
    'get_players fails when invalid season is entered',
    {
        expect_error(get_players(season=0),
                     paste('\\[statsnbaR get_players\\]',
                           0,
                           'is not a valid season.'))
    })

test_that(
    'get_players fails when league=WNBA',
    {
        expect_error(get_players(league='WNBA'),
                     paste('\\[statsnbaR get_players\\]',
                           'WNBA',
                           'is not a valid league code.'))
    })

