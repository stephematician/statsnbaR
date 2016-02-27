library(statsnbaR)
context('game log functions parse arguments')

test_that(
    'player_game_logs fails when invalid season is entered',
    {
        expect_error(player_game_logs(season=NA),
                     paste('\\[statsnbaR player_game_logs\\]',
                           NA,
                           'is not a valid season.'))
    })

test_that(
    'player_game_logs fails when league=\'foo\'',
    {
        expect_error(player_game_logs(league='foo'),
                     paste('\\[statsnbaR player_game_logs\\]',
                           'foo',
                           'is not a valid league name.'))
    })

test_that(
    'player_game_logs fails when season_type=\'foo\'',
    {
        expect_error(player_game_logs(season_type='foo'),
                     paste('\\[statsnbaR player_game_logs\\]',
                           'foo',
                           'is not a valid season type.'))
    })

test_that(
    'team_game_logs fails when invalid season is entered',
    {
        expect_error(team_game_logs(season=NA),
                     paste('\\[statsnbaR team_game_logs\\]',
                           NA,
                           'is not a valid season.'))
    })

test_that(
    'team_game_logs fails when league=\'foo\'',
    {
        expect_error(team_game_logs(league='foo'),
                     paste('\\[statsnbaR team_game_logs\\]',
                           'foo',
                           'is not a valid league name.'))
    })

test_that(
    'team_game_logs fails when season_type=\'foo\'',
    {
        expect_error(team_game_logs(season_type='foo'),
                     paste('\\[statsnbaR team_game_logs\\]',
                           'foo',
                           'is not a valid season type.'))
    })
