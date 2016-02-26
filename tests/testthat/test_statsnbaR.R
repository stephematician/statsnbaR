library(statsnbaR)
context('statsnbaR functions')

test_that(
'scraping function is loaded',
{
    expect_true(exists('api_scrape'))
})

test_that(
'player data function is loaded',
{
    expect_true(exists('get_players'))
})

test_that(
'per player query functions are available',
{
    expect_true(exists('per_player_filters'))
    expect_true(exists('per_player_base'))
    expect_true(exists('per_player_advanced'))
    expect_true(exists('per_player_scoring'))
    expect_true(exists('per_player_usage'))
})

