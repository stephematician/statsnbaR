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
'per player filter functions are available',
{
    expect_true(exists('filter_per_player'))
    expect_true(exists('filter_per_player_clutch'))
})

test_that(
'per player query functions are available',
{
    expect_true(exists('per_player_data'))
})

