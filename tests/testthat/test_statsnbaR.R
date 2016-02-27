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
'player biography filters are available',
{
    expect_true(exists('filter_bio'))
})

test_that(
'player biography query function is available',
{
    expect_true(exists('get_bio'))
})


test_that(
'per player aggregated data filters are available',
{
    expect_true(exists('filter_per_player'))
    expect_true(exists('filter_per_player_clutch'))
})

test_that(
'per player query aggregated data query functions are available',
{
    expect_true(exists('per_player_agg'))
})

