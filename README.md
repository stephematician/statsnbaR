statsnbaR
--------------
_Stephen Wade 22/02/2016_

### R interface to stats.nba.com

This is a simple interface to stats.nba.com.

Before going into any further details of this package, there are some
house-keeping tasks:

  1. All the data from the website is Copyright (c) 2016 NBA Media Ventures, LLC.
All rights reserved. When using this package you must agree to the
[Terms of Use](http://www.nba.com/news/termsofuse.html) of the website. All the
terms are important and must be read and agreed to. Pay extra attention to:
    * Section 1 - Ownership and User Restrictions;
    * Section 9 - NBA Statistics;
    * Section 11--14 and 16--21; and
    * read all of it, really!
  2. As this package sends http requests to
  [stats.nba.com](http://stats.nba.com), you must read and agree to the terms of
their [Privacy Policy](http://www.nba.com/news/privacy_policy.html)
before using this package.
  3. This code is licensed under the 
[MIT license](https://www.r-project.org/Licenses/MIT), and you may use this package
strictly under those terms.

The details of the API end-points were manually sourced by the approach
given in this 
[blog post](http://www.gregreda.com/2015/02/15/web-scraping-finding-the-api).
In order to semi-future-proof the package, the queries to these end-points and
the data extracted from them are evaluated through a fairly informal
abstract data layer (ADL). The ADL is specified in internal data extracted
from a YAML which can be viewed on github:
<http://www.github.com/stephematician/statsnbaR/tree/master/data-raw/ADL.yaml>.

The package is split into player, team and game data. All the functions are
fully documented, and so details can be found there. However for the sake of
sales, here's some example code to give you an idea how easy it is to use the
package

### Installation

Installation is performed via github using the `devtools` package.
```r
library(devtools)
install_github('stephematician/statsnbaR')
```

### Player data examples
Let's just have a look at the player data from the 2015-2016 season, and
select the D-League players who are active:
```r
library(statsnbaR)
library(dplyr)
dleague_players <- get_players(league='d-league')

head(filter(dleague_players, roster_status==TRUE) %>%
     select(person_id, first_name, last_name, team_id, team_city, team_name))
```

| person_id|first_name |last_name     |    team_id|team_city   |team_name |
|---------:|:----------|:-------------|----------:|:-----------|:---------|
|    201861|Antoine    |Agudio        | 1612709893|Canton      |Charge    |
|   1627378|Mychal     |Ammons        | 1612709903|Idaho       |Stampede  |
|    203648|Thanasis   |Antetokounmpo | 1612709919|Westchester |Knicks    |
|    203951|Keith      |Appling       | 1612709913|Erie        |BayHawks  |
|   1626276|Darion     |Atkins        | 1612709919|Westchester |Knicks    |
|   1627359|Eric       |Atkins        | 1612709913|Erie        |BayHawks  |

  - The `person_id` and `team_id` are important keys for other data such as 
    shot charts.

#### Traditional statistics

We might also want to know their traditional stats for their last 10 games, in,
say a previous season (just to show you how to do this kind of query)

```r
filter_last10 = filter_per_player(league='d-league',
                                   last_n=10,
                                   season=2014)
ppd_last10_2014 = per_player_data(filter_last10)

ppd_last10_2014 = filter(ppd_last10_2014,
                         person_id %in% dleague_players$person_id) %>%
                  select(-c(person_id, team_id))
```

|player_name    |team_abbr | age| games| win| loss|     mins| fgm| fga| fg3m| fg3a| ftm| fta| oreb| dreb| reb| ast| tov| stl| blk| blka| pf| pfd| points| plus_minus| dd2| td3|
|:--------------|:---------|---:|-----:|---:|----:|--------:|---:|---:|----:|----:|---:|---:|----:|----:|---:|---:|---:|---:|---:|----:|--:|---:|------:|----------:|---:|---:|
|A.J. Davis     |SXF       |  27|     9|   6|    3| 102.6350|   6|  29|    1|   14|   5|  10|    0|   16|  16|   3|   7|   2|   2|    2|  5|   9|     18|         -3|   0|   0|
|Aaron Craft    |SCW       |  24|    10|   8|    2| 363.7950|  36|  77|    9|   23|  29|  38|    7|   48|  55|  56|  19|  19|   4|    5| 27|  31|    110|         94|   2|   1|
|Adonis Thomas  |GRD       |  22|    10|   5|    5| 404.8900|  85| 183|   25|   73|  25|  28|   10|   57|  67|  13|  18|  10|   4|    4| 13|  24|    220|         -7|   1|   0|
|Adrian Thomas  |BAK       |  28|    10|   5|    5| 182.2250|  18|  52|   16|   46|   0|   0|    1|   15|  16|   7|   3|   5|   0|    0| 17|   3|     52|        -87|   0|   0|
|Akeem Richmond |RGV       |  24|     2|   1|    1|   6.8500|   0|   4|    0|    4|   0|   0|    0|    0|   0|   0|   0|   0|   0|    0|  0|   0|      0|         -8|   0|   0|
|Akil Mitchell  |RGV       |  23|    10|   5|    5| 217.0017|  30|  52|    1|    5|   9|  29|   26|   61|  87|  15|  15|   6|   5|    2| 22|  20|     70|        -10|   1|   0|

We can see that Aaron Craft was ballin' in the last ten games of the 2014-15
D-League season. Did he ever play the NBA?
```r
# 2015-16 is the default
nba_players = get_players(league='nba')
filter(nba_players, last_name=='Craft')
```

Aww, empty data.frame. Oh well!

#### Advanced statistics

You can also get more advanced statistics by specifying `measurement='advanced'`
in the call to per_player_data.
```r
ppad_last10_2014 = per_player_data(filter_last10,
                                    measurement='advanced')
```

Again selecting only the D-league players that played this year:

|player_name    |team_abbr | age| games| win| loss| mins| off_rtg| def_rtg| net_rtg| ast_pct| ast/tov| ast_ratio| oreb_pct| dreb_pct| reb_pct| tov_pct| EFG_pct| ts_pct| usg_pct|   pace|    PIE|
|:--------------|:---------|---:|-----:|---:|----:|----:|-------:|-------:|-------:|-------:|-------:|---------:|--------:|--------:|-------:|-------:|-------:|------:|-------:|------:|------:|
|A.J. Davis     |SXF       |  27|     9|   6|    3| 11.4|   107.0|   106.0|     1.0|   0.041|    0.43|       6.9|    0.000|    0.165|   0.089|    16.1|   0.224|  0.269|   0.181|  95.97|  0.000|
|Aaron Craft    |SCW       |  24|    10|   8|    2| 36.4|   111.6|   100.5|    11.1|   0.196|    2.95|      33.2|    0.021|    0.122|   0.075|    11.3|   0.526|  0.587|   0.126| 104.73|  0.098|
|Adonis Thomas  |GRD       |  22|    10|   5|    5| 40.5|   110.3|   109.9|     0.4|   0.049|    0.72|       5.7|    0.028|    0.145|   0.090|     8.0|   0.533|  0.563|   0.232|  98.79|  0.114|
|Adrian Thomas  |BAK       |  28|    10|   5|    5| 18.2|    94.1|   116.4|   -22.4|   0.058|    2.33|      11.3|    0.005|    0.096|   0.045|     4.8|   0.500|  0.500|   0.126| 104.77|  0.034|
|Akeem Richmond |RGV       |  24|     2|   1|    1|  3.4|    66.7|   115.1|   -48.4|   0.000|    0.00|       0.0|    0.000|    0.000|   0.000|     0.0|   0.000|  0.000|   0.267| 107.35| -0.160|
|Akil Mitchell  |RGV       |  23|    10|   5|    5| 21.7|   113.8|   115.0|    -1.2|   0.088|    1.00|      15.8|    0.112|    0.289|   0.196|    15.8|   0.587|  0.540|   0.141| 107.48|  0.093|

#### Setting `per` game, possession, minute

You might notice that the results for the advanced stats appear to be in
per-game format, whereas for the basic stats they were in totals. You can
actually get per-game traditional stats, too, by setting `per='game'` in the
filter. This is what the output would look like.

|player_name    |team_abbr | age| games| win| loss| mins| fgm|  fga| fg3m| fg3a| ftm| fta| oreb| dreb| reb| ast| tov| stl| blk| blka|  pf| pfd| points| plus_minus| dd2| td3|
|:--------------|:---------|---:|-----:|---:|----:|----:|---:|----:|----:|----:|---:|---:|----:|----:|---:|---:|---:|---:|---:|----:|---:|---:|------:|----------:|---:|---:|
|A.J. Davis     |SXF       |  27|     9|   6|    3| 11.4| 0.7|  3.2|  0.1|  1.6| 0.6| 1.1|  0.0|  1.8| 1.8| 0.3| 0.8| 0.2| 0.2|  0.2| 0.6| 1.0|    2.0|       -0.3|   0|   0|
|Aaron Craft    |SCW       |  24|    10|   8|    2| 36.4| 3.6|  7.7|  0.9|  2.3| 2.9| 3.8|  0.7|  4.8| 5.5| 5.6| 1.9| 1.9| 0.4|  0.5| 2.7| 3.1|   11.0|        9.4|   2|   1|
|Adonis Thomas  |GRD       |  22|    10|   5|    5| 40.5| 8.5| 18.3|  2.5|  7.3| 2.5| 2.8|  1.0|  5.7| 6.7| 1.3| 1.8| 1.0| 0.4|  0.4| 1.3| 2.4|   22.0|       -0.7|   1|   0|
|Adrian Thomas  |BAK       |  28|    10|   5|    5| 18.2| 1.8|  5.2|  1.6|  4.6| 0.0| 0.0|  0.1|  1.5| 1.6| 0.7| 0.3| 0.5| 0.0|  0.0| 1.7| 0.3|    5.2|       -8.7|   0|   0|
|Akeem Richmond |RGV       |  24|     2|   1|    1|  3.4| 0.0|  2.0|  0.0|  2.0| 0.0| 0.0|  0.0|  0.0| 0.0| 0.0| 0.0| 0.0| 0.0|  0.0| 0.0| 0.0|    0.0|       -4.0|   0|   0|
|Akil Mitchell  |RGV       |  23|    10|   5|    5| 21.7| 3.0|  5.2|  0.1|  0.5| 0.9| 2.9|  2.6|  6.1| 8.7| 1.5| 1.5| 0.6| 0.5|  0.2| 2.2| 2.0|    7.0|       -1.0|   1|   0|

You can see that the minutes value now matches up and looks like a per-game
number of minutes.

There are a couple of things worth noticing about the data returned by the 
queries:

  - The data returned by JSON seems to be clearly character based, so total
    values might be more useful for any careful analysis as they won't
    suffer whatever rounding stats.nba.com offers
  - Also notice that percentages are given as not-percentages, e.g. they are
    given as the actual raw 'rates' between 0 and 1.
  - Wins, losses, etc - are not subject to the usual `per='game'` rules, i.e.
    these values always return totals.
    
In general, it is always worth looking at a few records of the returned data
to get an idea what units the values are actually returning for different
queries.


### Team data examples

### Game log examples

These are broken into two sets; the player game log or the team game
log.

#### Player game log

Game log queries are relatively similar to getting the names and details of
players via `get_players()`.
```r
gl_2013 = get_game_log(league='nba', season=2013)
head(select(gl_2013, -c(season, team_abbr, team_name, fgm:plus_minus)))
```
And the output produced looks something like:

| person_id|player_name     |  game_id|game_date  |matchup     |win   | mins| video|
|---------:|:---------------|--------:|:----------|:-----------|:-----|----:|:-----|
|      2546|Carmelo Anthony | 21300640|2014-01-24 |NYK vs. CHA |TRUE  |   39|TRUE  |
|      2544|LeBron James    | 21300893|2014-03-03 |MIA vs. CHA |TRUE  |   41|TRUE  |
|    201142|Kevin Durant    | 21300592|2014-01-17 |OKC vs. GSW |TRUE  |   44|TRUE  |
|    201147|Corey Brewer    | 21301183|2014-04-11 |MIN vs. HOU |TRUE  |   45|TRUE  |
|    201142|Kevin Durant    | 21301024|2014-03-21 |OKC @ TOR   |TRUE  |   52|TRUE  |
|    203082|Terrence Ross   | 21300647|2014-01-25 |TOR vs. LAC |FALSE |   44|TRUE  |

  - The `game_id` is an important key for other data such as 
    shot charts, and play-by-play data.
  - _This is a large set of data, so if you only want `game_id`s, then team game
    logs is one easier approach_.

#### Team game logs

This is the same as the player game log, as an example only, I've also selected
a `season_type` here - which you may also do for player game logs.

_This is a much smaller set of data, so if you only want `game_id`s this would
be the way to retrieve them_.


### Play-by-play examples

### Shot chart examples

### Notes

#### Alternatives

An Alternative to this package is the more highly featured
[nbastatsR](https://www.github.com/abresler/nbastatR) package, definitely
worth a look.

This package was partly inspired by
[nba_py](https://github.com/seemethere/nba_py) which, sadly, seems outdated.

#### Testing platform

This package was built and tested with the following R software:
```
> sessionInfo()
R version 3.2.3 (2015-12-10)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 14.04.4 LTS

other attached packages:
[1] statsnbaR_0.1 httr_1.1.0

loaded via a namespace (and not attached):
[1] R6_2.1.2        curl_0.9.6      jsonlite_0.9.19
```
