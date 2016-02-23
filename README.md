statsnbaR
--------------
_Stephen Wade 22/02/2016_

### R interface to stats.nba.com

This is a simple interface to stats.nba.com based off of the Python package
[nba_py](https://github.com/seemethere/nba_py), which is sadly outdated as
of February 2016.

Before going into any further details of the package, there are some
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
abstract data layer (ADL). The ADL is specified in internal data extracted,
from a YAML which can be viewed on github:
<http://www.github.com/stephematician/statsnbaR/tree/master/data-raw>.

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
Let's just have a look at the player data from the 2015-2016 season,
including the historical players:
```r
library(statsnbaR)
nba_players <- get_players(league='NBA')

tail(nba_players)
```

```

```

### Team data examples

### Game log examples

### Shot chart examples

