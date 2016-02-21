# statsnbaR
_Stephen Wade_
22/02/2016

### stats.nba.com interface for R

This is a simple interface to stats.nba.com based off of the Python package
[nba_py](https://github.com/seemethere/nba_py), which is sadly outdated as
of February 2016.

Before going into any further details of the package, there are some
house-keeping tasks:

All the data from the website is Copyright (c) 2016 NBA Media Ventures, LLC.
All rights reserved. When using this package you must agree to the
[Terms of Use](http://www.nba.com/news/termsofuse.html) of the website.

As this package sends http requests to stats.nba.com, you should read their
their [Privacy Policy](http://www.nba.com/news/privacy_policy.html)
before using this package.

This code is licensed under the 
[MIT license](https://www.r-project.org/Licenses/MIT), and you may use this package
strictly under those terms.

The details of the API end-points were manually sourced by the approach
given in this 
[blog post](http://www.gregreda.com/2015/02/15/web-scraping-finding-the-api).
In order to semi-future-proof this, the queries to these end-points and
the data extracted from them are evaluated through a fairly informal
abstract data layer (ADL). The ADL is specified in internal data extracted,
from a YAML which can be viewed on github:
[github.com/stephematician/statsnbaR/data-raw](http://www.github.com/stephematician/statsnbaR/tree/master/data-raw).

The package is split into player, team and game data.
