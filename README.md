# dclone: Data Cloning and MCMC Tools for Maximum Likelihood Methods

[![CRAN version](http://www.r-pkg.org/badges/version/dclone)](http://cran.rstudio.com/web/packages/dclone/index.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/dclone)](http://www.rdocumentation.org/packages/dclone)
[![Linux build status](https://travis-ci.org/datacloning/dclone.svg?branch=master)](https://travis-ci.org/datacloning/dclone)

Low level functions for implementing
maximum likelihood estimating procedures for
complex models using data cloning and Bayesian
Markov chain Monte Carlo methods
as described in Solymos 2010 ([R Journal 2(2):29--37](http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf)).
Sequential and parallel MCMC support
for JAGS, WinBUGS, OpenBUGS, and Stan.

Find help on the [Dclone users mailing list](https://groups.google.com/forum/#!forum/dclone-users).
More resources at [datacloning.org](http://datacloning.org).

## Versions

Install the CRAN version of the package from R:

```R
install.packages("dclone")
```

Install the development version of the package:

```R
remotes::install_github("datacloning/dclone")
```

User visible changes in the package are listed in the [NEWS](https://github.com/datacloning/dclone/blob/master/NEWS.md) file.

## Report a problem

Use the [issue tracker](https://github.com/datacloning/dclone/issues)
to report a problem.

## References

Solymos, P., 2010. dclone: Data Cloning in R. R Journal 2(2):29--37. [[PDF](https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf)]
