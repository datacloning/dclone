\name{parUpdate}
\alias{parUpdate}
\title{Update jags models on parallel workers}
\description{
  Update the Markov chain associated with the model on parallel workers.
  (This represents the 'burn-in' phase when nodes are not monitored.)
}
\usage{
parUpdate(cl, object, n.iter=1, \ldots)
}
\arguments{
  \item{cl}{
    A cluster object created by \code{\link[parallel]{makeCluster}}, or
    an integer. It can also be \code{NULL},
    see \code{\link{parDosa}}.
  }
  \item{object}{character, name of a jags model object}
  \item{n.iter}{number of iterations of the Markov chain to run}
  \item{\ldots}{additional arguments to the update method, see
    \code{\link[rjags]{update.jags}}}
}
\value{
  The \code{parUpdate} function modifies the
  original object on parallel workers and returns \code{NULL}.
}
\author{Peter Solymos}
\seealso{\code{\link[rjags]{update.jags}}

See example on help page of \code{\link{parCodaSamples}}.}
\keyword{models}
