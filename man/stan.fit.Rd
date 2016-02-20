\encoding{UTF-8}
\name{stan.fit}
\Rdversion{1.1}
\alias{stan.fit}
\title{
Fit Stan models with cloned data
}
\description{
Convenient functions designed to work well with
cloned data arguments and Stan.
}
\usage{
stan.fit(data, params, model, inits = NULL,
    seed = sample.int(.Machine$integer.max, 1),
    n.chains = 3,
    format = c("mcmc.list", "stanfit"),
    stan.model = TRUE, fit = NA, chain_id, ...)
}
\arguments{
  \item{data}{
A list (or environment) containing the data.
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function containing
the model, or a \code{\link[dclone]{custommodel}} object.
}
  \item{inits}{
Optional specification of initial values in the
form of a list or a function.
If \code{NULL}, initial values will be
generated automatically.
}
  \item{seed}{
Random seed.
}
  \item{n.chains}{
number of Markov chains.
}
  \item{format}{
Required output format.
}
  \item{stan.model}{
Logical, if stan model should be returned.
}
  \item{fit}{
Fitted Stan object.
}
  \item{chain_id}{
Chain id.
}
  \item{\dots}{
Further arguments.
}
}
\value{
By default, an \code{mcmc.list} object. If data cloning is used via the
\code{data} argument, \code{summary} returns a modified summary
containing scaled data cloning standard errors
(scaled by \code{sqrt(n.clones)}), and
\eqn{R_{hat}} values (as returned by \code{\link[coda]{gelman.diag}}).
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
Underlying functions:
\code{\link[rstan]{stan}} and
\code{\link[rstan]{stanfit}} in package \pkg{rstan}

Methods: \code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}},
\code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}},
\code{\link{vcov.mcmc.list.dc}}
}
\examples{
\dontrun{
}
}
\keyword{ models }
\keyword{ htest }
