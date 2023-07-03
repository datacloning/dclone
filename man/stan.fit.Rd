\encoding{UTF-8}
\name{stan.fit}
\Rdversion{1.1}
\alias{stan.fit}
\alias{stan.parfit}
\alias{stan.model}
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
    stan.model = TRUE, fit = NA, ...)
stan.model(object, ...)
stan.parfit(cl, data, params, model, inits = NULL,
    seed = sample.int(.Machine$integer.max, n.chains),
    n.chains = 3,
    format = c("mcmc.list", "stanfit"),
    stan.model = TRUE, fit = NA, ...)
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
Desired output format.
}
  \item{stan.model}{
Logical, if \code{stanmodel} object should be returned.
}
  \item{fit}{
Fitted Stan object.
}
  \item{cl}{
A cluster object created by \code{\link[parallel]{makeCluster}}, or
an integer, see \code{\link{parDosa}} and
\code{\link{evalParallelArgument}}.
}
  \item{object}{
A fitted MCMC object ('mcmc.list' class for example),
with \code{"stan.model"} attribute.
}
  \item{\dots}{
Further arguments.
}
}
\value{
By default, an \code{stan.fit} returns an
\code{mcmc.list} object. If data cloning is used via the
\code{data} argument, \code{summary} returns a modified summary
containing scaled data cloning standard errors
(scaled by \code{sqrt(n.clones)}), and
\eqn{R_{hat}} values (as returned by \code{\link[coda]{gelman.diag}}).

\code{stan.model} returns the \code{stanmodel} object.

\code{stan.parfit} runs chains using multiple cores when \code{cl}
is an integer. Using a cluster object leads to recompiling the
model (therefore \code{fit} is ignored), and might not be
very quick to run.
}
\author{
Peter Solymos
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
if (require(rstan)) {
    model <- custommodel("data {
          int<lower=0> N;
          vector[N] y;
          vector[N] x;
        }
        parameters {
          real alpha;
          real beta;
          real<lower=0> sigma;
        }
        model {
          alpha ~ normal(0,10);
          beta ~ normal(0,10);
          sigma ~ cauchy(0,5);
          for (n in 1:N)
            y[n] ~ normal(alpha + beta * x[n], sigma);
        }")
    N <- 100
    alpha <- 1
    beta <- -1
    sigma <- 0.5
    x <- runif(N)
    y <- rnorm(N, alpha + beta * x, sigma)
    dat <- list(N=N, y=y, x=x)
    params <- c("alpha", "beta", "sigma")
    ## compile on 1st time only
    fit0 <- stan.fit(dat, params, model)
    ## reuse compiled fit0
    fit <- stan.fit(dat, params, model, fit=fit0)
    sm <- stan.model(fit)
    summary(fit)
    sm

    ## data cloning
    dcdat <- dclone(dat, n.clones=2, multiply="N")
    dcfit <- stan.fit(dcdat, params, model, fit=fit0)
    summary(dcfit)
    nclones(dcfit)

    ## using parallel options
    cl <- makeCluster(2)
    ## cannot utilize compiled fit0
    fit2 <- stan.parfit(cl=cl, dat, params, model)
    stopCluster(cl)
    if (.Platform$OS.type != "windows") {
        ## utilize compiled fit0
        fit3 <- stan.parfit(cl=2, dat, params, model, fit=fit0)
    }
}
}
}
\keyword{ models }
\keyword{ htest }
