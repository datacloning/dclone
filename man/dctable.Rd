\encoding{UTF-8}
\name{dctable}
\Rdversion{1.1}
\alias{dctable}
\alias{dctable.default}
\alias{extractdctable}
\alias{extractdctable.default}
\alias{plot.dctable}
\alias{dcdiag}
\alias{dcdiag.default}
\alias{extractdcdiag}
\alias{extractdcdiag.default}
\alias{plot.dcdiag}
\title{
Retrieve descriptive statistics from fitted objects to
evaluate convergence
}
\description{
The function is used to retrieve descriptive statistics
from fitted objects on order to evaluate
convergence of the data cloning algorithm. This is best done
via visual display of the results,
separately for each parameters of interest.
}
\usage{
dctable(x, ...)
\method{dctable}{default}(x, ...)
\method{plot}{dctable}(x, which = 1:length(x),
    type = c("all", "var", "log.var"),
    position = "topright", box.cex = 0.75, box.bg, ...)
extractdctable(x, ...)
\method{extractdctable}{default}(x, ...)

dcdiag(x, ...)
\method{dcdiag}{default}(x, ...)
\method{plot}{dcdiag}(x, which = c("all", "lambda.max",
    "ms.error", "r.squared", "log.lambda.max"),
    position = "topright", ...)
extractdcdiag(x, ...)
\method{extractdcdiag}{default}(x, ...)
}
\arguments{
  \item{x}{
  An MCMC or a 'dctable' object.
}
  \item{\dots}{
  Optionally more fitted model objects for function \code{dctable}.
}
  \item{which}{
  What to plot. For \code{dctable}, character names or integer indices
  of the estimated parameters are accepted.
  for \code{dcdiag} it should be one of
  \code{c("all", "lambda.max", "ms.error", "r.squared")}.
}
  \item{type}{
  Type of plot to be drawn. See Details.
}
  \item{position}{
  Position for the legend, as for \code{\link{legend}}.
}
  \item{box.cex}{
  Scaling factor for the interquartile boxes.
}
  \item{box.bg}{
  Background color for the interquartile boxes.
}
}
\details{
\code{dctable} returns the \code{"dctable"} attribute of the MCMC
object, or if it is \code{NULL}, calculates the \code{dctable}
summaries. If more than one fitted objects are provided, summaries are
calculated for all objects, and results are ordered by the number of
clones.

The \code{plot} method for \code{dctable} helps in graphical
representation of the descriptive statistics.
\code{type = "all"} results in plotting means,
standard deviations and quantiles
against the number of clones as boxplot. \code{type = "var"}
results in plotting the scaled variances
against the number of clones. In this case variances are
divided by the variance of the
model with smallest number of clones, \code{min(n.clones)}.
\code{type = "log.var"} is the same as \code{"var"},
but on the log scale. Along with the values, the
\code{min(n.clones) / n.clones} line is plotted for reference.

Lele et al. (2010) introduced diagnostic measures
for checking the convergence of the data cloning algorithm
which are based on the joint posterior distribution
and not only on single parameters. These
include to calculate the largest eigenvalue of the posterior
variance covariance matrix (\code{lambda.max} as returned by
\code{\link{lambdamax.diag}}),
or to calculate mean squared error (\code{ms.error}) and another
correlation-like fit statistic (\code{r.squared}) based on a
Chi-squared approximation
(as returned by \code{\link{chisq.diag}}). The maximum
eigenvalue reflects the degenerateness of the
posterior distribution, while the two fit measures reflect
if the Normal approximation is adequate. All
three statistics should converge to zero as the number of clones
increases. If this happens, different prior specifications are no
longer influencing the results (Lele et al., 2007, 2010).
These are conveniently collected by the \code{dcdiag} function.

\bold{IMPORTANT!}
Have you checked if different prior specifications
lead to the same results?
}
\value{
An object of class 'dctable'.
It is a list, and contains as many data frames as the number of
parameters in the fitted object. Each data frame contains
descriptives as the function of the number of clones.

\code{dcdiag} returns a data frame with convergence diagnostics.

The \code{plot} methods produce graphs as side effect.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized
linear mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{105}, 1617--1625.

Solymos, P., 2010. dclone: Data Cloning in R.
\emph{The R Journal} \strong{2(2)}, 29--37.
URL: \url{https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
}
\author{
Peter Solymos,
implementation is based on many discussions
with Khurram Nadeem and Subhash Lele.
}
\seealso{
Data cloning: \code{\link{dclone}}

Model fitting: \code{\link{jags.fit}}, \code{\link{bugs.fit}},
\code{\link{dc.fit}}
}
\examples{
\dontrun{
## simulation for Poisson GLMM
set.seed(1234)
n <- 20
beta <- c(2, -1)
sigma <- 0.1
alpha <- rnorm(n, 0, sigma)
x <- runif(n)
X <- model.matrix(~x)
linpred <- crossprod(t(X), beta) + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
    sigma ~ dlnorm(0, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## number of clones to be used, etc.
## iteartive fitting
jmod <- dc.fit(jdata, c("beta", "sigma"), jfun1,
    n.clones = 1:5, multiply = "n", unchanged = "np")
## summary with DC SE and R hat
summary(jmod)
dct <- dctable(jmod)
plot(dct)
## How to use estimates to make priors more informative?
glmm.model.up <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:p) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dgamma(priors[(p+1),2], priors[(p+1),1])
}
## function for updating, x is an MCMC object
upfun <- function(x) {
    if (missing(x)) {
        p <- ncol(X)
        return(cbind(c(rep(0, p), 0.001), rep(0.001, p+1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.01, length(par))))
    }
}
updat <- list(n = n, Y = Y, X = X, p = ncol(X), priors = upfun())
dcmod <- dc.fit(updat, c("beta", "sigma"), glmm.model.up,
    n.clones = 1:5, multiply = "n", unchanged = "p",
    update = "priors", updatefun = upfun)
summary(dcmod)
dct <- dctable(dcmod)
plot(dct)
plot(dct, type = "var")
}
}
\keyword{ models }
\keyword{ htest }
