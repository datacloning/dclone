

#' Methods for the 'mcmc.list' class
#' 
#' Methods for 'mcmc.list' objects.
#' 
#' 
#' @aliases dcsd dcsd.mcmc.list coef.mcmc.list confint.mcmc.list.dc
#' vcov.mcmc.list.dc vcov.mcmc.list quantile.mcmc.list
#' @param x,object MCMC object to be processed.
#' @param parm A specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names.  If missing, all
#' parameters are considered.
#' @param level The confidence level required.
#' @param \dots Further arguments passed to functions.
#' @param invfisher Logical, if the inverse of the Fisher information matrix
#' (\code{TRUE}) should be returned instead of the variance-covariance matrix
#' of the joint posterior distribution (\code{FALSE}).
#' @return \code{dcsd} returns the data cloning standard errors of a posterior
#' MCMC chain calculated as standard deviation times the square root of the
#' number of clones.
#' 
#' The \code{coef} method returns mean of the posterior MCMC chains for the
#' monitored parameters.
#' 
#' The \code{confint} method returns Wald-type confidence intervals for the
#' parameters assuming asymptotic normality.
#' 
#' The \code{vcov} method returns the inverse of the Fisher information matrix
#' (\code{invfisher = TRUE}) or the covariance matrix of the joint posterior
#' distribution (\code{invfisher = FALSE}).  The \code{invfisher} is valid only
#' for \code{mcmc.list.dc} (data cloned) objects.
#' 
#' The \code{quantile} method returns quantiles for each variable.
#' @note Some functions only available for the 'mcmc.list.dc' class which
#' inherits from class 'mcmc.list'.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link{jags.fit}}, \code{\link{bugs.fit}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## simple regression example from the JAGS manual
#' jfun <- function() {
#'     for (i in 1:N) {
#'         Y[i] ~ dnorm(mu[i], tau)
#'         mu[i] <- alpha + beta * (x[i] - x.bar)
#'     }
#'     x.bar <- mean(x)
#'     alpha ~ dnorm(0.0, 1.0E-4)
#'     beta ~ dnorm(0.0, 1.0E-4)
#'     sigma <- 1.0/sqrt(tau)
#'     tau ~ dgamma(1.0E-3, 1.0E-3)
#' }
#' ## data generation
#' set.seed(1234)
#' N <- 100
#' alpha <- 1
#' beta <- -1
#' sigma <- 0.5
#' x <- runif(N)
#' linpred <- crossprod(t(model.matrix(~x)), c(alpha, beta))
#' Y <- rnorm(N, mean = linpred, sd = sigma)
#' ## data for the model
#' dcdata <- dclone(list(N = N, Y = Y, x = x), 5, multiply = "N")
#' ## data cloning
#' dcmod <- jags.fit(dcdata, c("alpha", "beta", "sigma"), jfun, 
#'     n.chains = 3)
#' summary(dcmod)
#' coef(dcmod)
#' dcsd(dcmod)
#' confint(dcmod)
#' vcov(dcmod)
#' vcov(dcmod, invfisher = FALSE)
#' quantile(dcmod)
#' }
#' 
NULL





#' Data Cloning
#' 
#' Low level functions for implementing maximum likelihood estimating
#' procedures for complex models using data cloning and Bayesian Markov chain
#' Monte Carlo methods.  Sequential and parallel MCMC support for JAGS,
#' WinBUGS, OpenBUGS, and Stan.
#' 
#' Main functions include:
#' 
#' \itemize{ \item \code{\link{dclone}}, \code{\link{dcdim}},
#' \code{\link{dciid}}, \code{\link{dctr}}: cloning R objects in various ways.
#' 
#' \item \code{\link{jags.fit}}, \code{\link{bugs.fit}},
#' \code{\link{stan.fit}}: conveniently fit JAGS/BUGS/Stan models.
#' \code{\link{jags.parfit}}, \code{\link{bugs.parfit}},
#' \code{\link{stan.parfit}} fits chains on parallel workers.
#' 
#' \item \code{\link{dc.fit}}: iterative model fitting by the data cloning
#' algorithm.  \code{\link{dc.parfit}} is the parallelized version.
#' 
#' \item \code{\link{dctable}}, \code{\link{dcdiag}}: helps evaluating data
#' cloning convergence by descriptive statistics and diagnostic tools.  (These
#' are based on e.g. \code{\link{chisq.diag}} and
#' \code{\link{lambdamax.diag}}.)
#' 
#' \item \code{\link{coef.mcmc.list}}, \code{\link{confint.mcmc.list.dc}},
#' \code{\link{dcsd.mcmc.list}}, \code{\link{quantile.mcmc.list}},
#' \code{\link{vcov.mcmc.list.dc}}, \code{\link{mcmcapply}},
#' \code{\link{stack.mcmc.list}}: methods for \code{mcmc.list} objects.
#' 
#' \item \code{\link{write.jags.model}}, \code{\link{clean.jags.model}},
#' \code{\link{custommodel}}: convenient functions for handling
#' \code{JAGS/BUGS/Stan} models.
#' 
#' \item \code{\link{jagsModel}}, \code{\link{codaSamples}}: basic functions
#' from \pkg{rjags} package rewrote to recognize data cloning attributes from
#' data (\code{\link{parJagsModel}}, \code{\link{parUpdate}},
#' \code{\link{parCodaSamples}} are the parallel versions).
#' 
#' }
#' 
#' 
#' @name dclone-package
#' @docType package
#' @author Author: Peter Solymos
#' 
#' Maintainer: Peter Solymos, \email{solymos@@ualberta.ca}
#' @references
#' 
#' Forum: \url{https://groups.google.com/forum/#!forum/dclone-users}
#' 
#' Issues: \url{https://github.com/datacloning/dcmle/issues}
#' 
#' Data cloning website: \url{http://datacloning.org}
#' 
#' Solymos, P., 2010. dclone: Data Cloning in R. \emph{The R Journal}
#' \strong{2(2)}, 29--37. URL:
#' \url{https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
#' 
#' Lele, S.R., B. Dennis and F. Lutscher, 2007. Data cloning: easy maximum
#' likelihood estimation for complex ecological models using Bayesian Markov
#' chain Monte Carlo methods. \emph{Ecology Letters} \strong{10}, 551--563.
#' 
#' Lele, S. R., K. Nadeem and B. Schmuland, 2010. Estimability and likelihood
#' inference for generalized linear mixed models using data cloning.
#' \emph{Journal of the American Statistical Association} \strong{105},
#' 1617--1625.
#' @keywords package
NULL





#' Manipulating dclone environments
#' 
#' Manipulating dclone environments.
#' 
#' \code{type = "model"} manipulates the \code{.DcloneEnvModel} environment,
#' which is meant to store temporary objects for model fitting with `snow' type
#' parallelism (see \code{\link{parDosa}} for the implementation). This is
#' swiped clean after use.
#' 
#' The\code{type = "results"} manipulates the \code{.DcloneEnvResults}
#' environment, which is meant to store result objects on the workers. This is
#' \emph{not} swiped clean after use.
#' 
#' \code{pullDcloneEnv} pulls an object from these environments, similar to
#' \code{\link{get}} in effect.
#' 
#' \code{pushDcloneEnv} pushes an object to these environments, similar to
#' \code{\link{assign}} in effect.
#' 
#' \code{clearDcloneEnv} removes object(s) from these environments, similar to
#' \code{\link{rm}} in effect.
#' 
#' \code{listDcloneEnv} lists name(s) of object(s) in these environments,
#' similar to \code{\link{ls}} in effect.
#' 
#' \code{existsDcloneEnv} tests if an object exists in these environments,
#' similar to \code{\link{exists}} in effect.
#' 
#' @aliases DcloneEnv .DcloneEnvModel .DcloneEnvResults pullDcloneEnv
#' pushDcloneEnv clearDcloneEnv listDcloneEnv existsDcloneEnv
#' @param x a variable name, given as a character string.  No coercion is done,
#' and the first element of a character vector of length greater than one will
#' be used, with a warning.
#' @param value a value to be assigned to \code{x}.
#' @param type character, the type of environment to be accessed, see Details.
#' @param \dots the objects to be removed, as names (unquoted) or character
#' strings (quoted).
#' @param list a character vector naming objects to be removed.
#' @param mode the mode or type of object sought: see the \code{\link{exists}}.
#' @param inherits logical, should the enclosing frames of the environment be
#' searched?
#' @return For \code{pullDcloneEnv}, the object found.  If no object is found
#' an error results.
#' 
#' \code{pushDcloneEnv} is invoked for its side effect, which is assigning
#' \code{value} to the variable \code{x}.
#' 
#' For \code{clearDcloneEnv} its is the side effect of an object removed. No
#' value returned.
#' 
#' \code{listDcloneEnv} returns a character vector.
#' 
#' \code{existsDcloneEnv} returns logical, \code{TRUE} if and only if an object
#' of the correct name and mode is found.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link{parDosa}}
#' @keywords utilities manip environment
NULL





#' Abundances of ovenbird in Alberta
#' 
#' The data set contains observations (point counts) of 198 sites of the
#' Alberta Biodiversity Monitoring Institute.
#' 
#' \code{count}: integer, ovenbird counts per site.
#' 
#' \code{site, year}: numeric, site number and year of data collection.
#' 
#' \code{ecosite}: factor with 5 levels, ecological categorization of the
#' sites.
#' 
#' \code{uplow}: factor with 2 levels, ecological categorization of the sites
#' (same es ecosite but levels are grouped into \code{upland} and
#' \code{lowland}).
#' 
#' \code{dsucc, dalien, thd}: numeric, percentage of successional, alienating
#' and total human disturbance based on interpreted 3 x 7 km photoplots
#' centered on each site.
#' 
#' \code{long, lat}: numeric, public longitude/latitude coordinates of the
#' sites.
#' 
#' 
#' @name ovenbird
#' @docType data
#' @source Alberta Biodiversity Monitoring Institute, http://www.abmi.ca
#' @keywords datasets
#' @examples
#' 
#' data(ovenbird)
#' summary(ovenbird)
#' str(ovenbird)
#' 
NULL





#' Exemplary MCMC list object
#' 
#' This data set was made via the \code{\link{jags.fit}} function.
#' 
#' 
#' @name regmod
#' @docType data
#' @source See Example.
#' @keywords datasets
#' @examples
#' 
#' data(regmod)
#' summary(regmod)
#' plot(regmod)
#' \dontrun{
#' ## DATA GENERATION
#' ## simple regression example from the JAGS manual
#' jfun <- function() {
#'     for (i in 1:N) {
#'         Y[i] ~ dnorm(mu[i], tau)
#'         mu[i] <- alpha + beta * (x[i] - x.bar)
#'     }
#'     x.bar <- mean(x[])
#'     alpha ~ dnorm(0.0, 1.0E-4)
#'     beta ~ dnorm(0.0, 1.0E-4)
#'     sigma <- 1.0/sqrt(tau)
#'     tau ~ dgamma(1.0E-3, 1.0E-3)
#' }
#' ## data generation
#' set.seed(1234)
#' N <- 100
#' alpha <- 1
#' beta <- -1
#' sigma <- 0.5
#' x <- runif(N)
#' linpred <- crossprod(t(model.matrix(~x)), c(alpha, beta))
#' Y <- rnorm(N, mean = linpred, sd = sigma)
#' ## list of data for the model
#' jdata <- list(N = N, Y = Y, x = x)
#' ## what to monitor
#' jpara <- c("alpha", "beta", "sigma")
#' ## fit the model with JAGS
#' regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3,
#'     updated.model = FALSE)
#' }
#' 
NULL



