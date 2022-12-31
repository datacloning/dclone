#' Generate posterior samples in mcmc.list format
#' 
#' This function sets a trace monitor for all requested nodes, updates the
#' model and coerces the output to a single \code{mcmc.list} object.  This
#' function uses \code{\link[rjags]{coda.samples}} but keeps track of data
#' cloning information supplied via the \code{model} argument.
#' 
#' 
#' @param model a jags model object
#' @param variable.names a character vector giving the names of variables to be
#' monitored
#' @param n.iter number of iterations to monitor
#' @param thin thinning interval for monitors
#' @param na.rm logical flag that indicates whether variables containing
#' missing values should be omitted. See details in help page of
#' \code{\link[rjags]{coda.samples}}.
#' @param ... optional arguments that are passed to the update method for jags
#' model objects
#' @return An \code{mcmc.list} object. An \code{n.clones} attribute is attached
#' to the object, but unlike in \code{\link{jags.fit}} there is no
#' \code{\link{updated.model}} attribute as it is equivalent to the input jags
#' model object.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link[rjags]{coda.samples}},
#' \code{\link[rjags]{update.jags}}, \code{\link[rjags]{jags.model}}
#' 
#' Parallel version: \code{\link{parCodaSamples}}
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' model <- function() {
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
#' jdata <- dclone(list(N = N, Y = Y, x = x), 2, multiply="N")
#' jpara <- c("alpha", "beta", "sigma")
#' ## jags model
#' res <- jagsModel(file=model, data=jdata, n.chains = 3, n.adapt=1000)
#' nclones(res)
#' update(res, n.iter=1000)
#' nclones(res)
#' m <- codaSamples(res, jpara, n.iter=2000)
#' summary(m)
#' nclones(m)
#' }
#' 
#' @export codaSamples
codaSamples <-
function(model, variable.names = NULL, n.iter,
thin = 1, na.rm=TRUE, ...)
{
    requireNamespace("rjags")
    n.clones <- nclones(model)
    res <- coda.samples(model=model,
        variable.names=variable.names,
        n.iter=n.iter,
        thin=thin, na.rm=na.rm, ...)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
