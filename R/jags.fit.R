## not using jagsModel and codaSamples so updated.model is without attribute


#' Fit JAGS models with cloned data
#' 
#' Convenient functions designed to work well with cloned data arguments and
#' JAGS.
#' 
#' 
#' @param data A named list or environment containing the data.  If an
#' environment, \code{data} is coerced into a list.
#' @param params Character vector of parameters to be sampled.
#' @param model Character string (name of the model file), a function
#' containing the model, or a or \code{\link{custommodel}} object (see
#' Examples).
#' @param inits Optional specification of initial values in the form of a list
#' or a function (see Initialization at \code{\link[rjags]{jags.model}}).  If
#' \code{NULL}, initial values will be generated automatically.  It is an error
#' to supply an initial value for an observed node.
#' @param n.chains Number of chains to generate.
#' @param n.adapt Number of steps for adaptation.
#' @param n.update Number of updates before iterations.  It is usually a bad
#' idea to use \code{n.update=0} if \code{n.adapt>0}, so a warning is issued in
#' such cases.
#' @param thin Thinning value.
#' @param n.iter Number of iterations.
#' @param updated.model Logical, if the updated model should be attached as
#' attribute (this can be used to further update if convergence was not
#' satisfactory, see \code{\link{updated.model}} and
#' \code{\link{update.mcmc.list}}).
#' @param \dots Further arguments passed to \code{\link[rjags]{coda.samples}},
#' and \code{\link[rjags]{update.jags}} (e.g. the \code{progress.bar}
#' argument).
#' @return An \code{mcmc.list} object. If data cloning is used via the
#' \code{data} argument, \code{summary} returns a modified summary containing
#' scaled data cloning standard errors (scaled by \code{sqrt(n.clones)}, see
#' \code{\link{dcsd}}), and \eqn{R_{hat}} values (as returned by
#' \code{\link[coda]{gelman.diag}}).
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Underlying functions: \code{\link[rjags]{jags.model}},
#' \code{\link[rjags]{update.jags}}, \code{\link[rjags]{coda.samples}}
#' 
#' Parallel chain computations: \code{\link{jags.parfit}}
#' 
#' Methods: \code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}},
#' \code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}},
#' \code{\link{vcov.mcmc.list.dc}}
#' @keywords models htest
#' @examples
#' 
#' \dontrun{
#' if (require(rjags)) {
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
#' regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
#' ## model summary
#' summary(regmod)
#' ## data cloning
#' dcdata <- dclone(jdata, 5, multiply = "N")
#' dcmod <- jags.fit(dcdata, jpara, jfun, n.chains = 3)
#' summary(dcmod)
#' }
#' }
#' 
#' @export jags.fit
jags.fit <-
function(data, params, model, inits = NULL, n.chains = 3, n.adapt = 1000, 
n.update = 1000, thin = 1, n.iter = 5000, updated.model = TRUE, ...)
{
    ## stop if rjags not found
    requireNamespace("rjags")
    if (n.adapt>0 && n.update==0)
        warnings("consider updating for independence after adaptation")
    ## inital steps
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    n.clones <- dclone::nclones.list(data)
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
    }
    ## handling inits arg, model initialization
    m <- if (is.null(inits)) {
        rjags::jags.model(model, data, n.chains=n.chains, n.adapt=n.adapt,
            quiet=!as.logical(getOption("dcoptions")$verbose))
    } else {
        rjags::jags.model(model, data, inits, n.chains=n.chains, n.adapt=n.adapt,
            quiet=!as.logical(getOption("dcoptions")$verbose))
    }
    if (is.null(list(...)$progress.bar)) {
        trace <- if (getOption("dcoptions")$verbose)
            getOption("jags.pb") else "none"
    } else trace <- list(...)$progress.bar
    byval <- if (!is.null(list(...)$by))
        list(...)$by else floor(min(n.iter/50, 100))
    ## model updating
    if (n.update > 0) {
        update(m, n.update, progress.bar=trace, by=byval)
    }
    ## coda samples
    if (n.iter > 0) {
        res <- rjags::coda.samples(m, params, n.iter=n.iter, thin=thin, 
            progress.bar=trace, by=byval)
    } else {
        if (!is.null(n.clones) && n.clones > 1) {
            attr(m, "n.clones") <- n.clones
        }
        return(m)
    }
    ## jags.model attribute
    if (updated.model)
        attr(res, "updated.model") <- m
    ## n.clones attr
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
