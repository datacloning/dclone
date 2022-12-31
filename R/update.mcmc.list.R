#' Automatic updating of an MCMC object from JAGS
#' 
#' Automatic updating of an MCMC object until a desired statistic value
#' reached.
#' 
#' \code{updated.model} can be used to retrieve the updated model from an MCMC
#' object fitted via the function \code{\link{jags.fit}} and
#' \code{\link{dc.fit}} (with \code{flavour = "jags"}). The \code{update}
#' method is a wrapper for this purpose, specifically designed for the case
#' when MCMC convergence is problematic. A function is evaluated on the updated
#' model in each iteration of the updating process, and an MCMC object is
#' returned when iteration ends, or when the evaluated function returns
#' \code{TRUE} value.
#' 
#' \code{n.update} and \code{n.iter} can be vectors, if lengths are shorter
#' then \code{times}, values are recycled.
#' 
#' Data cloning information is preserved.
#' 
#' @aliases update.mcmc.list updated.model
#' @param object A fitted MCMC object ('mcmc.list' class for example), with
#' \code{"updated.model"} attribute.
#' @param fun A function that evaluates convergence of the MCMC chains, must
#' return logical result. See Examples. The iterative updating quits when
#' return value is \code{TRUE}. Can be missing, in which case there is no
#' stopping rule.
#' @param times Number of times the updating should be repeated. If \code{fun}
#' returns \code{TRUE}, updating is finished and MCMC object is returned.
#' @param n.update Number of updating iterations. The default 0 indicates, that
#' only \code{n.iter} iterations are used.
#' @param n.iter Number of iterations for sampling and evaluating \code{fun}.
#' If missing, value is taken from \code{object}.
#' @param thin Thinning value. If missing, value is taken from \code{object}.
#' @param \dots Other arguments passed to \code{\link[rjags]{coda.samples}}.
#' @return \code{updated.model} returns the state of the JAGS model after
#' updating and sampling. This can be further updated by the function
#' \code{\link[rjags]{update.jags}} and sampled by
#' \code{\link[rjags]{coda.samples}} if convergence diagnostics were not
#' satisfactory.
#' 
#' \code{update} returns an MCMC object with \code{"updated.model"} attribute.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link{jags.fit}}, \code{\link[rjags]{coda.samples}},
#' \code{\link[rjags]{update.jags}}
#' @keywords models htest
#' @examples
#' 
#' \dontrun{
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
#' ## get the updated model
#' upmod <- updated.model(regmod)
#' upmod
#' ## automatic updating
#' ## using R_hat < 1.1 as criteria
#' critfun <- function(x)
#'     all(gelman.diag(x)$psrf[,1] < 1.1)
#' mod <- update(regmod, critfun, 5)
#' ## update just once
#' mod2 <- update(regmod)
#' summary(mod)
#' }
#' 
#' @export update.mcmc.list
update.mcmc.list <-
function(object, fun, times = 1, n.update = 0, 
n.iter, thin, ...)
{
    ## stop if rjags not found
    requireNamespace("rjags")
    ## eval of args
    if (is.null(updated.model(object)))
        stop("updated model not found")
    mod <- object
    ## update can go even if fun is missing
    if (missing(fun))
        fun <- function(z) FALSE
    fun <- match.fun(fun)
    fval <- fun(mod)
    if (!is.logical(fval))
        stop("'fun' must return logical")
    if (length(fval) > 1)
        stop("'fun' must return a single value")
    if (fval)
        return(object)
    ## what to sample
    params <- varnames(mod)
    ## missing args
    if (missing(thin))
        thin <- coda::thin(object)
    if (missing(n.iter))
        n.iter <- niter(object) * coda::thin(object)
    ## n.update/n.iter vs. times
    if (length(n.update) < times)
        n.update <- rep(n.update, times)[1:times]
    if (length(n.iter) < times)
        n.iter <- rep(n.iter, times)[1:times]
    ## auto-updating
    for (i in 1:times) {
        if (n.update[i] > 0)
            update(updated.model(object), n.update[i], ...)
        mod <- coda.samples(updated.model(object), params, n.iter[i], thin, ...)
        if (fun(mod))
            break
    }
    ## put things together
    attr(mod, "updated.model") <- updated.model(object)
    n.clones <- nclones(object)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(mod, "n.clones") <- n.clones
        class(mod) <- c("mcmc.list.dc", class(mod))
    }
    mod
}
