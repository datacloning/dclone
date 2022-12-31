#' Generate posterior samples in 'mcmc.list' format on parallel workers
#' 
#' This function sets a trace monitor for all requested nodes, updates the
#' model on each workers. Finally, it return the chains to the master and
#' coerces the output to a single \code{mcmc.list} object.
#' 
#' 
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer. It can also be \code{NULL}, see \code{\link{parDosa}}.
#' @param model character, name of a jags model object
#' @param variable.names a character vector giving the names of variables to be
#' monitored
#' @param n.iter number of iterations to monitor
#' @param thin thinning interval for monitors
#' @param na.rm logical flag that indicates whether variables containing
#' missing values should be omitted. See details in help page of
#' \code{\link[rjags]{coda.samples}}.
#' @param ... optional arguments that are passed to the update method for jags
#' model objects
#' @return An \code{mcmc.list} object with possibly an \code{n.clones}
#' attribute.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Original sequential function in \pkg{rjags}:
#' \code{\link[rjags]{coda.samples}}
#' 
#' Sequential \pkg{dclone}-ified version: \code{\link{codaSamples}}
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' if (require(rjags)) {
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
#' jdata <- list(N = N, Y = Y, x = x)
#' jpara <- c("alpha", "beta", "sigma")
#' ## jags model on parallel workers
#' ## n.chains must be <= no. of workers
#' cl <- makePSOCKcluster(4)
#' parJagsModel(cl, name="res", file=model, data=jdata,
#'     n.chains = 2, n.adapt=1000)
#' parUpdate(cl, "res", n.iter=1000)
#' m <- parCodaSamples(cl, "res", jpara, n.iter=2000)
#' stopifnot(2==nchain(m))
#' ## with data cloning
#' dcdata <- dclone(list(N = N, Y = Y, x = x), 2, multiply="N")
#' parJagsModel(cl, name="res2", file=model, data=dcdata,
#'     n.chains = 2, n.adapt=1000)
#' parUpdate(cl, "res2", n.iter=1000)
#' m2 <- parCodaSamples(cl, "res2", jpara, n.iter=2000)
#' stopifnot(2==nchain(m2))
#' nclones(m2)
#' stopCluster(cl)
#' }
#' }
#' 
#' @export parCodaSamples
parCodaSamples <-
function(cl, model, variable.names = NULL, n.iter,
thin = 1, na.rm=TRUE, ...)
{
    ## stop if rjags not found
    requireNamespace("rjags")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    if (!is.character(model))
        model <- as.character(model) # deparse(substitute(model))
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=model, na.rm=na.rm)
    jagsparallel <- function(i, ...) {
        cldata <- pullDcloneEnv("cldata", type = "model")
        if (!existsDcloneEnv(cldata$name, type = "results"))
            return(NULL)
        res <- pullDcloneEnv(cldata$name, type = "results")
        n.clones <- nclones(res)
        out <- rjags::coda.samples(res, variable.names=cldata$variable.names,
            n.iter=cldata$n.iter, thin=cldata$thin, na.rm=cldata$na.rm, ...)
        ## jags model is pushed back to .env, mcmc.list is returned
        pushDcloneEnv(cldata$name, res, type = "results")
        if (!is.null(n.clones) && n.clones > 1) {
            attr(out, "n.clones") <- n.clones
        }
        out
    }
#    dir <- if (inherits(cl, "SOCKcluster")) # model now has full path
#        getwd() else NULL
    res <- parDosa(cl, 1:length(cl), jagsparallel, cldata,
        lib = c("dclone", "rjags"), balancing = "none", size = 1,
        rng.type = getOption("dcoptions")$RNG,
        cleanup = TRUE,
        dir = NULL, # model now has full path
        unload=FALSE, ...)
    res <- res[!sapply(res, is.null)]
    n.clones <- lapply(res, nclones)
    if (length(unique(unlist(n.clones))) != 1L) {
        n.clones <- NULL
        warnings("inconsistent 'n.clones' values, set to NULL")
    } else n.clones <- n.clones[[1]]
    for (i in 1:length(res)) {
        attr(res, "n.clones") <- NULL
    }
    res <- as.mcmc.list(lapply(res, as.mcmc))
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
