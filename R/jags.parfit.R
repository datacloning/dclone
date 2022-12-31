#' Parallel computing with JAGS
#' 
#' Does the same job as \code{\link[dclone]{jags.fit}}, but parallel chains are
#' run on parallel workers, thus computations can be faster (up to
#' 1/\code{n.chains}) for long MCMC runs.
#' 
#' Chains are run on parallel workers, and the results are combined in the end.
#' 
#' No update method is available for parallel \code{mcmc.list} objects. See
#' \code{\link{parUpdate}} and related parallel functions
#' (\code{\link{parJagsModel}}, \code{\link{parCodaSamples}}) for such purpose.
#' 
#' Additionally loaded JAGS modules (e.g. \code{"glm"}, \code{"lecuyer"}) need
#' to be loaded to the workers when using 'snow' type cluster as \code{cl}
#' argument. See Examples.
#' 
#' The use of the \code{"lecuyer"} module is recommended when running more than
#' 4 chains. See Examples and \code{\link{parallel.inits}}.
#' 
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer, see \code{\link{parDosa}} and
#' \code{\link{evalParallelArgument}}.
#' @param data A named list or environment containing the data. If an
#' environment, \code{data} is coerced into a list.
#' @param params Character vector of parameters to be sampled.
#' @param model Character string (name of the model file), a function
#' containing the model, or a or \code{\link{custommodel}} object (see
#' Examples).
#' @param inits Specification of initial values in the form of a list or a
#' function, can be missing. Missing value setting can include RNG seed
#' information, see Initialization at \code{\link[rjags]{jags.model}}. If this
#' is a function and using 'snow' type cluster as \code{cl}, the function must
#' be self containing, i.e. not having references to R objects outside of the
#' function, or the objects should be exported with \code{\link{clusterExport}}
#' before calling \code{jags.parfit}. Forking type parallelism does not require
#' such attention.
#' @param n.chains Number of chains to generate, must be higher than 1.
#' Ideally, this is equal to the number of parallel workers in the cluster.
#' @param ...  Other arguments passed to \code{\link[dclone]{jags.fit}}.
#' @return An \code{mcmc.list} object.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Sequential version: \code{\link[dclone]{jags.fit}}
#' 
#' Function for stepwise modeling with JAGS: \code{\link{parJagsModel}},
#' \code{\link{parUpdate}}, \code{\link{parCodaSamples}}
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' if (require(rjags)) {
#' set.seed(1234)
#' n <- 20
#' x <- runif(n, -1, 1)
#' X <- model.matrix(~x)
#' beta <- c(2, -1)
#' mu <- crossprod(t(X), beta)
#' Y <- rpois(n, exp(mu))
#' glm.model <- function() {
#'     for (i in 1:n) {
#'         Y[i] ~ dpois(lambda[i])
#'         log(lambda[i]) <- inprod(X[i,], beta[1,])
#'     }
#'     for (j in 1:np) {
#'         beta[1,j] ~ dnorm(0, 0.001)
#'     }
#' }
#' dat <- list(Y=Y, X=X, n=n, np=ncol(X))
#' load.module("glm")
#' m <- jags.fit(dat, "beta", glm.model)
#' cl <- makePSOCKcluster(3)
#' ## load glm module
#' tmp <- clusterEvalQ(cl, library(dclone))
#' parLoadModule(cl, "glm")
#' pm <- jags.parfit(cl, dat, "beta", glm.model)
#' ## chains are not identical -- this is good
#' pm[1:2,]
#' summary(pm)
#' ## examples on how to use initial values
#' ## fixed initial values
#' inits <- list(list(beta=matrix(c(0,1),1,2)),
#'     list(beta=matrix(c(1,0),1,2)),
#'     list(beta=matrix(c(0,0),1,2)))
#' pm2 <- jags.parfit(cl, dat, "beta", glm.model, inits)
#' ## random numbers generated prior to jags.parfit
#' inits <- list(list(beta=matrix(rnorm(2),1,2)),
#'     list(beta=matrix(rnorm(2),1,2)),
#'     list(beta=matrix(rnorm(2),1,2)))
#' pm3 <- jags.parfit(cl, dat, "beta", glm.model, inits)
#' ## self contained function
#' inits <- function() list(beta=matrix(rnorm(2),1,2))
#' pm4 <- jags.parfit(cl, dat, "beta", glm.model, inits)
#' ## function pointing to the global environment
#' fun <- function() list(beta=matrix(rnorm(2),1,2))
#' inits <- function() fun()
#' clusterExport(cl, "fun")
#' ## using the L'Ecuyer module with 6 chains
#' load.module("lecuyer")
#' parLoadModule(cl,"lecuyer")
#' pm5 <- jags.parfit(cl, dat, "beta", glm.model, inits,
#'     n.chains=6)
#' nchain(pm5)
#' unload.module("lecuyer")
#' parUnloadModule(cl,"lecuyer")
#' stopCluster(cl)
#' ## multicore type forking
#' if (.Platform$OS.type != "windows") {
#' pm6 <- jags.parfit(3, dat, "beta", glm.model)
#' }
#' }
#' }
#' 
#' @export jags.parfit
jags.parfit <-
function(cl, data, params, model, inits = NULL, n.chains = 3, ...)
{
    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=FALSE)
    ## sequential evaluation falls back on jags.fit
    if (is.null(cl)) {
        return(jags.fit(data, params, model,
            inits = inits, n.chains = n.chains, ...))
    }
    ## parallel evaluation starts here
    ## stop if rjags not found
    requireNamespace("rjags")
    if (inherits(cl, "cluster"))
        clusterEvalQ(cl, requireNamespace("rjags"))
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    trace <- getOption("dcoptions")$verbose
    ## eval args
    if (!is.null(list(...)$updated.model))
        stop("'updated.model' argument is not available for parallel computations")
    if (!is.null(list(...)$n.iter))
        if (list(...)$n.iter == 0)
            stop("'n.iter = 0' is not supported for parallel computations")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    ## write model
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        ## write model only if SOCK cluster or multicore (shared memory)
        if (is.numeric(cl) || inherits(cl, "SOCKcluster")) {
            model <- write.jags.model(model)
            on.exit(try(clean.jags.model(model)))
        }
    }
    ## generating initial values and RNGs if needed
    if (inherits(cl, "cluster") && "lecuyer" %in% list.modules()) {
        mod <- parListModules(cl)
        for (i in 1:length(mod)) {
            if (!("lecuyer" %in% mod[[i]]))
                stop("'lecuyer' module must be loaded on workers")
        }
    }
    inits <- parallel.inits(inits, n.chains)
#    inits <- jags.fit(data, params, model, inits, n.chains,
#        n.adapt=0, n.update=0, n.iter=0)$state(internal=TRUE)
    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel function to evaluate by parDosa
    jagsparallel <- function(i, ...)   {
        cldata <- pullDcloneEnv("cldata", type = "model")
        jags.fit(data=cldata$data, params=cldata$params,
            model=cldata$model,
            inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dcoptions")$LB)
        "load" else "none"
#    dir <- if (inherits(cl, "SOCKcluster")) # model now has full path
#        getwd() else NULL
    mcmc <- parDosa(cl, 1:n.chains, jagsparallel, cldata,
        lib=c("dclone","rjags"), balancing=balancing, size=1,
        rng.type=getOption("dcoptions")$RNG, cleanup=TRUE,
        dir=NULL, # model now has full path
        unload=FALSE, ...)
    ## binding the chains
    res <- as.mcmc.list(lapply(mcmc, as.mcmc))
    ## attaching attribs and return
    n.clones <- nclones(data)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
