#' Parallel computing with WinBUGS/OpenBUGS
#' 
#' Does the same job as \code{\link[dclone]{bugs.fit}}, but parallel chains are
#' run on parallel workers, thus computations can be faster (up to
#' 1/\code{n.chains}) for long MCMC runs.
#' 
#' Chains are run on parallel workers, and the results are combined in the end.
#' 
#' The seed must be supplied, as it is the user's responsibility to make sure
#' that pseudo random sequences do not seriously overlap.
#' 
#' The WinBUGS implementation is quite unsafe from this regard, because the
#' pseudo-random number generator used by WinBUGS generates a finite (albeit
#' very long) sequence of distinct numbers, which would eventually be repeated
#' if the sampler were run for a sufficiently long time. Thus it's usage must
#' be discouraged. That is the reason for the warning that is issued when
#' \code{program = "winbugs"}.
#' 
#' OpenBUGS (starting from version 3.2.2) implemented a system where internal
#' state of the pseudo random number generator can be set to one of 14
#' predefined states (seed values in \code{1:14}). Each predefined state is
#' 10^12 draws apart to avoid overlap in pseudo random number sequences.
#' 
#' Note: the default setting \code{working.directory = NULL} cannot be changed
#' when running parallel chains with \code{bugs.parfit} because the multiple
#' instances would try to read/write the same directory.
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
#' function, can be missing. If this is a function and using 'snow' type
#' cluster as \code{cl}, the function must be self containing, i.e. not having
#' references to R objects outside of the function, or the objects should be
#' exported with \code{\link{clusterExport}} before calling \code{bugs.parfit}.
#' Forking type parallelism does not require such attention.
#' @param n.chains Number of chains to generate, must be higher than 1.
#' Ideally, this is equal to the number of parallel workers in the cluster.
#' @param seed Vector of random seeds, must have \code{n.chains} unique values.
#' See Details.
#' @param program The program to use, not case sensitive. See
#' \code{\link{bugs.fit}}.
#' @param ...  Other arguments passed to \code{\link[dclone]{bugs.fit}}.
#' @return An \code{mcmc.list} object.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Sequential version: \code{\link[dclone]{bugs.fit}}
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' ## fitting with WinBUGS, bugs example
#' if (require(R2WinBUGS)) {
#' data(schools)
#' dat <- list(J = nrow(schools),
#'     y = schools$estimate,
#'     sigma.y = schools$sd)
#' bugs.model <- function(){
#'        for (j in 1:J){
#'          y[j] ~ dnorm (theta[j], tau.y[j])
#'          theta[j] ~ dnorm (mu.theta, tau.theta)
#'          tau.y[j] <- pow(sigma.y[j], -2)
#'        }
#'        mu.theta ~ dnorm (0.0, 1.0E-6)
#'        tau.theta <- pow(sigma.theta, -2)
#'        sigma.theta ~ dunif (0, 1000)
#'      }
#' param <- c("mu.theta", "sigma.theta")
#' SEED <- floor(runif(3, 100000, 999999))
#' cl <- makePSOCKcluster(3)
#' if (.Platform$OS.type == "windows") {
#' sim <- bugs.parfit(cl, dat, param, bugs.model, seed=SEED)
#' summary(sim)
#' }
#' dat2 <- dclone(dat, 2, multiply="J")
#' if (.Platform$OS.type == "windows") {
#' sim2 <- bugs.parfit(cl, dat2, param, bugs.model,
#'     program="winbugs", n.iter=2000, n.thin=1, seed=SEED)
#' summary(sim2)
#' }
#' }
#' if (require(BRugs)) {
#' ## fitting the model with OpenBUGS
#' ## using the less preferred BRugs interface
#' sim3 <- bugs.parfit(cl, dat2, param, bugs.model,
#'     program="brugs", n.iter=2000, n.thin=1, seed=1:3)
#' summary(sim3)
#' }
#' if (require(R2OpenBUGS)) {
#' ## fitting the model with OpenBUGS
#' ## using the preferred R2OpenBUGS interface
#' sim4 <- bugs.parfit(cl, dat2, param, bugs.model,
#'     program="openbugs", n.iter=2000, n.thin=1, seed=1:3)
#' summary(sim4)
#' }
#' stopCluster(cl)
#' ## multicore type forking
#' if (require(R2OpenBUGS) && .Platform$OS.type != "windows") {
#' sim7 <- bugs.parfit(3, dat2, param, bugs.model,
#'     program="openbugs", n.iter=2000, n.thin=1, seed=1:3)
#' summary(sim7)
#' }
#' }
#' 
#' @export bugs.parfit
bugs.parfit <-
function(cl, data, params, model, inits=NULL,
n.chains = 3, seed,
program=c("winbugs", "openbugs", "brugs"), ...) ## only mcmc.list format is supported when cl is not NULL
{
    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=FALSE)
    ## sequential evaluation falls back on jags.fit
    if (is.null(cl)) {
        return(bugs.fit(data, params, model,
            inits = inits, n.chains = n.chains, seed=seed, ...))
    }
    ## parallel evaluation starts here
    ## not case sensitive evaluation of program arg
    program <- match.arg(tolower(program), c("winbugs", "openbugs", "brugs"))

    if (program %in% c("winbugs", "brugs") && !suppressWarnings(requireNamespace("R2WinBUGS")))
        stop("there is no package called 'R2WinBUGS'")
    if (program == "brugs" && !suppressWarnings(requireNamespace("BRugs")))
        stop("there is no package called 'BRugs'")
    if (program == "openbugs" && !suppressWarnings(requireNamespace("R2OpenBUGS")))
        stop("there is no package called 'R2OpenBUGS'")

    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    if (!is.null(list(...)$format))
        if (list(...)$format == "bugs")
            stop("only 'mcmc.list' format is supported for parallel parallel computations")

    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    if (missing(seed))
        stop("'seed' must be provided")
    if (is.null(seed))
        stop("'seed' must be not be NULL")
    ## if all seed are the same
    if (length(unique(seed)) != n.chains)
        stop("provide 'seed' for each chain")
    if (length(unique(seed)) != n.chains)
        stop("'seed' must have 'n.chains' unique values")

    trace <- getOption("dcoptions")$verbose
    ## retrieves n.clones
    n.clones <- dclone::nclones.list(data)
    ## removes n.clones attr from each element of data
    data <- lapply(data, function(z) {
        attr(z, "n.clones") <- NULL
        z
    })

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


    if (is.null(inits))
        inits <- lapply(1:n.chains, function(i) NULL)
    if (is.function(inits))
        inits <- lapply(1:n.chains, function(i) inits)
    if (length(inits) != n.chains)
        stop("inits length incompatible with n.chains")

    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits,
        seed=seed, program=program)
    ## parallel function to evaluate by parDosa
    bugsparallel <- function(i, ...)   {
        cldata <- pullDcloneEnv("cldata", type = "model")
        bugs.fit(data=cldata$data, params=cldata$params,
            model=cldata$model,
            inits=cldata$inits[[i]], n.chains=1,
            seed=cldata$seed[i],
            program=cldata$program, format="mcmc.list",
            working.directory=NULL, ...)
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
    LIB <- "dclone"
    if (program == "winbugs")
        LIB <- c(LIB, "R2WinBUGS")
    if (program == "brugs")
        LIB <- c(LIB, "R2WinBUGS", "BRugs")
    if (program == "openbugs")
        LIB <- c(LIB, "R2OpenBUGS")

    mcmc <- parDosa(cl, 1:n.chains, bugsparallel, cldata,
        lib=LIB,
        balancing=balancing, size=1,
        rng.type=getOption("dcoptions")$RNG, cleanup=TRUE,
        dir=NULL, # model now has full path
        unload=FALSE, ...)
    ## binding the chains
    res <- as.mcmc.list(lapply(mcmc, as.mcmc))

    ## adding n.clones attribute, and class attr if mcmc.list
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    if (program == "winbugs")
        warnings("seed settings in WinBUGS are not best suited for parallel computations")
    if (program == "brugs")
        warning("program = 'openbugs' is the preferred interface")
    res
}
