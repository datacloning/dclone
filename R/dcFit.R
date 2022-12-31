#' Internal function for iterative model fitting with data cloning
#' 
#' This is the workhorse for \code{\link{dc.fit}} and \code{\link{dc.parfit}}.
#' 
#' 
#' @param data A named list (or environment) containing the data.
#' @param params Character vector of parameters to be sampled. It can be a list
#' of 2 vectors, 1st element is used as parameters to monitor, the 2nd is used
#' as parameters to use in calculating the data cloning diagnostics.
#' @param model Character string (name of the model file), a function
#' containing the model, or a \code{\link{custommodel}} object (see Examples).
#' @param inits Optional specification of initial values in the form of a list
#' or a function (see Initialization at \code{\link[rjags]{jags.model}}). If
#' missing, will be treated as \code{NULL} and initial values will be generated
#' automatically.
#' @param n.clones An integer vector containing the numbers of clones to use
#' iteratively.
#' @param multiply Numeric or character index for list element(s) in the
#' \code{data} argument to be multiplied by the number of clones instead of
#' repetitions.
#' @param unchanged Numeric or character index for list element(s) in the
#' \code{data} argument to be left unchanged.
#' @param update Numeric or character index for list element(s) in the
#' \code{data} argument that has to be updated by \code{updatefun} in each
#' iterations. This usually is for making priors more informative, and
#' enhancing convergence. See Details and Examples.
#' @param updatefun A function to use for updating \code{data[[update]]}. It
#' should take an 'mcmc.list' object as 1st argument, 2nd argument can be the
#' number of clones. See Details and Examples.
#' @param initsfun A function to use for generating initial values,
#' \code{inits} are updated by the object returned by this function from the
#' second iteration. If initial values are not dependent on the previous
#' iteration, this should be \code{NULL}, otherwise, it should take an
#' 'mcmc.list' object as 1st argument, 2nd argument can be the number of
#' clones. This feature is useful if latent nodes are provided in \code{inits}
#' so it also requires to be cloned for subsequent iterations. See Details and
#' Examples.
#' @param flavour If \code{"jags"}, the function \code{\link{jags.fit}} is
#' called.  If \code{"bugs"}, the function \code{\link{bugs.fit}} is called.
#' If \code{"stan"}, the function \code{\link{stan.fit}} is called.
#' @param n.chains Number of chains to generate.
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer, see \code{\link{parDosa}} and
#' \code{\link{evalParallelArgument}}.
#' @param parchains Logical, whether parallel chains should be run.
#' @param return.all Logical. If \code{TRUE}, all the MCMC list objects
#' corresponding to the sequence \code{n.clones} are returned for further
#' inspection (this only works with \code{partype = "parchains"}). Otherwise
#' only the MCMC list corresponding to highest number of clones is returned
#' with summary statistics for the rest.
#' @param check.nclones Logical, whether to check and ensure that values of
#' \code{n.clones} are unique and increasing. \code{check.nclones = FALSE}
#' means that \code{n.clones} is used as is, thus it is possible to supply
#' repeated values but still use the update functionality.
#' @param \dots Other values supplied to \code{\link{jags.fit}}, or
#' \code{\link{bugs.fit}}, depending on the \code{flavour} argument.
#' @return An object inheriting from the class 'mcmc.list'.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}, implementation is based
#' on many discussions with Khurram Nadeem and Subhash Lele.
#' @seealso \code{\link{dc.fit}}, \code{\link{dc.parfit}}
#' @keywords models htest
#' @export .dcFit
.dcFit <-
function(data, params, model, inits, n.clones, multiply = NULL, unchanged = NULL,
update = NULL, updatefun = NULL, initsfun = NULL,
flavour = c("jags", "bugs", "stan"),
n.chains=3, cl = NULL, parchains = FALSE, return.all=FALSE,
check.nclones = TRUE, ...)
{
    flavour <- match.arg(flavour)
    ## initail evals
    if (missing(n.clones))
        stop("'n.clones' argument must be provided")
#    if (identical(n.clones, 1))
#        stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    ## determine k
    if (check.nclones) {
        k <- n.clones[order(n.clones)]
        k <- unique(k)
    } else {
        k <- n.clones
    }
    times <- length(k)
    rhat.crit <- getOption("dcoptions")$rhat
    trace <- getOption("dcoptions")$verbose
    ## evaluate updating
    if (!is.null(update) != !is.null(updatefun))
        stop("both 'update' and 'updatefun' must be provided")
    if (!is.null(update)) {
        unchanged <- unique(c(unchanged, update))
        updatefun <- match.fun(updatefun)
        UPARGS <- length(names(formals(updatefun))) < 2
    }
    ## evaluate inits
    if (missing(inits))
        inits <- NULL
    if (!is.null(initsfun)) {
        initsfun <- match.fun(initsfun)
        ian <- length(names(as.list(args(initsfun))))-1
        if (ian == 0)
            stop("'initsfun' must have at least one argument")
        if (ian > 2)
            warnings("arguments of 'initsfun' after position 2 are ingnored")
        INIARGS <- ian < 2
    }
    ## params to use in jags.fit and in dcdiag
    if (is.list(params)) {
        params.diag <- params[[2]]
        params <- params[[1]]
    } else {
        params.diag <- params
    }
    ## list for dcdiag results
    dcdr <- list()
    ## iteration starts here
    if (return.all) {
        out.all <- vector("list", times)
        names(out.all) <- paste0("nclones=", k)
    }
    for (i in seq_len(times)) {
        tmpch <- if (k[i] == 1) "clone" else "clones"
        if (trace) {
            cat("\nFitting model with", k[i], tmpch, "\n\n")
            flush.console()
        }
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)

        ## it is problematic when k>1 at 1st iteration
        ## user should fall back on jags.fit???
#        if (!is.null(initsfun) && k[i]>1 && !INIARGS) {
#            inits <- initsfun(, k[i])
#        }

        if (flavour == "jags") {
            if (parchains) {
                mod <- jags.parfit(cl, jdat, params, model, inits, n.chains, ...)
            } else {
                mod <- jags.fit(jdat, params, model, inits, n.chains, ...)
            }
        }
        if (flavour == "bugs") {
            if (parchains) {
                mod <- bugs.parfit(cl, jdat, params, model, inits,
                    n.chains=n.chains, format="mcmc.list", ...)
            } else {
                mod <- bugs.fit(jdat, params, model, inits,
                    n.chains=n.chains, format="mcmc.list", ...)
            }
        }
        if (flavour == "stan") {
            if (parchains) {
                mod <- stan.parfit(cl, jdat, params, model, inits,
                    n.chains=n.chains, format="mcmc.list", ...)
            } else {
                mod <- stan.fit(jdat, params, model, inits,
                    n.chains=n.chains, format="mcmc.list", ...)
            }
        }
        if (return.all)
            out.all[[i]] <- mod
        ## dctable evaluation
        if (i == 1) {
            vn <- varnames(mod)
            dcts <- list()
            ## note: quantiles must remain unchanged, because these values are
            ## defined in extractdctable.default
            quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
            dcts0 <- matrix(0, times, 4 + length(quantiles))
            dcts0[,1] <- k
            colnames(dcts0) <- c("n.clones", "mean", "sd", names(quantile(0, probs=quantiles)), "r.hat")
            for (j in 1:length(vn))
                dcts[[vn[j]]] <- dcts0
        }
        ## updating
        if (i < times) {
            if (!is.null(update)) {
                jdatUp <- if (UPARGS)
                    updatefun(mod) else updatefun(mod, k[i+1])
                if (length(update) > 1L) {
                    if (!is.list(jdatUp))
                        stop("updatefun must return a named list when length(update) > 1")
                    jdat[update] <- jdatUp[update]
                } else {
                    jdat[[update]] <- jdatUp
                }
            }
            if (!is.null(initsfun))
                inits <- if (INIARGS)
                    initsfun(mod) else initsfun(mod, k[i+1])
        }
        dctmp <- dclone::extractdctable.default(mod)
        ## params.diag needs to subset varnames and not params
        if (i == 1) {
            vn <- varnames(mod)
            params.diag <- vn[unlist(lapply(params.diag, grep, x=vn))]
        }
        dcdr[[i]] <- dclone::extractdcdiag.default(mod[,params.diag])
        for (j in 1:length(vn)) {
            dcts[[j]][i,-1] <- dctmp[j,]
        }
    }
    ## return list if return.all=TRUE, proceed otherwise
    if (return.all)
        return(out.all)
    ## warning if R.hat < crit
    rhat.problem <- any(dctmp[,"r.hat"] >= rhat.crit)
    if (any(is.na(rhat.problem))) {
        rhat.problem[is.na(rhat.problem)] <- FALSE
    }
    if (nchain(mod) > 1 && rhat.problem)
        warning("chains convergence problem, see R.hat values")
    ## finalizing dctable attribute
    dcts <- lapply(dcts, function(z) as.data.frame(z))
    class(dcts) <- "dctable"
    attr(mod, "dctable") <- dcts
    ## finalizing dcdiag attribute
    dcd <- t(as.data.frame(dcdr))
    rownames(dcd) <- 1:length(dcdr)
    dcd <- data.frame(dcd)
    ## this next line went to dcdiag.default, but strange things happen with 1 param case
    colnames(dcd) <- c("n.clones", "lambda.max", "ms.error", "r.squared", "r.hat")
    class(dcd) <- c("dcdiag", class(dcd))
    attr(mod, "dcdiag") <- dcd
    mod
}
