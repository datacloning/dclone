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
