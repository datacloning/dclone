dc.parfit <-
function(cl, data, params, model, inits, n.clones, multiply = NULL,
unchanged = NULL, update = NULL, updatefun = NULL, initsfun = NULL,
flavour = c("jags", "bugs", "stan"),
n.chains = 3,
partype = c("balancing", "parchains", "both"),
return.all=FALSE, check.nclones=TRUE, ...)
{
    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=FALSE)
    ## sequential evaluation falls back on dc.fit
    if (is.null(cl)) {
        return(dc.fit(data, params, model, inits, n.clones,
            multiply = multiply, unchanged = unchanged,
            update = update, updatefun = updatefun,
            initsfun = initsfun, flavour = flavour,
            n.chains = n.chains, return.all=return.all,
            check.nclones=check.nclones, ...))
    }
    ## parallel evaluation starts here
    flavour <- match.arg(flavour)
    if (flavour=="jags" && !is.null(list(...)$updated.model))
        stop("'updated.model' argument is not available for parallel computations")
    if (flavour=="bugs" && !is.null(list(...)$format))
        if (list(...)$format == "bugs")
            stop("format='bugs' is not available for parallel computations")
    if (flavour=="stan" && !is.null(list(...)$stan.model))
        stop("'stan.model' argument is not available for parallel computations")
    ## get parallel type
    partype <- match.arg(partype)
    ## some arguments are ignored with size balancing
    if (partype != "parchains") {
        if (!is.null(updatefun))
            warnings("'updatefun' argument is ignored when partype != 'parchains'")
        if (!is.null(update))
            warnings("'update' argument is ignored when partype != 'parchains'")
    }

#    if (partype != "balancing" && flavour == "bugs")
#        stop("flavour='bugs' supported for 'balancing' type only")

    ## this is bugs.fit/parfit argument
    PROGRAM <- list(...)$program

    if (length(n.clones) < 2 && partype=="balancing") {
        warnings("no need for parallel computing with 'balancing'")
    }
    ## multiple parallel chains
    if (partype == "parchains") {
        mod <- dclone::.dcFit(data, params, model, inits, n.clones,
            multiply=multiply, unchanged=unchanged,
            update=update, updatefun=updatefun,
            initsfun=initsfun, flavour = flavour,
            cl=cl, parchains=TRUE, n.chains=n.chains,
            return.all=return.all, check.nclones=check.nclones, ...)
    ## size balancing and balancing+parchains
    } else {
        if (return.all)
            stop("return.all=TRUE works only with 'parchains'")
        if (missing(n.clones))
            stop("'n.clones' argument must be provided")
#        if (identical(n.clones, 1))
#            stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
        if (is.environment(data)) {
            warnings("'data' was environment: it was coerced into a list")
            data <- as.list(data)
        }
        ## determine k
        k <- n.clones[order(n.clones)]
        k <- unique(k)
        times <- length(k)
        ## global options
        rhat.opts <- getOption("dcoptions")$rhat
        trace <- getOption("dcoptions")$verbose
        ## evaluate inits
        if (missing(inits))
            inits <- NULL
        if (!is.null(initsfun)) {
            initsfun <- match.fun(initsfun)
            ian <- length(names(formals(initsfun)))
            if (ian == 0)
                stop("'initsfun' must have at least one argument")
            else warnings("first (model) argument of 'initsfun' is ignored when partype != 'parcains'")
            if (ian > 2)
                warnings("arguments of 'initsfun' after position 2 are ingnored")
            INIARGS <- ian < 2
        } else INIARGS <- 0
        ## params to use in jags.fit and in dcdiag
        if (is.list(params)) {
            params.diag <- params[[2]]
            params <- params[[1]]
        } else {
            params.diag <- params
        }
        ## partype="both" is somehow denies to do it right
        if (partype == "both" && !identical(params, params.diag))
            stop("partype='both' cannot handle params as list")
        #### parallel part
        if (trace) {
            cat("\nParallel computation in progress\n\n")
            flush.console()
        }
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
        ## common data
        cldata <- list(data=data, params=params, model=model, inits=inits,
            multiply=multiply, unchanged=unchanged, k=k,
            INIARGS=INIARGS, initsfun=initsfun, n.chains=n.chains,
            params.diag=params.diag)
        ## parallel computations
        balancing <- if (!getOption("dcoptions")$LB)
            "size" else "both"
#        dir <- if (inherits(cl, "SOCKcluster")) # model now has full path
#            getwd() else NULL
        ## size balancing
        if (partype == "balancing") {
            ## parallel function
            dcparallel <- function(i, ...) {
                cldata <- pullDcloneEnv("cldata", type = "model")
                jdat <- dclone(cldata$data, i, multiply=cldata$multiply,
                    unchanged=cldata$unchanged)
                INITS <- if (!is.null(cldata$initsfun) && !cldata$INIARGS)
                    initsfun(,i) else cldata$inits
                if (flavour == "jags") {
                    mod <- jags.fit(data=jdat, params=cldata$params, model=cldata$model,
                        inits=INITS, n.chains=cldata$n.chains, ...)
                }
                if (flavour == "bugs") {
                    mod <- bugs.fit(data=jdat, params=cldata$params, model=cldata$model,
                        inits=INITS, n.chains=cldata$n.chains, format="mcmc.list", ...)
                }
                if (flavour == "stan") {
                    mod <- stan.fit(data=jdat, params=cldata$params, model=cldata$model,
                        inits=INITS, n.chains=cldata$n.chains, format="mcmc.list", ...)
                }
                vn <- varnames(mod)
                params.diag <- vn[unlist(lapply(cldata$params.diag, grep, x=vn))]
                if (i == max(k))
                    return(mod) else return(list(dct=dclone::extractdctable(mod),
                        dcd=dclone::extractdcdiag(mod[,params.diag])))
            }
            if (flavour == "jags") {
                LIB <- c("dclone", "rjags")
            }
            if (flavour == "bugs") {
                LIB <- "dclone"
                if (is.null(PROGRAM))
                    PROGRAM <- "winbugs"
                if (PROGRAM == "winbugs")
                    LIB <- c(LIB, "R2WinBUGS")
                if (PROGRAM == "brugs")
                    LIB <- c(LIB, "R2WinBUGS", "BRugs")
                if (PROGRAM == "openbugs")
                    LIB <- c(LIB, "R2OpenBUGS")
            }
            if (flavour == "stan") {
                LIB <- c("dclone", "rstan")
            }
            pmod <- parDosa(cl, k, dcparallel, cldata,
                lib=LIB, balancing=balancing, size=k,
                rng.type=getOption("dcoptions")$RNG,
                cleanup=TRUE,
                dir=NULL, # model now has full path
                unload=FALSE, ...)
            mod <- pmod[[times]]
            ## dctable
            dct <- lapply(1:(times-1), function(i) pmod[[i]]$dct)
            dct[[times]] <- extractdctable(mod)
            ## dcdiag
            dcd <- lapply(1:(times-1), function(i) pmod[[i]]$dcd)
            vn <- varnames(mod)
            params.diag <- vn[unlist(lapply(params.diag, grep, x=vn))]
            dcd[[times]] <- extractdcdiag(mod[,params.diag])
#            dcd[[times]] <- extractdcdiag(mod)
        ## balancing+parchains
        } else {
            ## RNG and initialization
            if (inherits(cl, "cluster") && "lecuyer" %in% list.modules()) {
                mod <- parListModules(cl)
                for (i in 1:length(mod)) {
                    if (!("lecuyer" %in% mod[[i]]))
                        stop("'lecuyer' module must be loaded on workers")
                }
            }
            dcinits <- function(i) {
                INITS <- if (!is.null(cldata$initsfun) && !cldata$INIARGS)
                    initsfun(,i) else cldata$inits
                inits <- parallel.inits(INITS, n.chains)
            }
            ## parDosa with cleanup (but cldata changes, has to be passed again)
            pini <- lapply(k, dcinits)
            cldata$inits <- do.call("c", pini)
            cldata$k <- rep(k, each=n.chains)
            ## parallel function to evaluate by parDosa
            dcparallel <- function(i, ...) {
                cldata <- pullDcloneEnv("cldata", type = "model")
                jdat <- dclone(cldata$data, cldata$k[i],
                    multiply=cldata$multiply, unchanged=cldata$unchanged)
                if (flavour == "jags") {
                    mod <- jags.fit(data=jdat, params=cldata$params,
                        model=cldata$model, inits=cldata$inits[[i]],
                        n.chains=1, updated.model=FALSE, ...)
                }
                if (flavour == "bugs") {
                    mod <- bugs.fit(data=jdat, params=cldata$params,
                        model=cldata$model, inits=cldata$inits[[i]],
                        n.chains=1, format="mcmc.list", ...)
                }
                if (flavour == "stan") {
                    mod <- stan.fit(data=jdat, params=cldata$params,
                        model=cldata$model, inits=cldata$inits[[i]],
                        n.chains=1, format="mcmc.list", ...)
                }
            }
            pmod <- parDosa(cl, 1:(times*n.chains), dcparallel, cldata,
                lib=c("dclone", "rjags"), balancing=balancing, size=cldata$k,
                rng.type=getOption("dcoptions")$RNG,
                cleanup=TRUE,
                dir=NULL, # model now has full path
                unload=FALSE, ...)
            ## binding the chains for each k value
            assemblyfun <- function(mcmc) {
                n.clones <- nclones(mcmc)
                res <- as.mcmc.list(lapply(mcmc, as.mcmc))
                if (!is.null(n.clones) && n.clones > 1) {
                    attr(res, "n.clones") <- n.clones
                    class(res) <- c("mcmc.list.dc", class(res))
                }
                res
            }
            i.end <- 1:times*n.chains
            i.start <- i.end+1-n.chains
            pmod <- lapply(1:times, function(i)
                assemblyfun(pmod[i.start[i]:i.end[i]]))
            mod <- pmod[[times]]
            ## dctable
            dct <- lapply(pmod, extractdctable)
            ## dcdiag
## partype="both" is somehow denies to do it right
#            vn <- varnames(mod)
#            params.diag <- vn[unlist(lapply(params.diag, grep, x=vn))]
#            dcd <- lapply(pmod, function(z) extractdcdiag(z[,params.diag]))
            dcd <- lapply(pmod, extractdcdiag)
        }
        ## warning if R.hat < crit
        rhat.problem <- any(dct[[times]][,"r.hat"] >= rhat.opts)
        if (any(is.na(rhat.problem))) {
            rhat.problem[is.na(rhat.problem)] <- FALSE
        }
        if (nchain(mod) > 1 && rhat.problem)
            warning("chains convergence problem, see R.hat values")
        ## finalizing dctable attribute
        rnam <- lapply(dct, rownames)
        nam <- rnam[[1]]
        dct2 <- vector("list", length(nam))
        names(dct2) <- rownames(dct[[1]])
        for (i in 1:length(nam)) {
            dct2[[i]] <- cbind(n.clones = k, t(sapply(dct, function(z) z[i, ])))
        }
        dct2 <- lapply(dct2, function(z) as.data.frame(z))
        class(dct2) <- "dctable"
        attr(mod, "dctable") <- dct2
        ## finalizing dcdiag attribute
        dcd2 <- as.data.frame(matrix(unlist(dcd), nrow=length(dcd), byrow=TRUE))
        colnames(dcd2) <- names(dcd[[1]])
        class(dcd2) <- c("dcdiag", class(dcd2))
        attr(mod, "dcdiag") <- dcd2
    }
    mod
}
