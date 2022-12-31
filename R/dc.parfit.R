#' Parallel model fitting with data cloning
#' 
#' Iterative model fitting on parallel workers with different numbers of
#' clones.
#' 
#' The \code{dc.parfit} is a parallel computing version of
#' \code{\link[dclone]{dc.fit}}. After parallel computations, temporary objects
#' passed to workers and the \pkg{dclone} package is cleaned up. It is not
#' guaranteed that objects already on the workers and independently loaded
#' packages are not affected. Best to start new instances beforehand.
#' 
#' \code{partype="balancing"} distributes each model corresponding to values in
#' \code{n.clones} as jobs to workers according to size balancing (see
#' \code{\link{parDosa}}). \code{partype="parchains"} makes repeated calls to
#' \code{\link{jags.parfit}} for each value in \code{n.clones}.
#' \code{partype="both"} also calls \code{\link{jags.parfit}} but each chain of
#' each cloned model is distributed as separate job to the workers.
#' 
#' The vector \code{n.clones} is used to determine size balancing. If load
#' balancing is also desired besides of size balancing (e.g. due to unequal
#' performance of the workers, the option \code{"dclone.LB"} should be set to
#' \code{TRUE} (by using \code{options("dclone.LB" = TRUE)}).  By default, the
#' \code{"dclone.LB"} option is \code{FALSE} for reproducibility reasons.
#' 
#' Some arguments from \code{\link{dc.fit}} are not available in parallel
#' version (\code{update}, \code{updatefun}, \code{initsfun}) when size
#' balancing is used (\code{partype} is \code{"balancing"} or \code{"both"}).
#' These arguments are evaluated only when \code{partype="parchains"}.
#' 
#' Size balancing is recommended if \code{n.clones} is a relatively long
#' vector, while parallel chains might be more efficient when \code{n.clones}
#' has few elements. For efficiency reasons, a combination of the two
#' (\code{partype="both"}) is preferred if cluster size allows it.
#' 
#' Additionally loaded JAGS modules (e.g. \code{"glm"}) need to be loaded to
#' the workers.
#' 
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer, see \code{\link{parDosa}} and
#' \code{\link{evalParallelArgument}}.
#' @param data A named list (or environment) containing the data.
#' @param params Character vector of parameters to be sampled. It can be a list
#' of 2 vectors, 1st element is used as parameters to monitor, the 2nd is used
#' as parameters to use in calculating the data cloning diagnostics.
#' (\code{partype = "both"} currently cannot handle \code{params} as list.)
#' @param model Character string (name of the model file), a function
#' containing the model, or a or \code{\link{custommodel}} object (see
#' Examples).
#' @param inits Optional specification of initial values in the form of a list
#' or a function (see Initialization at \code{\link[rjags]{jags.model}}). If
#' missing, will be treated as \code{NULL} and initial values will be generated
#' automatically. If this is a function, it must be self containing, i.e. not
#' having references to R objects outside of the function, or the objects
#' should be exported with \code{\link{clusterExport}} before calling
#' \code{dc.parfit}.
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
#' enhancing convergence. This argument is ignored if size balancing is used
#' (default), and not ignored when multiple parallel chains are used.
#' @param updatefun A function to use for updating \code{data[[update]]}. It
#' should take an 'mcmc.list' object as 1st argument, 2nd argument can be the
#' number of clones. This argument is ignored if size balancing is used
#' (default), and not ignored when multiple parallel chains are used.
#' @param initsfun A function to use for generating initial values,
#' \code{inits} are updated by the object returned by this function from the
#' second iteration. If initial values are not dependent on the previous
#' iteration, this should be \code{NULL}, otherwise, it should take an
#' 'mcmc.list' object as 1st argument, 2nd argument can be the number of
#' clones. This feature is useful if latent nodes are provided in \code{inits}
#' so it also requires to be cloned for subsequent iterations. The 1st argument
#' of the \code{initsfun} function is ignored if \code{partype != "parchains"}
#' but the function must have a first argument regardless, see Examples.
#' @param flavour If \code{"jags"}, the function \code{\link{jags.fit}} is
#' called.  If \code{"bugs"}, the function \code{\link{bugs.fit}} is called
#' (available with \code{partype = "balancing"} only).  If \code{"stan"}, the
#' function \code{\link{stan.fit}} is called.  See Details.
#' @param partype Type of parallel workload distribution, see Details.
#' @param n.chains Number of chains to generate.
#' @param return.all Logical. If \code{TRUE}, all the MCMC list objects
#' corresponding to the sequence \code{n.clones} are returned for further
#' inspection (this only works with \code{partype = "parchains"}). Otherwise
#' only the MCMC list corresponding to highest number of clones is returned
#' with summary statistics for the rest.
#' @param check.nclones Logical, whether to check and ensure that values of
#' \code{n.clones} are unique and increasing. \code{check.nclones = FALSE}
#' means that \code{n.clones} is used as is, thus it is possible to supply
#' repeated values but still use the update functionality.
#' @param \dots Other values supplied to \code{\link[dclone]{jags.fit}}, or
#' \code{\link[dclone]{bugs.fit}}, depending on the \code{flavour} argument.
#' @return An object inheriting from the class 'mcmc.list'.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Sequential version: \code{\link[dclone]{dc.fit}}.
#' 
#' Optimizing the number of workers: \code{\link{clusterSize}},
#' \code{\link{plotClusterSize}}.
#' 
#' Underlying functions: \code{\link[dclone]{jags.fit}},
#' \code{\link[dclone]{bugs.fit}}.
#' @references Lele, S.R., B. Dennis and F. Lutscher, 2007. Data cloning: easy
#' maximum likelihood estimation for complex ecological models using Bayesian
#' Markov chain Monte Carlo methods. \emph{Ecology Letters} \strong{10},
#' 551--563.
#' 
#' Lele, S. R., K. Nadeem and B. Schmuland, 2010. Estimability and likelihood
#' inference for generalized linear mixed models using data cloning.
#' \emph{Journal of the American Statistical Association} \strong{105},
#' 1617--1625.
#' 
#' Solymos, P., 2010. dclone: Data Cloning in R. \emph{The R Journal}
#' \strong{2(2)}, 29--37. URL:
#' \url{https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
#' @keywords models htest
#' @examples
#' 
#' \dontrun{
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
#' k <- 1:3
#' ## sequential version
#' dcm <- dc.fit(dat, "beta", glm.model, n.clones=k, multiply="n",
#'     unchanged="np")
#' ## parallel version
#' cl <- makePSOCKcluster(3)
#' pdcm1 <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k,
#'     multiply="n", unchanged="np",
#'     partype="balancing")
#' pdcm2 <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k,
#'     multiply="n", unchanged="np",
#'     partype="parchains")
#' pdcm3 <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k,
#'     multiply="n", unchanged="np",
#'     partype="both")
#' summary(dcm)
#' summary(pdcm1)
#' summary(pdcm2)
#' summary(pdcm3)
#' stopCluster(cl)
#' ## multicore type forking
#' if (.Platform$OS.type != "windows") {
#' mcdcm1 <- dc.parfit(3, dat, "beta", glm.model, n.clones=k,
#'     multiply="n", unchanged="np",
#'     partype="balancing")
#' mcdcm2 <- dc.parfit(3, dat, "beta", glm.model, n.clones=k,
#'     multiply="n", unchanged="np",
#'     partype="parchains")
#' mcdcm3 <- dc.parfit(3, dat, "beta", glm.model, n.clones=k,
#'     multiply="n", unchanged="np",
#'     partype="both")
#' }
#' 
#' ## Using WinBUGS/OpenBUGS
#' library(R2WinBUGS)
#' data(schools)
#' dat <- list(J = nrow(schools), y = schools$estimate,
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
#' inits <- function(){
#'     list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
#'          sigma.theta=runif(1, 0, 100))
#' }
#' param <- c("mu.theta", "sigma.theta")
#' cl <- makePSOCKcluster(2)
#' if (.Platform$OS.type == "windows") {
#' sim2 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2,
#'     flavour="bugs", program="WinBUGS", multiply="J",
#'     n.iter=2000, n.thin=1)
#' summary(sim2)
#' }
#' sim3 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2,
#'     flavour="bugs", program="brugs", multiply="J",
#'     n.iter=2000, n.thin=1)
#' summary(sim3)
#' library(R2OpenBUGS)
#' sim4 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2,
#'     flavour="bugs", program="openbugs", multiply="J",
#'     n.iter=2000, n.thin=1)
#' summary(sim4)
#' stopCluster(cl)
#' 
#' ## simulation for Poisson GLMM with inits
#' set.seed(1234)
#' n <- 5
#' beta <- c(2, -1)
#' sigma <- 0.1
#' alpha <- rnorm(n, 0, sigma)
#' x <- runif(n)
#' X <- model.matrix(~x)
#' linpred <- crossprod(t(X), beta) + alpha
#' Y <- rpois(n, exp(linpred))
#' ## JAGS model as a function
#' jfun1 <- function() {
#'     for (i in 1:n) {
#'         Y[i] ~ dpois(lambda[i])
#'         log(lambda[i]) <- alpha[i] + inprod(X[i,], beta)
#'         alpha[i] ~ dnorm(0, 1/sigma^2)
#'     }
#'     for (j in 1:np) {
#'         beta[j] ~ dnorm(0, 0.001)
#'     }
#'     sigma ~ dlnorm(0, 0.001)
#' }
#' ## data
#' jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
#' ## inits with latent variable and parameters
#' ini <- list(alpha=rep(0,n), beta=rep(0, NCOL(X)))
#' ## model arg is necessary as 1st arg,
#' ## but not used when partype!=parchains
#' ifun <-
#' function(model, n.clones) {
#'     list(alpha=dclone(rep(0,n), n.clones),
#'         beta=c(0,0))
#' }
#' ## make cluster
#' cl <- makePSOCKcluster(2)
#' ## pass global n variable used in ifun to workers
#' tmp <- clusterExport(cl, "n")
#' ## fit the model
#' jmod2 <- dc.parfit(cl, jdata, c("beta", "sigma"), jfun1, ini,
#'     n.clones = 1:2, multiply = "n", unchanged = "np",
#'     initsfun=ifun, partype="balancing")
#' stopCluster(cl)
#' 
#' ## Using Stan
#' if (require(rstan)) {
#'     model <- custommodel("data {
#'           int<lower=0> N;
#'           vector[N] y;
#'           vector[N] x;
#'         }
#'         parameters {
#'           real alpha;
#'           real beta;
#'           real<lower=0> sigma;
#'         }
#'         model {
#'           alpha ~ normal(0,10);
#'           beta ~ normal(0,10);
#'           sigma ~ cauchy(0,5);
#'           for (n in 1:N)
#'             y[n] ~ normal(alpha + beta * x[n], sigma);
#'         }")
#'     N <- 100
#'     alpha <- 1
#'     beta <- -1
#'     sigma <- 0.5
#'     x <- runif(N)
#'     y <- rnorm(N, alpha + beta * x, sigma)
#'     dat <- list(N=N, y=y, x=x)
#'     params <- c("alpha", "beta", "sigma")
#'     ## compile on 1st time only
#'     fit0 <- stan.fit(dat, params, model)
#'     if (.Platform$OS.type != "windows") {
#'         ## utilize compiled fit0
#'         dcfit <- dc.parfit(cl=2, dat, params, model, n.clones=1:2,
#'             flavour="stan", multiply="N", fit=fit0)
#'         summary(dcfit)
#'         stan.model(dcfit)
#'         dcdiag(dcfit)
#'     }
#' }
#' }
#' 
#' @export dc.parfit
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
