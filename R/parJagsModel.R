#' Create a JAGS model object on parallel workers
#' 
#' \code{parJagsModel} is used to create an object representing a Bayesian
#' graphical model, specified with a BUGS-language description of the prior
#' distribution, and a set of data.
#' 
#' 
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer. It can also be \code{NULL}, see \code{\link{parDosa}}.  Size
#' of the cluster must be equal to or larger than \code{n.chains}.
#' @param name character, name for the model to be assigned on the workers.
#' @param file the name of the file containing a description of the model in
#' the JAGS dialect of the BUGS language.  Alternatively, \code{file} can be a
#' readable text-mode connection, or a complete URL. It can be also a function
#' or a \code{\link{custommodel}} object.
#' @param data a list or environment containing the data. Any numeric objects
#' in \code{data} corresponding to node arrays used in \code{file} are taken to
#' represent the values of observed nodes in the model
#' @param inits optional specification of initial values in the form of a list
#' or a function (see \code{Initialization} on help page of
#' \code{\link[rjags]{jags.model}}). If omitted, initial values will be
#' generated automatically. It is an error to supply an initial value for an
#' observed node.
#' @param n.chains the number of parallel chains for the model
#' @param n.adapt the number of iterations for adaptation. See
#' \code{\link[rjags]{adapt}} for details. If \code{n.adapt = 0} then no
#' adaptation takes place.
#' @param quiet if \code{TRUE} then messages generated during compilation will
#' be suppressed. Effect of this argument is not visible on the master process.
#' @return \code{parJagsModel} returns an object inheriting from class
#' \code{jags} which can be used to generate dependent samples from the
#' posterior distribution of the parameters. These \code{jags} models are
#' residing on the workers, thus updating/sampling is possible.
#' 
#' Length of \code{cl} must be equal to or greater than \code{n.chains}. RNG
#' seed generation takes place first on the master, and chains then initialized
#' on each worker by distributing \code{inits} and single chained models.
#' 
#' An object of class \code{jags} is a list of functions that share a common
#' environment, see \code{\link[rjags]{jags.model}} for details. Data cloning
#' information is attached to the returned object if data argument has
#' \code{n.clones} attribute.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Original sequential function in \pkg{rjags}:
#' \code{\link[rjags]{jags.model}}
#' 
#' Sequential \pkg{dclone}-ified version: \code{\link{jagsModel}}
#' 
#' See example on help page of \code{\link{parCodaSamples}}.
#' @keywords models
#' @export parJagsModel
parJagsModel <-
function(cl, name, file, data = sys.frame(sys.parent()),
inits, n.chains = 1, n.adapt = 1000, quiet = FALSE)
{
    ## stop if rjags not found
    requireNamespace("rjags")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    clusterEvalQ(cl, requireNamespace("rjags"))
    if (length(cl) < n.chains)
        stop("length(cl) < n.chains")
    if (is.function(file) || inherits(file, "custommodel")) {
        if (is.function(file))
            file <- match.fun(file)
        if (inherits(cl, "SOCKcluster")) {
            file <- write.jags.model(file)
            on.exit(try(clean.jags.model(file)))
        }
    }
    n.clones <- dclone::nclones.list(as.list(data))
    ## inits and RNGs
    if ("lecuyer" %in% list.modules()) {
        mod <- parListModules(cl)
        for (i in 1:length(mod)) {
            if (!("lecuyer" %in% mod[[i]]))
                stop("'lecuyer' module must be loaded on workers")
        }
    }
    inits <- if (missing(inits))
        parallel.inits(n.chains=n.chains) else parallel.inits(inits, n.chains)
#    inits <- jags.model(file, data, inits, n.chains,
#        n.adapt = 0)$state(internal = TRUE)
    if (!is.character(name))
        name <- as.character(name) # deparse(substitute(name))
    cldata <- list(file=file, data=as.list(data), inits=inits,
        n.adapt=n.adapt, name=name, quiet=quiet,
        n.adapt=n.adapt, quiet=quiet,
        n.clones=n.clones)
    jagsparallel <- function(i) {
        cldata <- pullDcloneEnv("cldata", type = "model")
        res <- rjags::jags.model(file=cldata$file, data=cldata$data,
            inits=cldata$inits[[i]], n.chains=1,
            n.adapt=cldata$n.adapt, quiet=cldata$quiet)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
        pushDcloneEnv(cldata$name, res, type = "results")
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster"))
        getwd() else NULL
    parDosa(cl, 1:n.chains, jagsparallel, cldata,
        lib = c("dclone", "rjags"), balancing = "none", size = 1,
        rng.type = getOption("dcoptions")$RNG,
        cleanup = TRUE,
        dir = NULL, # model now has full path
        unload=FALSE)
}
