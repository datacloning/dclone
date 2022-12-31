#' Parallel wrapper function to call from within a function
#' 
#' \code{parDosa} is a wrapper function around many functionalities of the
#' \pkg{parallel} package. It is designed to work closely with MCMC fitting
#' functions, e.g. can easily be called from inside of a function.
#' 
#' The function uses 'snow' type clusters when \code{cl} is a cluster object.
#' The function uses 'multicore' type forking (shared memory) when \code{cl} is
#' an integer. The value from \code{getOption("mc.cores")} is used if the
#' argument is \code{NULL}.
#' 
#' The function sets the random seeds, loads packages \code{lib} onto the
#' cluster, sets the working directory as \code{dir}, exports \code{cldata} and
#' evaluates \code{fun} on \code{seq}.
#' 
#' No balancing (\code{balancing = "none"}) means, that the problem is split
#' into roughly equal subsets, without respect to \code{size} (see
#' \code{\link[parallel]{clusterSplit}}). This splitting is deterministic
#' (reproducible).
#' 
#' Load balancing (\code{balancing = "load"}) means, that the problem is not
#' splitted into subsets \emph{a priori}, but subsequent items are placed on
#' the worker which is empty (see \code{\link[parallel]{clusterApplyLB}} for
#' load balancing). This splitting is non-deterministic (might not be
#' reproducible).
#' 
#' Size balancing (\code{balancing = "size"}) means, that the problem is
#' splitted into subsets, with respect to \code{size} (see
#' \code{\link{clusterSplitSB}} and \code{\link{parLapplySB}}). In size
#' balancing, the problem is re-ordered from largest to smallest, and then
#' subsets are determined by minimizing the total approximate processing time.
#' This splitting is deterministic (reproducible).
#' 
#' Size and load balancing (\code{balancing = "both"}) means, that the problem
#' is re-ordered from largest to smallest, and then undeterministic load
#' balancing is used (see \code{\link{parLapplySLB}}). If \code{size} is
#' correct, this is identical to size balancing. This splitting is
#' non-deterministic (might not be reproducible).
#' 
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer. It can also be \code{NULL}, see Details.
#' @param seq A vector to split.
#' @param fun A function or character string naming a function.
#' @param cldata A list containing data. This list is then exported to the
#' cluster by \code{\link[parallel]{clusterExport}}. It is stored in a hidden
#' environment. Data in \code{cldata} can be used by \code{fun}.
#' @param lib Character, name of package(s). Optionally packages can be loaded
#' onto the cluster. More than one package can be specified as character
#' vector. Packages already loaded are skipped.
#' @param dir Working directory to use, if \code{NULL} working directory is not
#' set on workers (default). Can be a vector to set different directories on
#' workers.
#' @param evalq Character, expressions to evaluate, e.g. for changing global
#' options (passed to \code{\link[parallel]{clusterEvalQ}}). More than one
#' expressions can be specified as character vector.
#' @param balancing Character, type of balancing to perform (see Details).
#' @param size Vector of problem sizes (or relative performance information)
#' corresponding to elements of \code{seq} (recycled if needed). The default
#' \code{1} indicates equality of problem sizes.
#' @param rng.type Character, \code{"none"} will not set any seeds on the
#' workers, \code{"RNGstream"} selects the \code{"L'Ecuyer-CMRG"} RNG and then
#' distributes streams to the members of a cluster, optionally setting the seed
#' of the streams by \code{set.seed(iseed)} (otherwise they are set from the
#' current seed of the master process: after selecting the L'Ecuyer generator).
#' See \code{\link[parallel]{clusterSetRNGStream}}. The logical value
#' \code{!(rng.type == "none")} is used for forking (e.g. when \code{cl} is
#' integer).
#' @param cleanup logical, if \code{cldata} should be removed from the workers
#' after applying \code{fun}. If \code{TRUE}, effects of \code{dir} argument is
#' also cleaned up.
#' @param unload logical, if \code{pkg} should be unloaded after applying
#' \code{fun}.
#' @param iseed integer or \code{NULL}, passed to
#' \code{\link[parallel]{clusterSetRNGStream}} to be supplied to
#' \code{\link{set.seed}} on the workers, or NULL not to set reproducible
#' seeds.
#' @param \dots Other arguments of \code{fun}, that are simple values and not
#' objects. (Arguments passed as objects should be specified in \code{cldata},
#' otherwise those are not exported to the cluster by this function.)
#' @return Usually a list with results returned by the cluster.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Size balancing: \code{\link{parLapplySB}},
#' \code{\link{parLapplySLB}}, \code{\link{mclapplySB}}
#' 
#' Optimizing the number of workers: \code{\link{clusterSize}},
#' \code{\link{plotClusterSize}}.
#' 
#' \code{parDosa} is used internally by parallel \pkg{dclone} functions:
#' \code{\link{jags.parfit}}, \code{\link{dc.parfit}},
#' \code{\link{parJagsModel}}, \code{\link{parUpdate}},
#' \code{\link{parCodaSamples}}.
#' 
#' \code{parDosa} manipulates specific environments described on the help page
#' \code{\link{DcloneEnv}}.
#' @keywords utilities connection
#' @export parDosa
parDosa <-
function(cl, seq, fun, cldata,
    lib = NULL, dir = NULL, evalq = NULL,
    size = 1, balancing = c("none", "load", "size", "both"),
    rng.type = c("none", "RNGstream"),
    cleanup = TRUE, unload = FALSE, iseed=NULL, ...)
{
    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=TRUE)
## common stuff for snow and multicore
    rng.type <- match.arg(rng.type)
    TRUENAM <- "cldata"
    TMPNAM <- "cldata"
## snow cluster
    if (inherits(cl, "cluster")) {

        ## push cldata
        ## this uses type="model"
        TMPNAM <- tempfile("cldata", "")
        TMPNAM <- substr(TMPNAM, 2, nchar(TMPNAM))
        ## use TMPNAM to avoid overwiting object on workers
        pushDcloneEnv(TMPNAM, cldata, type = "model")
        on.exit(clearDcloneEnv(list=listDcloneEnv(type = "model"),
            type = "model"))

        #requireNamespace("parallel")
        balancing <- match.arg(balancing)
        ## loads lib on each worker
        if (!is.null(lib)) {
            pkglist <- unique(unlist(clusterEvalQ(cl, .packages())))
            for (i in lib)
                if (!(i %in% pkglist))
                    eval(parse(text=paste("clusterEvalQ(cl, library(", i, "))", sep="")))
        }
        ## set seed on each worker
        if (rng.type != "none") {
            if (rng.type == "SPRNG")
                stop("type=SPRNG is deprecated, use RNGstream instead")
            clusterSetRNGStream(cl=cl, iseed=iseed)
        }
        ## sets common working directory, but dir can be a vector as well
        if (!is.null(dir)) {
            if (cleanup)
                dirold <- clusterEvalQ(cl, getwd())
            dir <- rep(dir, length(cl))[1:length(cl)]
            dirnew <- lapply(dir, function(z) paste("setwd(\"", z, "\")", sep=""))
            parLapply(cl, dirnew, function(z) eval(parse(text=z)))
        }
        ## evaluates literal expressions if needed
        if (!is.null(evalq)) {
            for (i in evalq)
                eval(parse(text=paste("clusterEvalQ(cl,", i, ")")))
        }
        ## export TMPNAM to workers
        #clusterExport(cl, TMPNAM, envir = dclone:::.DcloneEnvModel)
        clusterExport(cl, TMPNAM, envir = .DcloneEnvModel)
        ## push TMPNAM into .env as TRUENAM
        eval(parse(text=paste("clusterEvalQ(cl, pushDcloneEnv(\"",
            TRUENAM, "\", ", TMPNAM, ", type = \"model\"))", sep="")))
        ## remove TMPNAM
        eval(parse(text=paste("clusterEvalQ(cl, rm(list=\"",
            TMPNAM, "\"))", sep="")))
        ## parallel work done here according to balancing
        res <- switch(balancing,
            "none" = parLapply(cl, seq, fun, ...),
            "load" = clusterApplyLB(cl, seq, fun, ...),
            "size" = parLapplySB(cl, seq, size=size, fun, ...),
            "both" = parLapplySLB(cl, seq, size=size, fun, ...))
        if (cleanup) {
            ## remove all objects from .env
            clusterEvalQ(cl,
                clearDcloneEnv(list=listDcloneEnv(type = "model"),
                type = "model"))
            ## set old dir
            if (!is.null(dir)) {
                dirold <- lapply(dirold, function(z) paste("setwd(\"", z, "\")", sep=""))
                parLapply(cl, dirold, function(z) eval(parse(text=z)))
            }
        }
        ## unload libs
        if (unload && !is.null(lib)) {
            for (i in lib)
                if (!(i %in% pkglist))
                    eval(parse(text=paste("clusterEvalQ(cl, detach(package:",
                        i, ", unload=TRUE))", sep="")))
        }
    } else {
## multicore
        ## TMPNAM is TRUENAM
        pushDcloneEnv(TMPNAM, cldata, type = "model")

        #require parallel
        if (balancing == "load") {
            balancing <- "none"
            warning("forking is used: balancing type was set to 'none'")
        }
        if (balancing == "both") {
            balancing <- "size"
            warning("forking is used: balancing type was set to 'size'")
        }
        res <- mclapplySB(seq, fun, ...,
            mc.preschedule = (balancing == "none"), # no need to preschedule when SB
            mc.set.seed = !(rng.type == "none"),
            mc.silent = as.logical(getOption("dcoptions")$verbose),
            mc.cores = cl,
            mc.cleanup = cleanup,
            mc.allow.recursive = FALSE, # no need for recursive forking
            size = size)
    }
    res
}
