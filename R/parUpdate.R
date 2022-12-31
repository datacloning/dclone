#' Update jags models on parallel workers
#' 
#' Update the Markov chain associated with the model on parallel workers.
#' (This represents the 'burn-in' phase when nodes are not monitored.)
#' 
#' 
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer. It can also be \code{NULL}, see \code{\link{parDosa}}.
#' @param object character, name of a jags model object
#' @param n.iter number of iterations of the Markov chain to run
#' @param list() additional arguments to the update method, see
#' \code{\link[rjags]{update.jags}}
#' @return The \code{parUpdate} function modifies the original object on
#' parallel workers and returns \code{NULL}.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link[rjags]{update.jags}}
#' 
#' See example on help page of \code{\link{parCodaSamples}}.
#' @keywords models
#' @export parUpdate
parUpdate <-
function(cl, object, n.iter = 1, ...)
{
    ## stop if rjags not found
    requireNamespace("rjags")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    clusterEvalQ(cl, requireNamespace("rjags"))
    if (!is.character(object))
        object <- as.character(object) # deparse(substitute(object))
    cldata <- list(n.iter=n.iter, name=object)
    jagsparallel <- function(i, ...) {
        ## cldata is pushed by parDosa
        cldata <- pullDcloneEnv("cldata", type = "model")
        if (existsDcloneEnv(cldata$name, type = "results")) {
            res <- pullDcloneEnv(cldata$name,
                type = "results")
            #rjags:::update.jags(res, n.iter=cldata$n.iter, ...)
            update(object=res, n.iter=cldata$n.iter, ...)
            pushDcloneEnv(cldata$name, res, type = "results")
        }
        NULL
    }
#    dir <- if (inherits(cl, "SOCKcluster")) # model now has full path
#        getwd() else NULL
    parDosa(cl, 1:length(cl), jagsparallel, cldata,
        lib = c("dclone", "rjags"), balancing = "none", size = 1,
        rng.type = getOption("dcoptions")$RNG,
        cleanup = TRUE,
        dir = NULL, # model now has full path
        unload = FALSE, ...)
}
