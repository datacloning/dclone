#' Dynamically load JAGS modules on parallel workers
#' 
#' A JAGS module is a dynamically loaded library that extends the functionality
#' of JAGS. These functions load and unload JAGS modules and show the names of
#' the currently loaded modules on parallel workers.
#' 
#' 
#' @aliases parLoadModule parUnloadModule parListModules
#' @param cl a cluster object created by the \pkg{parallel} package.
#' @param name character, name of the module to be loaded
#' @param path file path to the location of the DLL. If omitted, the option
#' \code{jags.moddir} is used to locate the modules.  it can be a vector of
#' length \code{length(cl)} to set different DLL locations on each worker
#' @param quiet a logical. If \code{TRUE}, no message will be printed about
#' loading the module
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link[rjags]{list.modules}},
#' \code{\link[rjags]{load.module}}, \code{\link[rjags]{unload.module}}
#' @keywords interface
#' @examples
#' 
#' \dontrun{
#' if (require(rjags)) {
#' cl <- makePSOCKcluster(3)
#' parListModules(cl)
#' parLoadModule(cl, "glm")
#' parListModules(cl)
#' parUnloadModule(cl, "glm")
#' parListModules(cl)
#' stopCluster(cl)
#' }
#' }
#' 
#' @export parLoadModule
parLoadModule <-
function(cl, name, path, quiet = FALSE) 
{
    ## stop if rjags not found
    requireNamespace("rjags")
    clusterEvalQ(cl, requireNamespace("rjags"))
    if (missing(path)) {
        path <- clusterEvalQ(cl,  getOption("jags.moddir"))
        if (any(sapply(path, is.null))) {
            stop("option jags.moddir is not set")
        }
    } else {
        if (!(length(path)  %in% c(1, length(cl))))
            stop("invalid path length")
        if (length(path) == 1)
            path <- rep(path, length(cl))
    }
    fun <- function(path, name, quiet)
        rjags::load.module(name, path, quiet)
    parLapply(cl, path, fun, name=name, quiet=quiet)
}
