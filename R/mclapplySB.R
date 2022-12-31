#' Size balancing version of mclapply
#' 
#' \code{mclapplySB} is a size balancing version of
#' \code{\link[parallel]{mclapply}}.
#' 
#' \code{\link[parallel]{mclapply}} gives details of the forking mechanism.
#' 
#' \code{\link[parallel]{mclapply}} is used unmodified if sizes of the jobs are
#' equal (\code{length(unique(size)) == 1}). Size balancing (as described in
#' \code{\link{parDosa}}) is used to balance workload on the child processes
#' otherwise.
#' 
#' @param X a vector (atomic or list) or an expressions vector.  Other objects
#' (including classed objects) will be coerced by \code{\link{as.list}}.
#' @param FUN the function to be applied to each element of \code{X}
#' @param ... optional arguments to \code{FUN}
#' @param mc.preschedule see \code{\link[parallel]{mclapply}}
#' @param mc.set.seed see \code{\link[parallel]{mclapply}}
#' @param mc.silent see \code{\link[parallel]{mclapply}}
#' @param mc.cores The number of cores to use, i.e. how many processes will be
#' spawned (at most)
#' @param mc.cleanup see \code{\link[parallel]{mclapply}}
#' @param mc.allow.recursive see \code{\link[parallel]{mclapply}}
#' @param size Vector of problem sizes (or relative performance information)
#' corresponding to elements of \code{X} (recycled if needed).  The default
#' \code{1} indicates equality of problem sizes.
#' @return A list.
#' @author Peter Solymos
#' @seealso \code{\link[parallel]{mclapply}}, \code{\link{parDosa}}
#' @keywords interface
#' @export mclapplySB
mclapplySB <- 
function(X, FUN, ..., 
mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = 1L,
mc.cleanup = TRUE, mc.allow.recursive = TRUE, 
size = 1)
{
    if (length(unique(size) == 1)) {
        res <- parallel::mclapply(X, 
            FUN, ..., 
            mc.preschedule = mc.preschedule, 
            mc.set.seed = mc.set.seed,
            mc.silent = mc.silent, 
            mc.cores = mc.cores,
            mc.cleanup = mc.cleanup, 
            mc.allow.recursive = mc.allow.recursive)
    } else {
        s <- clusterSplitSB(1:mc.cores, X, size)
        id <- clusterSplitSB(1:mc.cores, 1:length(X), size)
        mcfun <- function(x, ...)
            lapply(x, FUN, ...)
        res <- parallel::mclapply(s, 
            mcfun, ..., 
            mc.preschedule = mc.preschedule, 
            mc.set.seed = mc.set.seed,
            mc.silent = mc.silent, 
            mc.cores = mc.cores,
            mc.cleanup = mc.cleanup, 
            mc.allow.recursive = mc.allow.recursive)
        res <- do.call(c, res)
        res <- res[order(unlist(id))]
    }
    res
}


