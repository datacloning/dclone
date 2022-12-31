#' Size balancing
#' 
#' Functions for size balancing.
#' 
#' \code{clusterSplitSB} splits \code{seq} into subsets, with respect to
#' \code{size}. In size balancing, the problem is re-ordered from largest to
#' smallest, and then subsets are determined by minimizing the total
#' approximate processing time. This splitting is deterministic (reproducible).
#' 
#' \code{parLapplySB} and \code{parLapplySLB} evaluates \code{fun} on elements
#' of \code{x} in parallel, similarly to \code{\link[parallel]{parLapply}}.
#' \code{parLapplySB} uses size balancing (via \code{clusterSplitSB}).
#' \code{parLapplySLB} uses size and load balancing. This means that the
#' problem is re-ordered from largest to smallest, and then undeterministic
#' load balancing is used (see \code{\link[parallel]{clusterApplyLB}}). If
#' \code{size} is correct, this is identical to size balancing. This splitting
#' is non-deterministic (might not be reproducible).
#' 
#' @aliases clusterSplitSB parLapplySB parLapplySLB
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}}
#' the the package \pkg{parallel}.
#' @param x,seq A vector to split.
#' @param fun A function or character string naming a function.
#' @param size Vector of problem sizes (approximate processing times)
#' corresponding to elements of \code{seq} (recycled if needed). The default
#' \code{1} indicates equality of problem sizes.
#' @param \dots Other arguments of \code{fun}.
#' @return \code{clusterSplitSB} returns a list of subsets split with respect
#' to \code{size}.
#' 
#' \code{parLapplySB} and \code{parLapplySLB} evaluates \code{fun} on elements
#' of \code{x}, and return a result corresponding to \code{x}. Usually a list
#' with results returned by the cluster.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Related functions without size balancing:
#' \code{\link[parallel]{clusterSplit}}, \code{\link[parallel]{parLapply}}.
#' 
#' Underlying functions: \code{\link[parallel]{clusterApply}},
#' \code{\link[parallel]{clusterApplyLB}}.
#' 
#' Optimizing the number of workers: \code{\link{clusterSize}},
#' \code{\link{plotClusterSize}}.
#' @keywords utilities connection
#' @examples
#' 
#' \dontrun{
#' cl <- makePSOCKcluster(2)
#' ## equal sizes, same as clusterSplit(cl, 1:5)
#' clusterSplitSB(cl, 1:5)
#' ## different sizes
#' clusterSplitSB(cl, 1:5, 5:1)
#' x <- list(1, 2, 3, 4)
#' parLapplySB(cl, x, function(z) z^2, size=1:4)
#' stopCluster(cl)
#' }
#' 
#' @export clusterSplitSB
clusterSplitSB <-
function(cl=NULL, seq, size = 1)
{
    if (is.null(cl))
        stop("no cluster 'cl' supplied")
    if (!inherits(cl, "cluster"))
        stop("not a valid cluster")
    m <- length(seq)
    size <- rep(size, m)[1:m]
    ## equal size
    if (length(unique(size)) == 1)
        return(clusterSplit(cl, seq))
    ## unequal size
    n <- length(cl)
    id <- 1:m
    ord <- order(size, decreasing = TRUE)
    size <- size[ord]
    id <- id[ord]
    w <- matrix(0, max(1,m-n+1), n)
    s <- matrix(NA, max(1,m-n+1), n)
    w[1,1:n] <- size[1:n]
    s[1,1:n] <- id[1:n]
    if (n < m)
        for (i in 2:nrow(w)) {
            j <- which(colSums(w) == min(colSums(w)))[1]
            w[i, j] <- size[i+n-1]
            s[i, j] <- id[i+n-1]
        }
    spl <- lapply(1:n, function(i) s[!is.na(s[,i]),i])
    rval <- lapply(spl, function(z) seq[z])
    if (n > length(rval))
        for (i in (length(rval)+1):n) {
            rval[[i]] <- numeric(0)
        }
    rval
}
