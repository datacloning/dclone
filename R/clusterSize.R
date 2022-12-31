#' Optimizing the number of workers
#' 
#' These functions help in optimizing workload for the workers if problems are
#' of different size.
#' 
#' These functions help determine the optimal number of workers needed for
#' different sized problems ('size' indicates approximate processing time
#' here).  The number of workers needed depends on the type of balancing.
#' 
#' For the description of the balancing types, see \code{\link{parDosa}}.
#' 
#' @aliases clusterSize plotClusterSize
#' @param n Number of workers.
#' @param size Vector of problem sizes (recycled if needed). The default
#' \code{1} indicates equality of problem sizes.
#' @param balancing Character, type of balancing to perform, one of
#' \code{c("none", "load", "size", "both")}.
#' @param plot Logical, if a plot should be drawn.
#' @param col Color of the polygons for work load pieces.
#' @param xlim,ylim Limits for the x and the y axis, respectively (optional).
#' @param main Title of the plot, can be missing.
#' @param \dots Other arguments passed to \code{\link{polygon}}.
#' @return \code{clusterSize} returns a data frame with approximate processing
#' time as the function of the number of workers (rows, in
#' \code{1:length(size)}) and the type of balancing (\code{c("none", "load",
#' "size", "both")}).  Approximate processing time is calculated from values in
#' \code{size} without taking into account any communication overhead.
#' 
#' \code{plotClusterSize} invisibly returns the total processing time needed
#' for a setting given its arguments. As a side effect, a plot is produced (if
#' \code{plot = TRUE}).
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @keywords utilities
#' @examples
#' 
#' ## determine the number of workers needed
#' clusterSize(1:5)
#' ## visually compare balancing options
#' opar <- par(mfrow=c(2, 2))
#' plotClusterSize(2,1:5, "none")
#' plotClusterSize(2,1:5, "load")
#' plotClusterSize(2,1:5, "size")
#' plotClusterSize(2,1:5, "both")
#' par(opar)
#' 
#' @export clusterSize
clusterSize <-
function(size)
{
    m <- length(size)
    if (m==1)
        stop("'length(size)' > 1 is needed")
    res1 <- sapply(2:m, function(i) plotClusterSize(i, size, "none", plot=FALSE))
    res2 <- sapply(2:m, function(i) plotClusterSize(i, size, "load", plot=FALSE))
    res3 <- sapply(2:m, function(i) plotClusterSize(i, size, "size", plot=FALSE))
    r0 <- sum(size)
    data.frame(
        workers=1:m, 
        none=c(r0, res1),
        load=c(r0, res2), 
        size=c(r0, res3), 
        both=c(r0, res3))
}
