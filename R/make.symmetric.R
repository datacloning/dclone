#' Make a square matrix symmetric by averaging.
#' 
#' Matrix symmetry might depend on numerical precision issues. The older
#' version of JAGS had a bug related to this issue for multivariate normal
#' nodes. This simple function can fix the issue, but new JAGS versions do not
#' require such intervention.
#' 
#' The function takes the average as \code{(x[i, j] + x[j, i]) / 2} for each
#' off diagonal cells.
#' 
#' @param x A square matrix.
#' @return A symmetric square matrix.
#' @note The function works for any matrix, even for those not intended to be
#' symmetric.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @keywords manip
#' @examples
#' 
#' x <- as.matrix(as.dist(matrix(1:25, 5, 5)))
#' diag(x) <- 100
#' x[lower.tri(x)] <- x[lower.tri(x)] - 0.1
#' x[upper.tri(x)] <- x[upper.tri(x)] + 0.1
#' x
#' make.symmetric(x)
#' 
#' @export make.symmetric
make.symmetric <-
function(x)
{
    if (dim(x)[1] != dim(x)[2])
        stop("'x' is not sqare matrix")
    rval <- t(x)
    m <- (x[lower.tri(x)] + rval[lower.tri(rval)]) / 2
    rval[lower.tri(rval)] <- m
    rval <- t(rval)
    rval[lower.tri(rval)] <- m
    rval
}

