#' Calculations on 'mcmc.list' objects
#' 
#' Conveniently calculates statistics for mcmc.list objects.
#' 
#' \code{mcmcapply} returns a certain statistics based on \code{FUN} after
#' coercing into a matrix. \code{FUN} can be missing, in this case
#' \code{mcmcapply} is equivalent to calling \code{as.matrix} on an 'mcmc.list'
#' object.
#' 
#' \code{stack} can be used to concatenates 'mcmc.list' objects into a single
#' vector along with index variables indicating where each observation
#' originated from (e.g. iteration, variable, chain).
#' 
#' @aliases mcmcapply stack.mcmc.list
#' @param x Objects of class \code{mcmc.list}.
#' @param FUN A function to be used in the calculations, returning a single
#' value.
#' @param \dots Other arguments passed to \code{FUN}.
#' @return \code{mcmcapply} returns statistic value for each variable based on
#' \code{FUN}, using all values in all chains of the MCMC object.
#' 
#' \code{stack} returns a data frame with columns: iter, variable, chain,
#' value.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @keywords utilities
#' @examples
#' 
#' data(regmod)
#' mcmcapply(regmod, mean)
#' mcmcapply(regmod, sd)
#' 
#' x <- stack(regmod)
#' head(x)
#' summary(x)
#' library(lattice)
#' xyplot(value ~ iter | variable, data=x,
#'     type="l", scales = "free", groups=chain)
#' 
#' @export mcmcapply
mcmcapply <-
function(x, FUN, ...)
{
    if (!inherits(x, "mcmc.list"))
        stop("object class is not 'mcmc.list'")
    y <- as.matrix(x)
    if (missing(FUN))
        y else apply(y, 2, match.fun(FUN), ...)
}
