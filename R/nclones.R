#' Number of Clones
#' 
#' Retrieves the number of clones from an object.
#' 
#' 
#' @aliases nclones nclones.list nclones.default
#' @param x An object.
#' @param \dots Other arguments to be passed.
#' @return Returns the number of of clones, or \code{NULL}.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link{dclone}}
#' @keywords manip
#' @examples
#' 
#' x <- dclone(1:10, 10)
#' nclones(x)
#' nclones(1:10) # this is NULL
#' 
#' @export nclones
nclones <-
function(x, ...)
{
    UseMethod("nclones")
}
