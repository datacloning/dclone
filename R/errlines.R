#' Plot error bars
#' 
#' The function plots error bars to existing plot.
#' 
#' The \code{errlines} function uses \code{\link{lines}} to draw error bars to
#' existing plot when \code{type = "l"}. \code{\link{polygon}} is used for
#' boxes when \code{type = "b"}.
#' 
#' If \code{code = 0} no ticks are drawn, if \code{code = 1}, only lower ticks
#' are drawn, if \code{code = 2} only lower ticks are drawn, if \code{code = 3}
#' both lower and upper ticks are drawn.
#' 
#' @aliases errlines errlines.default
#' @param x Numeric vector with coordinates along the horizontal axis (if
#' \code{vertical = FALSE}, this sets the vertical axis).
#' @param y A matrix-like object with 2 columns for lower and upper values on
#' the vertical axis (if \code{vertical = FALSE}, this sets the horizontal
#' axis).
#' @param type Character, \code{"l"} for lines, \code{"b"} for boxes to be
#' drawn.
#' @param code Integer code, determining the kind of ticks to be drawn. See
#' Details.
#' @param width Numeric, width of the ticks (if \code{type = "l"}) or width of
#' the boxes (if \code{type = "b"}).
#' @param vertical Logical, if errorbars should be plotted vertically or
#' horizontally.
#' @param col Color of the error lines to be drawn, recycled if needed.
#' @param bg If \code{type = "b"} the background color of the boxes.  By
#' default, no background color used.
#' @param \dots Other arguments passed to the function \code{\link{lines}}.
#' @return Adds error bars to an existing plot as a side effect. Returns
#' \code{NULL} invisibly.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso \code{\link{lines}}, \code{\link{polygon}}
#' @keywords aplot
#' @examples
#' 
#' x <- 1:10
#' a <- rnorm(10,10)
#' a <- a[order(a)]
#' b <- runif(10)
#' y <- cbind(a-b, a+b+rev(b))
#' opar <- par(mfrow=c(2, 3))
#' plot(x, a, ylim = range(y))
#' errlines(x, y)
#' plot(x, a, ylim = range(y))
#' errlines(x, y, width = 0.5, code = 1)
#' plot(x, a, ylim = range(y), col = 1:10)
#' errlines(x, y, width = 0.5, code = 3, col = 1:10)
#' plot(x, a, ylim = range(y))
#' errlines(x, y, width = 0.5, code = 2, type = "b")
#' plot(x, a, ylim = range(y))
#' errlines(x, y, width = 0.5, code = 3, type = "b")
#' plot(x, a, ylim = range(y), type = "n")
#' errlines(x, y, width = 0.5, code = 3, type = "b", bg = 1:10)
#' errlines(x, cbind(a-b/2, a+b/2+rev(b)/2))
#' points(x, a)
#' par(opar)
#' 
#' @export errlines
errlines <-
function(x, ...)
{
    UseMethod("errlines")
}
