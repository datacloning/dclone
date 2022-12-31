#' Setting Options
#' 
#' Setting options.
#' 
#' \code{dcoptions} is a convenient way of handling options related to the
#' package.
#' 
#' @param \dots Arguments in \code{tag = value} form, or a list of tagged
#' values. The tags must come from the parameters described below.
#' @return When parameters are set by \code{dcoptions}, their former values are
#' returned in an invisible named list. Such a list can be passed as an
#' argument to \code{dcoptions} to restore the parameter values. Tags are the
#' following:
#' 
#' \item{autoburnin}{logical, to use in \code{\link{gelman.diag}} (default is
#' \code{TRUE}).} \item{diag}{critical value to use for data cloning
#' convergence diagnostics, default is 0.05.} \item{LB}{logical, should load
#' balancing be used, default is \code{FALSE}.} \item{overwrite}{logical,
#' should existing model file be overwritten, default is \code{TRUE}.}
#' \item{rhat}{critical value for testing chain convergence, default is 1.1.}
#' \item{RNG}{parallel RNG type, either \code{"none"} (default), or
#' \code{"RNGstream"}, see \code{\link[parallel]{clusterSetRNGStream}}.}
#' \item{verbose}{integer, should output be verbose (>0) or not (0), default is
#' 1.}
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @keywords utilities
#' @examples
#' 
#' ## set LB option, but store old value
#' ov <- dcoptions("LB"=TRUE)
#' ## this is old value
#' ov
#' ## this is new value
#' getOption("dcoptions")
#' ## reset to old value
#' dcoptions(ov)
#' ## check reset
#' getOption("dcoptions")
#' 
#' 
#' @export dcoptions
dcoptions <-
function(...)
{
    opar <- getOption("dcoptions")
    args <- list(...)
    if (length(args)) {
        if (length(args) == 1 && is.list(args[[1]])) {
            npar <- args[[1]]
        } else {
            npar <- opar
            npar[match(names(args), names(npar))] <- args
        }
        options("dcoptions" = npar)
    }
    invisible(opar)
}
