#' Parallel RNGs for initial values
#' 
#' This function takes care of initial values with safe RNGs based on
#' \code{\link[rjags]{parallel.seeds}} of the \pkg{rjags} package.
#' 
#' Initial values are handled similar to as it is done in
#' \code{\link[rjags]{jags.model}}.
#' 
#' RNGs are based on values returned by \code{\link[rjags]{parallel.seeds}}.
#' 
#' If the \code{"lecuyer"} JAGS module is active, RNGs are based on the
#' \code{"lecuyer::RngStream"} factory, otherwise those are based on the
#' \code{"base::BaseRNG"} factory.
#' 
#' @param inits Initial values (see Initialization at
#' \code{\link[rjags]{jags.model}}).  If \code{NULL}, an empty list of length
#' \code{n.chains} will be generated and seeded (RNG type and seed).
#' @param n.chains Number of chains to generate.
#' @return Returns a list of initial values with RNGs.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}. Based on Martyn
#' Plummer's \code{\link[rjags]{parallel.seeds}} function and code in
#' \code{\link[rjags]{jags.model}} for initial value handling in the
#' \pkg{rjags} package.
#' @seealso \code{\link[rjags]{parallel.seeds}},
#' \code{\link[rjags]{jags.model}}
#' 
#' This seeding function is used in all of \pkg{dclone}'s parallel functions
#' that do initialization: \code{\link{parJagsModel}},
#' \code{\link{jags.parfit}}, \code{\link{dc.parfit}}
#' @keywords utilities
#' @examples
#' 
#' if (require(rjags)) {
#' ## "base::BaseRNG" factory.
#' parallel.inits(NULL, 2)
#' ## "lecuyer::RngStream" factory
#' load.module("lecuyer")
#' parallel.inits(NULL, 2)
#' unload.module("lecuyer")
#' ## some non NULL inits specifications
#' parallel.inits(list(a=0), 2)
#' parallel.inits(list(list(a=0), list(a=0)), 2)
#' parallel.inits(function() list(a=0), 2)
#' parallel.inits(function(chain) list(a=chain), 2)
#' }
#' 
#' @export parallel.inits
parallel.inits <- 
function(inits, n.chains) 
{
    requireNamespace("rjags")
    factory <- if ("lecuyer" %in% rjags::list.modules())
        "lecuyer::RngStream" else "base::BaseRNG"
    RNGs <- rjags::parallel.seeds(factory, n.chains)
    if (missing(inits))
        inits <- NULL
    if (!is.null(inits)) {
        checkParameters <- function(inits) {
            if(!is.list(inits))
                return (FALSE)
            inames <- names(inits)
            if (is.null(inames) || any(nchar(inames) == 0))
                return (FALSE)
            if (any(duplicated(inames)))
                return (FALSE)
            if (any(inames==".RNG.name")) {
                rngname <- inits[[".RNG.name"]]
                if (!is.character(rngname) || length(rngname) != 1)
                    return (FALSE)
                inits[[".RNG.name"]] <- NULL
            }
            ## Strip null initial values, but give a warning
            null.inits <- sapply(inits, is.null)
            if (any(null.inits)) {
                warning("NULL initial values supplied for variable",
                    paste(inames[null.inits], sep=","))
                inits <- inits[!null.inits]
            }
            if (!all(sapply(inits, is.numeric)))
                return (FALSE)
            return (TRUE)
        }
        init.values <- vector("list", n.chains)
        if (is.function(inits)) {
            if (any(names(formals(inits)) == "chain")) {
                for (i in 1:n.chains) {
                    init.values[[i]] <- inits(chain=i)
                }
            }
            else {
                for (i in 1:n.chains) {
                    init.values[[i]] <- inits()
                }
            }
        }
        else if (is.list(inits)) {
            if (checkParameters(inits)) {
                ## Replicate initial values for all chains
                for (i in 1:n.chains) {
                    init.values[[i]] <- inits
                }
            }
            else if (!all(sapply(inits, checkParameters))) {
                stop("Invalid initial values")
            }
            else {
                if (length(inits) != n.chains) {
                    stop("Length mismatch in inits")
                }
                init.values <- inits
            }
        }
        for (i in 1:n.chains) {
            init.values[[i]]$.RNG.state <- RNGs[[i]]$.RNG.state
            init.values[[i]]$.RNG.name <- RNGs[[i]]$.RNG.name
        }
    } else init.values <- RNGs
    init.values
}
