## stan.fit for dclone
stan.fit <-
function(data, params, model, inits = NULL,
    seed = sample.int(.Machine$integer.max, 1),
    n.chains = 3,
    format = c("mcmc.list", "stanfit"),
    stan.model = TRUE, fit = NA, ...)
{
    requireNamespace("rstan")
    format <- match.arg(format)
    if (missing(params))
        params <- NA
    if (is.null(inits))
        inits <- "random"
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    n.clones <- dclone::nclones.list(data)

    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
        model_chr <- readLines(model)
    }
    if (identical(fit, NA)) {
        fit0 <- stan(#file=normalizePath(model),
            model_code=model_chr,
            data = data, pars = params,
            chains = n.chains, seed = seed,
            init = inits,
            fit = fit,
            ...)
    } else {
        if (is.character(fit))
            fit <- get(fit, pos = -1, inherits = TRUE)
        if (inherits(fit, "mcmc.list")) {
            sm <- stan.model(fit)
            if (is.null(sm))
                stop("no 'stan.model' attribute found in 'fit'")
        }
        if (inherits(fit, "stanmodel"))
            sm <- fit
        if (inherits(fit, "stanfit"))
            sm <- get_stanmodel(fit)

        fit0 <- sampling(sm,
            data = data, pars = params,
            chains = n.chains, seed = seed,
            init = inits,
            ...)
    }
    ## dc info stuff
    if (format == "mcmc.list") {
        res <- .rstan_as.mcmc.list.stanfit(fit0)
        ## get rid of  'lp__'
        res <- res[,which(varnames(res) != "lp__")]
        if (stan.model)
            attr(res, "stan.model") <- get_stanmodel(fit0)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
            class(res) <- c("mcmc.list.dc", class(res))
        }
    } else {
        res <- fit0
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
    }
    res
}

## internal function
.rstan_as.mcmc.list.stanfit <- function (object, pars, ...) {
    ## internal's internals
    rstan_num_pars <- function (d) prod(d)
    rstan_remove_empty_pars <- function (pars, model_dims) {
        ind <- rep(TRUE, length(pars))
        model_pars <- names(model_dims)
        if (is.null(model_pars))
            stop("model_dims need be a named list")
        for (i in seq_along(pars)) {
            p <- pars[i]
            m <- match(p, model_pars)
            if (!is.na(m) && prod(model_dims[[p]]) == 0)
                ind[i] <- FALSE
        }
        pars[ind]
    }
    rstan_calc_starts <- function (dims) {
        len <- length(dims)
        s <- sapply(unname(dims), function(d) rstan_num_pars(d), USE.NAMES = FALSE)
        cumsum(c(1, s))[1:len]
    }
    rstan_idx_col2rowm <- function (d) {
        len <- length(d)
        if (0 == len)
            return(1)
        if (1 == len)
            return(1:d)
        idx <- aperm(array(1:prod(d), dim = d))
        return(as.vector(idx))
    }
    rstan_pars_total_indexes <- function (names, dims, fnames, pars) {
        starts <- rstan_calc_starts(dims)
        par_total_indexes <- function(par) {
            p <- match(par, fnames)
            if (!is.na(p)) {
                names(p) <- par
                attr(p, "row_major_idx") <- p
                return(p)
            }
            p <- match(par, names)
            np <- rstan_num_pars(dims[[p]])
            if (np == 0)
                return(NULL)
            idx <- starts[p] + seq(0, by = 1, length.out = np)
            names(idx) <- fnames[idx]
            attr(idx, "row_major_idx") <- starts[p] +
                rstan_idx_col2rowm(dims[[p]]) - 1
            idx
        }
        idx <- lapply(pars, FUN = par_total_indexes)
        nulls <- sapply(idx, is.null)
        idx <- idx[!nulls]
        names(idx) <- pars[!nulls]
        idx
    }
    ## original def
    pars <- if (missing(pars))
        object@sim$pars_oi
    else check_pars_second(object@sim, pars)
    pars <- rstan_remove_empty_pars(pars, object@sim$dims_oi)
    tidx <- rstan_pars_total_indexes(object@sim$pars_oi, object@sim$dims_oi,
        object@sim$fnames_oi, pars)
    tidx <- lapply(tidx, function(x) attr(x, "row_major_idx"))
    tidx <- unlist(tidx, use.names = FALSE)
    lst <- vector("list", object@sim$chains)
    for (ic in 1:object@sim$chains) {
        x <- do.call(cbind, object@sim$samples[[ic]])[, tidx,
            drop = FALSE]
        warmup2 <- object@sim$warmup2[ic]
        if (warmup2 > 0)
            x <- x[-(1:warmup2), ]
        x <- as.matrix(x)
        end <- object@sim$iter
        thin <- object@sim$thin
        start <- end - (nrow(x) - 1) * thin
        class(x) <- "mcmc"
        attr(x, "mcpar") <- c(start, end, thin)
        lst[[ic]] <- x
    }
    class(lst) <- "mcmc.list"
    return(lst)
}
