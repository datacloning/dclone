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
    } else {
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
        res <- As.mcmc.list(fit0)
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
