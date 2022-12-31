## stan.fit for dclone


#' Fit Stan models with cloned data
#' 
#' Convenient functions designed to work well with cloned data arguments and
#' Stan.
#' 
#' 
#' @aliases stan.fit stan.parfit stan.model
#' @param data A list (or environment) containing the data.
#' @param params Character vector of parameters to be sampled.
#' @param model Character string (name of the model file), a function
#' containing the model, or a \code{\link[dclone]{custommodel}} object.
#' @param inits Optional specification of initial values in the form of a list
#' or a function. If \code{NULL}, initial values will be generated
#' automatically.
#' @param seed Random seed.
#' @param n.chains number of Markov chains.
#' @param format Desired output format.
#' @param stan.model Logical, if \code{stanmodel} object should be returned.
#' @param fit Fitted Stan object.
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}},
#' or an integer, see \code{\link{parDosa}} and
#' \code{\link{evalParallelArgument}}.
#' @param object A fitted MCMC object ('mcmc.list' class for example), with
#' \code{"stan.model"} attribute.
#' @param \dots Further arguments.
#' @return By default, an \code{stan.fit} returns an \code{mcmc.list} object.
#' If data cloning is used via the \code{data} argument, \code{summary} returns
#' a modified summary containing scaled data cloning standard errors (scaled by
#' \code{sqrt(n.clones)}), and \eqn{R_{hat}} values (as returned by
#' \code{\link[coda]{gelman.diag}}).
#' 
#' \code{stan.model} returns the \code{stanmodel} object.
#' 
#' \code{stan.parfit} runs chains using multiple cores when \code{cl} is an
#' integer. Using a cluster object leads to recompiling the model (therefore
#' \code{fit} is ignored), and might not be very quick to run.
#' @author Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Underlying functions: \code{\link[rstan]{stan}} and
#' \code{\link[rstan]{stanfit}} in package \pkg{rstan}
#' 
#' Methods: \code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}},
#' \code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}},
#' \code{\link{vcov.mcmc.list.dc}}
#' @keywords models htest
#' @examples
#' 
#' \dontrun{
#' if (require(rstan)) {
#'     model <- custommodel("data {
#'           int<lower=0> N;
#'           vector[N] y;
#'           vector[N] x;
#'         }
#'         parameters {
#'           real alpha;
#'           real beta;
#'           real<lower=0> sigma;
#'         }
#'         model {
#'           alpha ~ normal(0,10);
#'           beta ~ normal(0,10);
#'           sigma ~ cauchy(0,5);
#'           for (n in 1:N)
#'             y[n] ~ normal(alpha + beta * x[n], sigma);
#'         }")
#'     N <- 100
#'     alpha <- 1
#'     beta <- -1
#'     sigma <- 0.5
#'     x <- runif(N)
#'     y <- rnorm(N, alpha + beta * x, sigma)
#'     dat <- list(N=N, y=y, x=x)
#'     params <- c("alpha", "beta", "sigma")
#'     ## compile on 1st time only
#'     fit0 <- stan.fit(dat, params, model)
#'     ## reuse compiled fit0
#'     fit <- stan.fit(dat, params, model, fit=fit0)
#'     sm <- stan.model(fit)
#'     summary(fit)
#'     sm
#' 
#'     ## data cloning
#'     dcdat <- dclone(dat, n.clones=2, multiply="N")
#'     dcfit <- stan.fit(dcdat, params, model, fit=fit0)
#'     summary(dcfit)
#'     nclones(dcfit)
#' 
#'     ## using parallel options
#'     cl <- makeCluster(2)
#'     ## cannot utilize compiled fit0
#'     fit2 <- stan.parfit(cl=cl, dat, params, model)
#'     stopCluster(cl)
#'     if (.Platform$OS.type != "windows") {
#'         ## utilize compiled fit0
#'         fit3 <- stan.parfit(cl=2, dat, params, model, fit=fit0)
#'     }
#' }
#' }
#' 
#' @export stan.fit
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
