#' Data Cloning Diagnostics
#' 
#' These functions calculates diagnostics for evaluating data cloning
#' convergence.
#' 
#' These diagnostics can be used to test for the data cloning convergence (Lele
#' et al. 2007, 2010). Asymptotically the posterior distribution of the
#' parameters approaches a degenerate multivariate normal distribution. As the
#' distribution is getting more degenerate, the maximal eigenvalue
#' (\eqn{\lambda_{max}}) of the unscaled covariance matrix is decreasing. There
#' is no critical value under which \eqn{\lambda_{max}} is good enough. By
#' default, 0.05 is used (see \code{getOption("dclone")$diag}).
#' 
#' Another diagnostic tool is to check if the joint posterior distribution is
#' multivariate normal. It is done by \code{chisq.diag} as described by Lele et
#' al. (2010).
#' 
#' @aliases lambdamax.diag chisq.diag lambdamax.diag.mcmc.list
#' chisq.diag.mcmc.list
#' @param x An object of class \code{mcmc} or \code{mcmc.list}.
#' @param \dots Other arguments to be passed.
#' @return \code{lambdamax.diag} returns a single value, the maximum of the
#' eigenvalues of the unscaled variance covariance matrix of the estimated
#' parameters.
#' 
#' \code{chisq.diag} returns two test statistic values (mean squared error and
#' r-squared) with empirical and theoretical quantiles.
#' @author Khurram Nadeem, \email{knadeem@@math.ualberta.ca}
#' 
#' Peter Solymos, \email{solymos@@ualberta.ca}
#' @seealso Eigen decomposition: \code{\link{eigen}}
#' @references Lele, S.R., B. Dennis and F. Lutscher, 2007. Data cloning: easy
#' maximum likelihood estimation for complex ecological models using Bayesian
#' Markov chain Monte Carlo methods. \emph{Ecology Letters} \strong{10},
#' 551--563.
#' 
#' Lele, S. R., K. Nadeem and B. Schmuland, 2010. Estimability and likelihood
#' inference for generalized linear mixed models using data cloning.
#' \emph{Journal of the American Statistical Association} \strong{105},
#' 1617--1625.
#' 
#' Solymos, P., 2010. dclone: Data Cloning in R. \emph{The R Journal}
#' \strong{2(2)}, 29--37. URL:
#' \url{https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
#' @keywords htest
#' @examples
#' 
#' data(regmod)
#' lambdamax.diag(regmod)
#' chisq.diag(regmod)
#' 
#' @export lambdamax.diag
lambdamax.diag <-
function(x, ...)
{
    UseMethod("lambdamax.diag")
}
