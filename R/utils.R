#' Print a summary of the estimated multilevel mediation model
#'
#' Prints the estimated parameters (numerical summaries of the marginal
#' posterior distributions).
#'
#' @param mod A \code{stanfit} object obtained from \code{mlm()}
#' @param level "Confidence" level; Defines the limits of the credible intervals.
#' Defaults to .95 (i.e. displays 95\% CIs.)
#' @param pars Parameters to summarize. Defaults to main average-level
#' parameters. See Details for more information.
#' @param digits How many decimal points to display in the output. Defaults to 2.
#'
#' @return A \code{data.frame} summarizing the estimated multilevel
#' mediation model:
#' \describe{
#'  \item{Parameter}{Name of parameter}
#'  \item{Mean}{Mean of parameter's posterior distribution.}
#'  \item{Median}{Median of parameter's posterior distribution.}
#'  \item{SE}{Standard deviation of parameter's posterior distribution.}
#'  \item{ci_lwr}{The lower limit of Credible Intervals.}
#'  \item{ci_upr}{The upper limit of Credible Intervals.}
#'  \item{n_eff}{Number of efficient samples.}
#'  \item{Rhat}{Should be 1.00.}
#'}
#'
#' @details After estimating a model (drawing samples from the joint posterior
#' probability distribution) with \code{mlm()}, show the estimated results
#' by using \code{mlm_summary(fit)}, where \code{fit} is an object containing
#' the fitted model.
#'
#' The function shows, for each parameter specified with \code{pars},
#' the posterior mean, and limits of the Credible Interval as specified
#' by \code{level}. For example, \code{level = .91} shows a
#' 91\% Credible Interval, which summarizes the central 91\% mass of
#' the marginal posterior distribution.
#'
#' \subsection{Parameters}{
#' By default, \code{mlm()} estimates and returns a large number of parameters,
#' including the varying effects, and their associated standard deviations.
#' However, \code{mlm_summay()} by default only displays a subset of the
#' estimated parameters:
#'
#' \describe{
#'  \item{a}{Regression slope of the X -> M relationship.}
#'  \item{b}{Regression slope of the M -> Y relationship.}
#'  \item{cp}{Regression slope of the X -> Y relationship. (Direct effect.)}
#'  \item{me}{Mediated effect (\eqn{a * b + \sigma_{{a_j}{b_j}}}).}
#'  \item{c}{Total effect of X on Y. ( \eqn{cp + me} )}
#'  \item{pme}{Percent mediated effect.}
#'}
#' The user may specify \code{pars = NULL} to display all estimated parameters.
#' Other options include e.g. \code{pars = "tau"} to display the varying
#' effects' standard deviations. To display all the group-level parameters
#' (also known as random effects) only, specify \code{pars = "random"}.
#' With this argument, \code{mlm_summary()} prints the following parameters:
#'
#' \describe{
#'  \item{tau_a}{Standard deviation of subject-level \code{a_j}s.}
#'  \item{tau_b}{Standard deviation of subject-level \code{b_j}s.}
#'  \item{tau_cp}{Standard deviation of subject-level \code{c\'_j}s.}
#'  \item{covab}{Estimated covariance of \code{a_j} and \code{b_j}s.}
#'  \item{corrab}{Estimated correlation of \code{a_j} and \code{b_j}s.}
#'}
#'
#' To learn more about the additional parameters, refer to the Stan code
#' (\code{cat(get_stancode(fit))}).
#'}
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @export
mlm_summary <- function(
    mod = NULL,
    level = .95,
    pars = c("a", "b", "cp", "me", "c", "pme"),
    digits = 2
    ){

    # Check that mod is a Stanfit object
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    # Choose which parameters to display
    if (is.null(pars)) pars <- mod@sim$pars_oi  # Return all parameters
    if (any(pars == "random")) pars <- c(
        "tau_a", "tau_b", "tau_cp", "covab", "corrab"
    )

    # Obtain model summary from Stanfit
    lower_ci <- .5 - (level/2)
    upper_ci <- .5 + (level/2)

    mod_sum <- rstan::summary(object = mod,
                              probs = c(lower_ci, .5, upper_ci),
                              pars = pars)$summary[,-2]
    # Clean and get post. probs
    if (is.null(dim(mod_sum))) {  # If only one param entered
        mod_sum <- data.frame(t(mod_sum))
        mod_sum <- data.frame(t(apply(mod_sum, 2, round, digits = digits)))
        Names <- pars
    } else {
        mod_sum <- data.frame(mod_sum)
        mod_sum <- data.frame(apply(mod_sum, 2, round, digits = digits))
        Names <- row.names(mod_sum)
        }
    mod_sum$n_eff <- floor(mod_sum$n_eff)
    mod_sum$Parameter <- Names
    mod_sum <- mod_sum[,c(8,1,2,4,3,5,6,7)]
    names(mod_sum) <- c("Parameter", "Mean", "SE", "Median",
                        paste0(lower_ci*100, "%"), paste0(upper_ci*100, "%"),
                        "n_eff", "Rhat")
    row.names(mod_sum) <- NULL
    return(mod_sum)
}

#' Create isolated within- (and optionally between-) person variables.
#'
#' Creates variables that represent pure within- and between-person predictors.
#'
#' @param d A \code{data.frame}.
#' @param by A vector of values in \code{d} by which the data is clustered.
#' i.e. a vector of unique participant IDs.
#' @param value Names of columns in \code{d} to isolate. Multiple values can be
#' given by \code{value = c("var1", "var2", "var3")}
#' @param z Should the created values be standardized (defaults to FALSE).
#' @param which Which component to return. "within" (default) returns
#' within-person deviations only; "between" returns between-person means only;
#' "both" returns both.
#'
#' @return A \code{data.frame} with additional columns for the within- and
#' between-person variables. The new columns are labelled _cw for
#' centered-within and _cb for centered-between.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @examples
#' # Create within-person deviations of work stressors in BLch9.
#' data(BLch9)
#' BLch9 <- isolate(BLch9, by = "id", value = "fwkstrs")
#' head(BLch9)  # Now has new column for within-person work stressors.
#'
#' @export
isolate <- function(d = NULL, by = NULL, value = NULL,
                    z = FALSE, which = "within"){
    for (val in value){
        oldnames <- names(d)
        d$c <- as.numeric(scale(d[,val], scale = z))  # Mean centered (or Zd)
        d <- within(d,
                    {cb = stats::ave(
                        c, d[,by],
                        FUN = function(x) mean(x, na.rm = T))
                    })
        d$cw <- d$c - d$cb  # Within-person
        names(d) <- c(oldnames,
                      paste0(val, "_c"),
                      paste0(val, "_cb"),
                      paste0(val, "_cw"))
        N <- dim(d)[2]
        if (which == "within") d <- d[,-c(N-1, N-2)]
        else if (which == "between") d <- d[,-c(N, N-2)]
        else d <- d[,-(N-2)]
    }
    return(d)
}
