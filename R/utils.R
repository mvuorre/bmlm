#' Display results of multilevel mediation model
#'
#' Shows the relevant output of the Stan object obtained from \code{bmlm::mlm()}.
#'
#' @param mod A \code{stanfit} object obtained from \code{bmlm::mlm()}
#' @param level "Confidence" level; quantiles to summarize posterior distributions. Defaults to 0.91.
#' @param ref_val Obtain posterior probabilities that parameters are in the observed direction from \code{ref_val}. Defaults to 0.
#' @param pars Parameters to summarize. Defaults to main average-level parameters. See Details for more information.
#' @param digits How many decimal points to display in the output. Defaults to 2.
#'
#' @return A \code{data.frame} summarizing the estimated multilevel
#' mediation model:
#' \describe{
#'  \item{Parameter}{Name of parameter}
#'  \item{Mean}{Mean of parameter's posterior distribution.}
#'  \item{SD}{Standard deviation of parameter's posterior distribution.}
#'  \item{CI_\%}{The lower and upper limits of Credible Intervals.}
#'  \item{pprob}{Posterior probability.}
#'  \item{n_eff}{Number of efficient samples.}
#'  \item{Rhat}{Should be 1.00.}
#'}
#'
#' @details After estimating a model (drawing samples from the join posterior probability distribution) with mlm(), show the estimated results by using mlm_summary(fit), where "fit" is an object containing the fitted model.
#'
#' The function shows, for each parameter specified in \code{pars},
#' the posterior mean, and limits of the Credible Interval as specified
#'  in \code{level}. For example, \code{level = .91} shows a
#'  91% Credible Interval, which summarizes the central mass of
#'  the marginal posterior distribution.
#'
#' \subsection{Parameters}{
#' By default, \code{mlm()} estimates and returns a large number of parameters, including the varying effects, and their associated standard deviations. However, \code{mlm_summay()} only displays the most relevant subset of the estimated parameters:
#'
#' \describe{
#'  \item{a}{Regression slope of the X -> M relationship.}
#'  \item{b}{Regression slope of the M -> Y relationship.}
#'  \item{cp}{Regression slope of the X -> Y relationship.
#'  (Unmediated part of X to Y relation.)}
#'  \item{corrab}{Estimated correlation of the
#'  participant-level a_j and b_j parameters.}
#'  \item{ab}{Mediated effect (\code{a * b}).}
#'  \item{c}{Total effect of X on Y. ( \eqn{cp + ab + \sigma_ab} )}
#'  \item{pme}{Percent mediated effect.}
#'}
#'}
#'
#' To show all parameters, use \code{pars = NULL}. To learn more about the additional parameters, refer to the Stan code (\code{print_model(fit)}).
#'
#'
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @export
mlm_summary <- function(mod = NULL,
                        level = .91,
                        ref_val = 0,
                        pars = c("a", "b", "cp", "corrab", "ab", "c", "pme"),
                        digits = 2){

    # Check that mod is a Stanfit object
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")
    if (is.null(pars)) pars <- mod@sim$pars_oi  # Return all parameters

    # Obtain model summary from Stanfit
    lower_ci <- .5 - (level/2)
    upper_ci <- .5 + (level/2)
    mod_sum <- rstan::summary(object = mod,
                              probs = c(lower_ci, upper_ci),
                              pars = pars)$summary[,-2]

    # Clean output to a data.frame with no row names
    mod_sum <- rstan::summary(object = mod,
                              probs = c(lower_ci, upper_ci),
                              pars = pars)$summary[,-2]
    descriptors <- as.data.frame(round(mod_sum, digits = digits))[,1:4]
    diagnostics <- as.data.frame(round(mod_sum, digits = digits))[,5:6]
    mod_sum <- cbind(parameter = as.character(row.names(mod_sum)),
                     descriptors,
                     pprob = sapply(as.data.frame(mod, pars = pars), getpps),
                     diagnostics)
    row.names(mod_sum) <- NULL
    names(mod_sum) <- c("Parameter", "Mean", "SD",
                        paste0("CI_", lower_ci*100),
                        paste0("CI_", upper_ci*100),
                        "pprob", "n_eff", "Rhat")

    mod_sum

    return(mod_sum)
}

#' Get posterior probability
#'
#' Calculates the proportion of samples on the dominant side of a reference value.
#'
#' @param x A vector of MCMC samples.
#'
#' @return Posterior probability.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @export
getpps <- function(x){
    # Posterior probability density in observed direction from zero
    if (sign(sum(x)) == 1) xpprob <- sum(x > 0) / length(x)
    else if (sign(sum(x)) == -1) xpprob <- sum(x < 0) / length(x)
    # Print a warning if parameter is zero
    else {
        xpprob <- 0
        print("Warning calculating post. probs. (Par = 0?)")
    }
    return(xpprob)
}

#' Create within-person deviations and between-person means.
#'
#' Creates variables that represent pure within- and between-person predictors.
#'
#' @param d A \code{data.frame}.
#' @param by A vector of values by which the data is clustered.
#' i.e. a vector of unique participant IDs.
#' @param value A vector of values to transform.
#' @param z Should the new values be standardized (defaults to FALSE).
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
#' \dontrun{
#' # Create within-person deviations of \code{x}.
#' mydata <- isolate(mydata, by = "id", value = "x")  # Overwrites mydata
#'}
#'
#' @export
isolate <- function(d = NULL, by = NULL, value = NULL,
                    z = FALSE, which = "within"){
    oldnames <- names(d)
    d$c <- as.numeric(scale(d[,value], scale = z))  # Mean centered (or Zd)
    d <- within(d, {cb = stats::ave(c, d[,by], FUN = mean)})  # Between-person
    d$cw <- d$c - d$cb  # Within-person
    names(d) <- c(oldnames,
                  paste0(value, "_c"),
                  paste0(value, "_cb"),
                  paste0(value, "_cw"))
    N <- dim(d)[2]
    if (which == "within") d <- d[,-c(N-1, N-2)]
    else if (which == "between") d <- d[,-c(N, N-2)]
    else d <- d[,-(N-2)]
    return(d)
}
