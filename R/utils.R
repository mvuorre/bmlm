#' Print a summary of the estimated multilevel mediation model
#'
#' Prints the estimated parameters (numerical summaries of the marginal
#' posterior distributions).
#'
#' @param mod A \code{stanfit} object obtained from \code{mlm()}
#' @param level "Confidence" level; Defines the limits of the credible intervals.
#' Defaults to .99 (i.e. displays 99\% CIs.)
#' @param ref_val Obtain posterior probabilities that parameters are in the
#' observed direction from \code{ref_val}. Defaults to 0.
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
#'  \item{SD}{Standard deviation of parameter's posterior distribution.}
#'  \item{ci_lwr}{The lower limit of Credible Intervals.}
#'  \item{ci_upr}{The upper limit of Credible Intervals.}
#'  \item{pprob}{Posterior probability.}
#'  \item{n_eff}{Number of efficient samples.}
#'  \item{Rhat}{Should be 1.00.}
#'}
#'
#' @details After estimating a model (drawing samples from the joint posterior
#' probability distribution) with \code{mlm()}, show the estimated results
#' by using \code{mlm_summary(fit)}, where "fit" is an object containing
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
#'  \item{cp}{Regression slope of the X -> Y relationship.
#'  (The direct effect.)}
#'  \item{ab}{Mediated effect (\code{a * b}).}
#'  \item{c}{Total effect of X on Y. ( \eqn{cp + ab + \sigma_ab} )}
#'  \item{pme}{Percent mediated effect.}
#'  \item{covab}{Estimated covariance of the
#'  participant-level a_j and b_j parameters.}
#'  \item{corrab}{Estimated correlation of the
#'  participant-level a_j and b_j parameters.}
#'}
#' The user may specify \code{pars = NULL} to display all estimated parameters.
#' Other options include e.g. \code{pars = "tau"} to display the varying
#' effects' standard deviations.
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
    level = .99,
    ref_val = 0,
    pars = c("a", "b", "cp", "ab", "c", "pme", "covab", "corrab"),
    digits = 2
    ){

    # Check that mod is a Stanfit object
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    # Choose which parameters to display
    if (is.null(pars)) pars <- mod@sim$pars_oi  # Return all parameters

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
    mod_sum$pprob <- apply(as.data.frame(mod, pars = pars), 2,
                           FUN = getpps, ref_val = ref_val)
    mod_sum$n_eff <- floor(mod_sum$n_eff)
    mod_sum$Parameter <- Names
    mod_sum <- mod_sum[,c(9,1,2,4,3,5,8,6,7)]
    names(mod_sum) <- c("Parameter", "Mean", "SD",
                        "Median", "ci_lwr", "ci_upr",
                        "pprob", "n_eff", "Rhat")
    row.names(mod_sum) <- NULL
    return(mod_sum)
}

#' Get posterior probability
#'
#' Calculates the proportion of samples on the dominant side of a reference value.
#'
#' @param x A vector of MCMC samples.
#' @param ref_val Reference value, i.e. if samples are greater/smaller than
#' this. (Defaults to 0.)
#'
#' @return Posterior probability.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @examples
#' # Probability that standard normal deviates are greater than 1
#' getpps(rnorm(1000), ref_val = 1)
#'
#' @export
getpps <- function(x, ref_val = 0){
    # Posterior probability density in observed direction from zero
    if (sign(sum(x)) == 1) xpprob <- sum(x > ref_val) / length(x)
    else if (sign(sum(x)) == -1) xpprob <- sum(x < ref_val) / length(x)
    # Print a warning if parameter is zero
    else {
        xpprob <- 0
        print("Warning calculating post. probs. (Par = 0?)")
    }
    return(xpprob)
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
        d <- within(d, {cb = stats::ave(c, d[,by], FUN = mean)})  # Between-person
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

#' Create a word document from a summary table.
#'
#' Saves a Word document to the current working directory.
#'
#' @param d A \code{data.frame}.
#' @param name Name of file to create. Defaults to "Table.docx".
#'
#' @details Requires the ReporteRs R package. Copy-pasting individual values
#' is error-prone. Use \code{tab2doc()} to create a word document containing
#' a summary table.
#'
#' @return Saves a word document in the current working directory.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @examples
#' \dontrun{
#' tab2doc(mlm_summary(fit), name = "Fit_summary")
#'}
#'
#' @export
tab2doc <- function(d = NULL, name = NULL){

    if (!requireNamespace("ReporteRs", quietly = TRUE)) {
        stop("ReporteRs package needed for this function. Please install it.",
             call. = FALSE)
    }

    if (is.null(d)) stop("Please provide a table.", call. = FALSE)
    if (is.null(name)) name <- "Table"
    name <- paste0(name, ".docx")

    if (file.exists(name)) {
        message("Overwriting previous file")
        file.remove(name)
    }
    doc <- ReporteRs::docx()
    tab <- ReporteRs::FlexTable(data = d)
    doc <- ReporteRs::addFlexTable(doc, tab)
    ReporteRs::writeDoc(doc, name)
}
