#' Display results of multilevel mediation model
#'
#' Shows the relevant output of the Stan object obtained from \code{bmlm::mlm()}.
#'
#' @param mod A \code{stanfit} object obtained from \code{bmlm::mlm()}
#' @param level "Confidence" level; quantiles to summarize posterior distributions. Defaults to 0.91.
#' @param ref_val Obtain posterior probabilities that parameters are in the observed direction from \code{ref_val}. Defaults to 0.
#' @param pars Parameters to summarize. Defaults to main average-level parameters. See Details for more information.
#' @param level1 Should participant-specific mediation parameters be displayed?
#' Defaults to FALSE.
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
#'  91\% Credible Interval, which summarizes the central 91\% mass of
#'  the marginal posterior distribution.
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
#' Other options include \code{pars = "tau"} to display the varying
#' effects standard deviations. By specifying \code{level1 = TRUE}, the
#' output includes estimated mediation parameters for all participants.
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
    level = .91,
    ref_val = 0,
    pars = c("a", "b", "cp", "ab", "c", "pme", "covab", "corrab"),
    level1 = FALSE,
    digits = 2
    ){

    # Check that mod is a Stanfit object
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    # Choose which parameters to display
    if (is.null(pars)) pars <- mod@sim$pars_oi  # Return all parameters
    if (level1) pars <- c(pars, "u_ab", "u_cp", "u_c", "u_pme")

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
    mod_sum$pprob <- apply(as.data.frame(mod, pars = pars), 2, FUN = getpps)
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
#' @param value Names of columns to isolate. Multiple values can be given by
#' \code{value = c("var1", "var2", "var3")}
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
#' Copy-pasting individual values is error-prone. Use \code{tab2doc()} to
#' create a word document containing a summary table.
#'
#' @param d A \code{data.frame}.
#' @param name Name of file to create. Defaults to "Table.docx".
#'
#' @details Requires the ReporteRs R package.
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
             call. = TRUE)
    }

    if (is.null(d)) stop("Please provide a table.")
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
