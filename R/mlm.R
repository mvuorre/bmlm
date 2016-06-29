#' Bayesian parameter estimation for multilevel mediation models
#'
#' Easy Bayesian parameter estimation for multilevel mediation models.
#'
#' @param d A \code{data.frame}.
#' @param id Column of participant IDs in \code{data}.
#' @param x Column of X values in \code{data}.
#' @param m Column of M values in \code{data}.
#' @param y Column of Y values in \code{data}.
#' @param prior_scale Prior standard deviation on regression coefficients.
#' See details.
#' @param intrcpt_scale Prior standard deviation on regression intercepts.
#' See details
#' @param no_priors Estimates the model with no priors. Not recommended,
#' see details.
#' @param ... Other optional parameters passed to \code{rstan::stan()}.
#'
#' @return An object of S4 class stanfit, with all its available methods.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Draw samples from the joint posterior distribution of a
#' multilevel mediation model using Stan.
#'
#' \code{mlm()} Allows the user to input standard deviation parameters for
#' the regression coefficients and intercepts. If no priors are provided,
#' these are set to 100 (unless no_priors = TRUE).
#'
#' If the user sets no_priors = TRUE, all parameters are estimated with
#' uniform priors. This is generally not recommended, but in some cases
#' the default priors might not be suitable to the data. \code{mlm()} uses
#' weakly regularizing priors by default, which help avoid overfitting.
#'
#' Currently, we assume that the user inputs X, M, and Y as within-person
#' deviated variables (Bolger & Laurenceau, 2013, ch.9). As a result, the
#' regression intercepts will be zero.
#'
#' @examples
#' \dontrun{
#' ## Run example from Bolger and Laurenceau (2013)
#' data(BLch9)
#' fit <- mlm(BLch9)
#' mlm_summary(fit)
#' }
#'
#' @import rstan
#' @export

mlm <- function(d = NULL, id = id, x = x, m = m, y = y,
                prior_scale = NULL,
                intrcpt_scale = NULL,
                no_priors = FALSE,
                ...) {

    # Check for data and quit if not suitable
    if (is.null(d)) stop("No data entered")

    # Create a data list for Stan
    ld <- list()
    # Coerce to 1:J sequential
    ld$id = as.integer(as.factor(as.character(d[,id])))
    ld$x = d[,x]
    ld$m = d[,m]
    ld$y = d[,y]
    ld$J <- length(unique(ld$id))
    ld$N <- nrow(d)
    ld$prior_scale <- prior_scale
    ld$intrcpt_scale <- intrcpt_scale
    ld$y_mean <- mean(d[,y])

    # Check priors
    if (is.null(prior_scale)) prior_scale <- 100
    if (is.null(intrcpt_scale)) intrcpt_scale <- 100

    # Sample from model
    if (no_priors == FALSE) {
        model_file <- system.file("stan/bmlm.stan", package="bmlm")
    } else {
        model_file <- system.file("stan/bmlm_nopriors.stan", package="bmlm")
    }
    message("Estimating model, please wait.")
    fit <- rstan::stan(file = model_file,
                       model_name = "Multilevel mediation",
                       data = ld,
                       pars = c("U", "Sigma", "Omega"),
                       include = FALSE,
                       ...)

    # Return stanfit object
    return(fit)
}
