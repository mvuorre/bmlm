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
#' the regression coefficients. If no priors are provided,
#' these are set to 100 (unless no_priors = TRUE).
#' \code{mlm()} uses weakly regularizing priors by default,
#' which help avoid overfitting.
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

mlm <- function(d = NULL, id = "id", x = "x", m = "m", y = "y",
                prior_scale = NULL,
                intrcpt_scale = NULL,
                ...) {

    # Check for data and quit if not suitable
    if (is.null(d)) stop("No data entered")

    # Check priors
    if (is.null(prior_scale)) prior_scale <- 100
    if (is.null(intrcpt_scale)) intrcpt_scale <- .001

    # Create a data list for Stan
    ld <- list()
    # Coerce to 1:J sequential
    ld$id = as.integer(as.factor(as.character(d[,id])))
    ld$X = d[,x]
    ld$M = d[,m]
    ld$Y = d[,y]
    ld$J <- length(unique(ld$id))
    ld$N <- nrow(d)
    ld$prior_scale <- prior_scale
    ld$intrcpt_scale <- intrcpt_scale


    # Sample from model
    model_file <- system.file("stan/bmlm.stan", package="bmlm")
    message("Estimating model, please wait.")
    fit <- rstan::stan(file = model_file,
                       model_name = "Multilevel mediation",
                       data = ld,
                       pars = c("U", "z_U", "L_Omega"),
                       include = FALSE,
                       ...)

    # Return stanfit object
    return(fit)
}
