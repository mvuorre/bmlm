#' Estimate a multilevel mediation model
#'
#' Estimates a Bayesian multilevel mediation model using Stan.
#'
#' @param d A \code{data.frame} or a \code{data_frame}.
#' @param id Column of participant IDs in \code{data}.
#' @param x Column of X values in \code{data}.
#' @param m Column of M values in \code{data}.
#' @param y Column of Y values in \code{data}.
#' @param slope_scale Prior standard deviation on regression slope coefficients.
#' See details.
#' @param tau_scale Prior scale on varying effects' SDs. See details.
#' @param intercept_scale Prior SD on regression intercept coefficients.
#' @param binary_y Set to TRUE if y is binary and should be modelled
#' with logistic regression. Defaults to FALSE (y treated as continuous.)
#' @param ... Other optional parameters passed to \code{rstan::stan()}.
#'
#' @return An object of S4 class stanfit, with all its available methods.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Draw samples from the joint posterior distribution of a
#' multilevel mediation model using Stan.
#'
#' \subsection{Priors}{
#'
#' \code{slope_scale} Allows the user to input a standard deviation parameter
#' to the prior distributions for the slope parameters in a, b, and cp
#' paths. The default is 100, which is only very weakly informative for standardized
#' data, but users are recommended to adjust this to fit the scale of the data.
#' \code{intercept_scale} The same as above, but for the regression intercepts.
#' Defaults to 10.
#' \code{tau_scale} Allows the user to input a scale parameter to Cauchy
#' distributions on the varying effects' standard deviation parameters. The
#' default is 10, which is very weakly regularizing for standardized variables.
#'
#' }
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
                slope_scale = NULL,
                tau_scale = NULL,
                intercept_scale = NULL,
                binary_y = FALSE,
                ...) {

    # Check for data
    if (is.null(d)) stop("No data entered")
    if (class(d)[1] == "tbl_df") d <- as.data.frame(d)  # Allow tibbles

    # Check priors
    if (is.null(slope_scale)) slope_scale <- 100
    if (is.null(tau_scale)) tau_scale <- 10
    if (is.null(intercept_scale)) intercept_scale <- 10

    # Create a data list for Stan
    ld <- list()
    # Coerce IDs to 1:J sequential
    ld$id = as.integer(as.factor(as.character(d[,id])))
    ld$X = d[,x]
    ld$M = d[,m]
    ld$Y = d[,y]
    ld$J <- length(unique(ld$id))
    ld$N <- nrow(d)
    ld$slope_scale <- slope_scale
    ld$tau_scale <- tau_scale
    ld$intercept_scale <- intercept_scale

    # Choose model
    if (binary_y) {
        model_file <- system.file("stan/bmlm_binary_y.stan", package="bmlm")
    } else {
        model_file <- system.file("stan/bmlm.stan", package="bmlm")
    }

    # Sample from model
    message("Estimating model, please wait.")
    fit <- rstan::stan(file = model_file,
                       model_name = "Multilevel mediation",
                       data = ld,
                       pars = c("U", "z_U", "L_Omega"),
                       include = FALSE,
                       ...)

    return(fit)
}
