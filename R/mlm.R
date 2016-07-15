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
#' @param tau_scale Prior scale on \code{tau} parameters. See details.
#' @param intrcpt_scale Prior standard deviation on intercepts.
#' @param binary Which response variables should be modelled as binary with
#' logistic regression. Defaults to NULL (M and Y treated as continuous.)
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
#' \code{prior_scale} Allows the user to input a standard deviation parameter
#' to the prior distributions for the slope parameters in a, b, and cp
#' paths. The default is 100, which is nearly uninformative for standardized
#' data, but users are recommended to adjust this to fit the scale of the data.
#' \code{inrtcpt_scale} The same as above, but for the regression intercepts.
#' Defaults to 10.
#' \code{tau_scale} Allows the user to input a scale parameter to Cauchy
#' distributions on the varying effects' standard deviation parameters. The
#' default is 10, which is very weakly regularizing for standardized variables.
#'
#' }
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
                tau_scale = NULL,
                intrcpt_scale = NULL,
                binary = NULL,
                ...) {

    # Check for data
    if (is.null(d)) stop("No data entered")
    if (class(d)[1] == "tbl_df") d <- as.data.frame(d)  # Allow tibbles

    # Check priors
    if (is.null(prior_scale)) prior_scale <- 100
    if (is.null(tau_scale)) tau_scale <- 10
    if (is.null(intrcpt_scale)) intrcpt_scale <- 10

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
    ld$tau_scale <- tau_scale
    ld$intrcpt_scale <- intrcpt_scale

    # Choose model
    if (is.null(binary)) {
        model_file <- system.file("stan/bmlm.stan", package="bmlm")
    } else if (binary == "y" || binary == y) {
        model_file <- system.file("stan/bmlm_binary_y.stan", package="bmlm")
    } else { stop("Unsupported variables defined as binary.") }

    # Sample from model
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
