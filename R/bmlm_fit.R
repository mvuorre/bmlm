#' Fits a multilevel mediation model with Stan.
#'
#' Multilevel mediation with Stan.
#'
#' @param d A \code{data.frame}.
#' @param id Column of participant IDs in \code{data}.
#' @param x Column of X values in \code{data}.
#' @param m Column of M values in \code{data}.
#' @param y Column of Y values in \code{data}.
#' @param ... Other optional parameters passed to \code{rstan::stan()}.
#'
#' @return An object of S4 class stanfit, with all its available methods.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Draw samples from a multilevel mediation model using Stan.
#'
#' @examples
#' \dontrun{
#' ## Run example from Bolger and Laurenceau (2013)
#' data(BLch9)
#' fit <- bmlm_fit(BLch9)
#' summary(fit)}
#'
#' @import rstan
#' @export

bmlm_fit <- function(d = NULL, id = NULL, x = x, m = m, y = y, ...) {

    # Create a data list for Stan
    standat <- with(d, list(
        N = nrow(d),
        X = x,
        M = m,
        Y = y,
        id = id,
        J = length(unique(id))
    ))

    # Sample from model
    model_file <- system.file("stan/bmlm.stan", package="bmlm")
    fit <- rstan::stan(file = model_file, data = standat, ...)
}
