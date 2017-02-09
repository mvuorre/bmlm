#' Estimate a multilevel mediation model
#'
#' Estimates a Bayesian multilevel mediation model using Stan.
#'
#' @param d A \code{data.frame} or a \code{data_frame}.
#' @param id Column of participant IDs in \code{data}.
#' @param x Column of X values in \code{data}.
#' @param m Column of M values in \code{data}.
#' @param y Column of Y values in \code{data}.
#' @param priors A list of named values to be used as the prior scale
#' parameters. See details.
#' @param binary_y Set to TRUE if y is binary and should be modelled
#' with logistic regression. Defaults to FALSE (y treated as continuous.)
#' This feature is experimental.
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
#' Users may pass a list of named values for the \code{priors} argument.
#' The values will be used to define the scale parameter of the
#' respective prior distributions.
#' This list may specify some or all of the following parameters:
#'
#' \describe{
#'  \item{dy, dm}{Regression intercepts (for Y and M as outcomes, respectively.)}
#'  \item{a, b, cp}{Regression slopes.}
#'  \item{tau_x}{Varying effects SDs for above parameters (e.g replace x with a.)}
#'  \item{lkj_shape}{Shape parameter for the LKJ prior.}
#' }
#' See examples for specifying the following: Gaussian distributions with SD = 10
#' as priors for the intercepts, Gaussians with SD = 2 for the slopes,
#' Half-Cauchy distributions with scale parameters 1 for the varying effects
#' SDs, and an LKJ prior of 2.
#' }
#'
#' @examples
#' \dontrun{
#' ## Run example from Bolger and Laurenceau (2013)
#' data(BLch9)
#' fit <- mlm(BLch9)
#' mlm_summary(fit)
#'
#' ### With priors
#' Priors <- list(dy = 10, dm = 10, a = 2, b = 2, cp = 2,
#'                tau_dy = 1, tau_dm = 1, tau_a = 1, tau_b = 1, tau_cp = 1,
#'                lkj_shape = 2)
#' fit <- mlm(BLch9, priors = Priors)
#' }
#'
#' @import rstan
#' @export
mlm <- function(d = NULL, id = "id", x = "x", m = "m", y = "y",
                priors = NULL,
                binary_y = FALSE,
                ...) {

    # Check for data
    if (is.null(d)) stop("No data entered")
    if (class(d)[1] == "tbl_df") d <- as.data.frame(d)  # Allow tibbles

    # Check priors
    default_priors <- list(
        dm = 1000, tau_dm = 50,
        dy = 1000, tau_dy = 50,
        a = 1000, tau_a = 50,
        b = 1000, tau_b = 50,
        cp = 1000, tau_cp = 50,
        lkj_shape = 1
    )
    if (is.null(priors$dm)) priors$dm <- default_priors$dm
    if (is.null(priors$dy)) priors$dy <- default_priors$dy
    if (is.null(priors$a)) priors$a <- default_priors$a
    if (is.null(priors$b)) priors$b <- default_priors$b
    if (is.null(priors$cp)) priors$cp <- default_priors$cp
    if (is.null(priors$tau_dm)) priors$tau_dm <- default_priors$tau_dm
    if (is.null(priors$tau_dy)) priors$tau_dy <- default_priors$tau_dy
    if (is.null(priors$tau_a)) priors$tau_a <- default_priors$tau_a
    if (is.null(priors$tau_b)) priors$tau_b <- default_priors$tau_b
    if (is.null(priors$tau_cp)) priors$tau_cp <- default_priors$tau_cp
    if (is.null(priors$lkj_shape)) priors$lkj_shape <- default_priors$lkj_shape
    names(priors) <- lapply(names(priors), function(x) paste0("prior_", x))

    # Create a data list for Stan
    ld <- list()
    ld$id = as.integer(as.factor(as.character(d[,id])))  # Sequential IDs
    ld$X = d[,x]
    ld$M = d[,m]
    ld$Y = d[,y]
    ld$J <- length(unique(ld$id))
    ld$N <- nrow(d)
    ld <- append(ld, priors)

    # Choose model
    if (binary_y) {
        # model_file <- system.file("stan/bmlm_binary_y.stan", package="bmlm")
        model_s <- stanmodels$bmlm_binary_y
    } else {
        # model_file <- system.file("stan/bmlm.stan", package="bmlm")
        model_s <- stanmodels$bmlm
    }

    # Sample from model
    message("Estimating model, please wait.")
    fit <- rstan::sampling(
        object = model_s,
        data = ld,
        pars = c("U", "z_U", "L_Omega", "Tau", "Sigma"),
        include = FALSE,
        ...)

    return(fit)
}
