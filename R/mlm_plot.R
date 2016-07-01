#' Plot multilevel mediation as a path diagram
#'
#' Plots a path diagram from a multilevel mediation model.
#'
#' @param mod A Stanfit model estimated with \code{bmlm::mlm()}.
#' @param xlab Label for X
#' @param ylab Label for Y
#' @param mlab Label for M
#' @param border.width Size of node borders (defaults to 2).
#' @param edge.label.cex Text size.
#' @param fade Should edges fade to white? (Defaults to FALSE.)
#' @param level "Confidence" level for credible intervals.
#' @param ... Other arguments passed on to \code{qgraph::qgraph()}.
#'
#' @return A qgraph object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Experimental. Plots a path diagram of the mediation model,
#' with estimated average parameter values and credible intervals.
#'
#' @export
mlm_path_plot <- function(mod = NULL, xlab = "X", ylab = "Y", mlab = "M",
                          border.width = 2,
                          edge.label.cex = 1.2,
                          fade = FALSE,
                          level = .91,
                          ...){

    # Requires the qgraph package
    if (!requireNamespace("qgraph", quietly = TRUE)) {
        stop("qgraph package needed for this function. Please install it.",
             call. = TRUE)
    }
    # Ensure suitable model object passed
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    # Get model summary
    sfit <- mlm_summary(mod, level = level)
    a <- round(as.numeric(sfit[1, c("Mean", "ci_lwr", "ci_upr")]), 2)
    b <- round(as.numeric(sfit[2, c("Mean", "ci_lwr", "ci_upr")]), 2)
    cp <- round(as.numeric(sfit[3, c("Mean", "ci_lwr", "ci_upr")]), 2)
    ab <- round(as.numeric(sfit[5, c("Mean", "ci_lwr", "ci_upr")]), 2)
    c <- round(as.numeric(sfit[6, c("Mean", "ci_lwr", "ci_upr")]), 2)
    pme <- round(as.numeric(sfit[7, c("Mean", "ci_lwr", "ci_upr")]), 2)

    # Specify plot layout and parameters
    edgelabels <- c(
        paste0("\n a = ", a[1], " \n [", a[2], ", ", a[3], "] \n"),
        paste0("\n b = ", b[1], " \n [", b[2], ", ", b[3], "] \n"),
        paste0("\n c' = ", cp[1], " \n [", cp[2], ", ", cp[3], "] \n")
    )
    x <- matrix(c(1, b[1], 0,
                  0, 1, 0,
                  a[1], cp[1] ,1), byrow=T, nrow = 3)

    # Create plot
    p2 <- qgraph::qgraph(x, layout = "circle",
                         shape = "square",
                         labels = c(mlab, ylab, xlab),
                         border.width = border.width,
                         edge.labels = edgelabels,
                         edge.label.cex = edge.label.cex,
                         fade = FALSE,
                         ...)
    graphics::text(-1.2, 1.1,
                   paste0("ab = ", ab[1], " [", ab[2], ", ", ab[3], "]"), pos=4)
    graphics::text(-1.2, 0.9,
                   paste0("c = ", c[1], " [", c[2], ", ", c[3], "]"), pos=4)
    graphics::text(-1.2, 0.7,
                   paste0("%me = ", pme[1], " [", pme[2], ", ", pme[3], "]"), pos=4)
}

#' Plot marginal posterior histograms or coefficients plots.
#'
#' Plot the model's estimated parameters as histograms or a coefficient plot.
#'
#' @param mod A Stanfit model estimated with \code{bmlm::mlm()}.
#' @param type Type of the plot, \code{hist} or \code{coefplot}.
#' @param level X level for Credible Intervals.
#' @param color Color for plots.
#' @param pars List of parameters to plot.
#' @param nrow Number of rows for multiple histograms.
#'
#' @return A ggplot2 object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details The point estimate for the coefficient plot is the posterior median.
#'
#'@import ggplot2
#'
#' @export
mlm_pars_plot <- function(mod = NULL,
                          type = "hist",
                          color = "black",
                          level = 0.91,
                          nrow = 3,
                          pars = c("a", "b", "cp", "corrab", "ab", "c", "pme")){

    # Requires the reshape2 package
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("reshape2 package needed for this function. Please install it.",
             call. = TRUE)
    }

    # Ensure suitable model object passed
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    d <- as.data.frame(mod, pars = pars)
    d <- reshape2::melt(d)

    # Note that aes calls must be aes_string to avoid build notes
    if (type == "hist"){
        p1 <- ggplot2::ggplot(d, aes_string(x="value")) +
            stat_bin(aes_string(y="..ndensity.."),
                     col="white", fill=color) +
            labs(x="", y="") +
            facet_wrap("variable",
                       scales = "free",
                       nrow=nrow) +
            theme_bw() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(size = .3),
                  panel.background = element_rect(color="gray50"),
                  panel.grid = element_blank(),
                  strip.background = element_rect(fill = NA, colour = NA),
                  strip.text.x = element_text(face = "bold"))
    } else {
        d <- dplyr::group_by_(d, "variable")
        d <- dplyr::summarize_(
            d,
            m = ~median(value),  # Formulas allow non-standard evaluation
            lwr = ~stats::quantile(value, probs = .5 - level/2),
            upr = ~stats::quantile(value, probs = .5 + level/2)
            )
        p1 <- ggplot2::ggplot(d, aes_string(x = "variable", y = "m")) +
            geom_hline(yintercept = 0, lty = 2, size = .3) +
            geom_pointrange(aes_string(y="m", ymin = "lwr", ymax = "upr")) +
            coord_flip() +
            theme_bw() +
            theme(axis.title = element_blank(),
                  axis.ticks = element_line(size = .3),
                  panel.background = element_rect(color="gray50"),
                  panel.grid = element_blank())
        p1
    }
    return(p1)
}
