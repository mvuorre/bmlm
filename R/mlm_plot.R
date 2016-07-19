#' Plot \code{bmlm}'s mediation model as a path diagram
#'
#' Plots a path diagram for an estimated multilevel mediation model.
#'
#' @param mod A Stanfit model estimated with \code{mlm()}.
#' @param xlab Label for X
#' @param ylab Label for Y
#' @param mlab Label for M
#' @param border.width Size of node borders (defaults to 2).
#' @param edge.label.cex Text size.
#' @param edge.color Color of the path arrows. (Set NULL to color negative paths
#' red, and positive paths green.)
#' @param fade Should edges fade to white? (Defaults to FALSE.)
#' @param level "Confidence" level for credible intervals. (Defaults to .99.)
#' @param text Should additional parameter values be displayed?
#' (Defaults to FALSE.)
#' @param template Should an empty template diagram be plotted?
#' (Defaults to FALSE.)
#' @param id Plot an individual-level path diagram by specifying ID number.
#' @param ... Other arguments passed on to \code{qgraph::qgraph()}.
#'
#' @return A qgraph object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Plots a path diagram of the mediation model,
#' with estimated parameter values and credible intervals. Can also
#' be used to draw a template diagram of the mediation model by setting
#' \code{template = TRUE}.
#'
#' @examples
#' # Draw a template path diagram of the mediation model
#' mlm_path_plot(template = TRUE)
#'
#' @export
mlm_path_plot <- function(mod = NULL, xlab = "X", ylab = "Y", mlab = "M",
                          border.width = 2,
                          edge.label.cex = 1,
                          edge.color = "black",
                          fade = FALSE,
                          level = .99,
                          text = FALSE,
                          template = FALSE,
                          id = NULL,
                          ...){

    # Requires the qgraph package
    if (!requireNamespace("qgraph", quietly = TRUE)) {
        stop("qgraph package needed for this function. Please install it.",
             call. = FALSE)
    }
    if (template) {
        # If user wants a template diagram
        edgelabels <- c(" \n  a  \n ", " \n  b  \n ", " \n  c'  \n ")
        x <- matrix(c(1, 1, 0,
                      0, 1, 0,
                      1, 1 ,1), byrow=T, nrow = 3)
    } else {
        if (!(class(mod) == "stanfit")) {
            stop("Model is not a stanfit object.", call. = FALSE)
        }
        params <- c("a", "b", "cp", "ab", "c", "pme")
        # Specify whether person-specific or average params given
        if (!is.null(id)){  # If person-specific model requested
            if (id > mod@sim$dims_oi$u_a) {
                stop("ID index out of bounds.", call. = FALSE)
            }
            sfit <- mlm_summary(
                mod,
                pars = paste0("u_", params, "[", id, "]"),
                level = level)
            sfit$Parameter <- params
        } else {  # Give average model
            sfit <- mlm_summary(mod,
                                pars = params,
                                level = level)
        }

        sfit <- subset(sfit, select = c("Parameter", "Mean", "ci_lwr", "ci_upr"))
        a <- sfit[sfit$Parameter == "a", c("Mean", "ci_lwr", "ci_upr")]
        b <- sfit[sfit$Parameter == "b", c("Mean", "ci_lwr", "ci_upr")]
        cp <- sfit[sfit$Parameter == "cp", c("Mean", "ci_lwr", "ci_upr")]
        ab <- sfit[sfit$Parameter == "ab", c("Mean", "ci_lwr", "ci_upr")]
        c <- sfit[sfit$Parameter == "c", c("Mean", "ci_lwr", "ci_upr")]
        pme <- sfit[sfit$Parameter == "pme", c("Mean", "ci_lwr", "ci_upr")]

        edgelabels <- c(
            paste0("\n", a[1], " \n   [", a[2], ", ", a[3], "]   \n"),
            paste0("\n", b[1], " \n  [", b[2], ", ", b[3], "]   \n"),
            paste0("\n", cp[1], " \n   [", cp[2], ", ", cp[3], "]   \n")
        )
        x <- matrix(as.numeric(c(1, b[1], 0,
                                 0, 1, 0,
                                 a[1], cp[1] ,1)), byrow=T, nrow = 3)
    }
    # Create plot
    qgraph::qgraph(x, layout = "circle",
                   shape = "rectangle",
                   vsize = 16,
                   vsize2 = 12,
                   labels = c(mlab, ylab, xlab),
                   label.norm = "OOOOOO",
                   border.width = border.width,
                   edge.labels = edgelabels,
                   edge.label.cex = edge.label.cex,
                   fade = fade,
                   asize = 10,
                   esize = 10,
                   mar = c(4, 4, 4, 4),
                   edge.color = edge.color,
                   ...)
    # If ID is specified, note this on plot
    if (!is.null(id)) {
        graphics::text(1.25, 1.25, paste0("ID: ", id), font = 2)
    }
    if (text & !template){
        graphics::text(
            -1.2, 1.1,
            paste0("ab = ", ab[1], " [", ab[2], ", ", ab[3], "]"), pos=4)
        graphics::text(
            -1.2, 0.9,
            paste0("c = ", c[1], " [", c[2], ", ", c[3], "]"), pos=4)
        graphics::text(
            -1.2, 0.7,
            paste0("%me = ", pme[1], " [", pme[2], ", ", pme[3], "]"), pos=4)
    }
}

#' Plot estimated parameters of multilevel mediation model
#'
#' Plot the model's estimated parameters as histograms or a coefficient plot.
#'
#' @param mod A Stanfit model estimated with \code{mlm()}.
#' @param type Type of the plot, \code{hist} or \code{coefplot}.
#' @param level X level for Credible Intervals. (Defaults to .99.)
#' @param color Color (and fill) for plots.
#' @param p_shape Shape of points for coefplot.
#' @param p_size Size of points for coefplot.
#' @param pars Which parameters to plot.
#' @param nrow Number of rows for multiple histograms.
#'
#' @return A ggplot2 object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details The point estimate for the coefficient plot is the posterior median.
#'
#' @import ggplot2
#'
#' @export
mlm_pars_plot <- function(mod = NULL,
                          type = "hist",
                          color = "black",
                          p_shape = 15,
                          p_size = 1.2,
                          level = 0.99,
                          nrow = 3,
                          pars = c("a", "b", "cp", "corrab", "ab", "c", "pme")){

    # Requires the reshape2 package
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("reshape2 package needed for this function. Please install it.",
             call. = FALSE)
    }

    # Ensure suitable model object passed
    if (!(class(mod) == "stanfit")) {
        stop("Model is not a stanfit object.", call. = FALSE)
    }

    d <- as.data.frame(mod, pars = pars)
    d <- reshape2::melt(d)

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
            geom_point(aes_string(y="m"), shape = p_shape, size = p_size) +
            geom_linerange(aes_string(y="m", ymin = "lwr", ymax = "upr"),
                           size = p_size / 4.5) +
            coord_flip() +
            theme_bw() +
            theme(axis.title = element_blank(),
                  axis.ticks = element_line(size = .3),
                  panel.background = element_rect(color="gray50"),
                  panel.grid = element_blank())
    }
    return(p1)
}
