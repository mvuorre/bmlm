#' Plot \code{bmlm}'s mediation model as a path diagram
#'
#' Plots a path diagram for an estimated multilevel mediation model.
#'
#' @param mod A Stanfit model estimated with \code{mlm()}.
#' @param xlab Label for X
#' @param ylab Label for Y
#' @param mlab Label for M
#' @param level "Confidence" level for credible intervals. (Defaults to .95.)
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
#' To modify various settings of the underlying qgraph object, see
#' \code{\link[qgraph]{qgraph}}.
#'
#' @examples
#' # Draw a template path diagram of the mediation model
#' mlm_path_plot(template = TRUE)
#'
#' @export
mlm_path_plot <- function(mod = NULL, xlab = "X", ylab = "Y", mlab = "M",
                          level = .95,
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

        sfit <- subset(sfit, select = c(1,2,5,6))
        a <- sfit[sfit$Parameter == "a", c(2:4)]
        b <- sfit[sfit$Parameter == "b", c(2:4)]
        cp <- sfit[sfit$Parameter == "cp", c(2:4)]
        ab <- sfit[sfit$Parameter == "ab", c(2:4)]
        c <- sfit[sfit$Parameter == "c", c(2:4)]
        pme <- sfit[sfit$Parameter == "pme", c(2:4)]

        edgelabels <- c(
            paste0("\n", a[1], " \n   [", a[2], ", ", a[3], "]   \n"),
            paste0("\n", b[1], " \n  [", b[2], ", ", b[3], "]   \n"),
            paste0("\n", cp[1], " \n   [", cp[2], ", ", cp[3], "]   \n")
        )
        x <- matrix(as.numeric(c(1, b[1], 0,
                                 0, 1, 0,
                                 a[1], cp[1] ,1)), byrow=T, nrow = 3)
    }

    # Set bmlm default args to qgraph
    qargs <- list(...)
    qargs$input <- x
    qargs$labels <- c(mlab, ylab, xlab)
    if (is.null(qargs$border.width)) qargs$border.width <- 2
    if (is.null(qargs$edge.label.cex)) qargs$edge.label.cex <- 1.2
    if (is.null(qargs$edge.color)) qargs$edge.color <- "black"
    if (is.null(qargs$vsize)) qargs$vsize <- 16
    if (is.null(qargs$vsize2)) qargs$vsize2 <- 12
    if (is.null(qargs$asize)) qargs$asize <- 4
    if (is.null(qargs$esize)) qargs$esize <- 4
    if (is.null(qargs$label.norm)) qargs$label.norm <- "OOOOOO"
    if (is.null(qargs$edge.labels)) qargs$edge.labels <- edgelabels
    if (is.null(qargs$mar)) qargs$mar <- c(4, 4, 4, 4)
    if (is.null(qargs$fade)) qargs$fade <- FALSE
    if (is.null(qargs$layout)) qargs$layout <- "circle"
    if (is.null(qargs$shape)) qargs$shape <- "rectangle"
    if (is.null(qargs$weighted)) qargs$weighted <- FALSE

    # Create plot
    do.call(qgraph::qgraph, qargs)

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
#' @param level X level for Credible Intervals. (Defaults to .95.)
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
#' @details The point estimate for the coefficient plot is the posterior mean.
#'
#' @import ggplot2
#'
#' @export
mlm_pars_plot <- function(mod = NULL,
                          type = "hist",
                          color = "black",
                          p_shape = 15,
                          p_size = 1.2,
                          level = 0.95,
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

    Theme <- theme_bw() +
        theme(axis.title = element_blank(),
              axis.ticks = element_line(size = .3),
              panel.background = element_rect(color="gray50"),
              panel.grid = element_blank())

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
    } else if (type == "coef") {
        td <- d
        d <- dplyr::group_by_(d, "variable")
        d <- dplyr::summarize_(
            d,
            m = ~mean(value),
            lwr = ~stats::quantile(value, probs = .5 - level/2),
            upr = ~stats::quantile(value, probs = .5 + level/2)
            )
        d <- as.data.frame(d)
        # If parameter is varying, do stuff
        d$var <- grepl("u_", d[, "variable"])
        par_var <- any(d$var)
        if (par_var) {
            # Reorder estimates on value
            d[, "variable"] <- stats::reorder(d[, "variable"], d[, "m"], mean)
            # Highlight average effect if present
            d$hl <- ifelse(d$var, "a", "b")
            p1 <- ggplot2::ggplot(d, aes_string(x = "variable",
                                                y = "m",
                                                color = "hl")) +
                scale_color_manual(values = c(color, "tomato2"), guide = "none") +
                geom_hline(yintercept = 0, lty = 2, size = .3) +
                geom_point(aes_string(y="m"), shape = p_shape, size = p_size) +
                geom_linerange(aes_string(y="m", ymin = "lwr", ymax = "upr"),
                               size = p_size / 4.5) +
                Theme + theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank())
        } else {
            p1 <- ggplot2::ggplot(d, aes_string(x = "variable", y = "m")) +
                geom_hline(yintercept = 0, lty = 2, size = .3) +
                geom_point(aes_string(y="m"), shape = p_shape, size = p_size) +
                geom_linerange(aes_string(y="m", ymin = "lwr", ymax = "upr"),
                               size = p_size / 4.5) +
                Theme
        }
    } else {  # Violin plot
        # If parameter is varying, do stuff
        d$var <- grepl("u_", d[, "variable"])
        par_var <- any(d$var)
        if (par_var) {
            # Reorder estimates on value
            d[, "variable"] <- stats::reorder(d[, "variable"], d[, "value"], mean)
            # Highlight average effect if present
            d$hl <- ifelse(d$var, "a", "b")
            p1 <- ggplot2::ggplot(d, aes_string(x = "variable",
                                                y = "value",
                                                fill = "hl")) +
                scale_fill_manual(values = c(color, "tomato2"), guide = "none") +
                geom_hline(yintercept = 0, lty = 2, size = .3) +
                geom_violin(data = d, aes_string(y="value"),
                            col = NA) +
                Theme + theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank())
        } else {
            p1 <- ggplot2::ggplot(d, aes_string(x = "variable", y = "m")) +
                geom_hline(yintercept = 0, lty = 2, size = .3) +
                geom_violin(data = d, aes_string(y="value"),
                            col = NA, fill = color) +
                Theme
        }
    }
    return(p1)
}
