#' Plot multilevel mediation as a path diagram
#'
#' Plots a path diagram from a multilevel mediation model.
#'
#' @param mod A Stanfit model estimated with \code{bmlm::mlm()}.
#' @param xlab Label for X
#' @param ylab Label for Y
#' @param mlab Label for M
#'
#' @return A qgraph object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Experimental. Plots a path diagram of the mediation model,
#' with estimated average parameter values and posterior probabilities.
#'
#' @export
mlm_path_plot <- function(mod = NULL, xlab = "X", ylab = "Y", mlab = "M"){

    # Requires the qgraph package
    if (!requireNamespace("qgraph", quietly = TRUE)) {
        stop("qgraph package needed for this function. Please install it.",
             call. = TRUE)
    }
    # Ensure suitable model object passed
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    # Get model summary
    sfit <- mlm_summary(mod)
    a <- round(as.numeric(sfit[1, c("Mean", "pprob")]), 2)
    b <- round(as.numeric(sfit[2, c("Mean", "pprob")]), 2)
    cp <- round(as.numeric(sfit[3, c("Mean", "pprob")]), 2)
    ab <- round(as.numeric(sfit[5, c("Mean", "pprob")]), 2)
    c <- round(as.numeric(sfit[6, c("Mean", "pprob")]), 2)
    pme <- round(as.numeric(sfit[7, c("Mean", "pprob")]), 2)

    # Specify plot layout and parameters
    edgelabels <- c(
        paste0(" a \n (M = ", a[1], ") \n (p = ", a[2], ") \n"),
        paste0(" b \n (M = ", b[1], ") \n (p = ", b[2], ") \n"),
        paste0(" cp \n (M = ", cp[1], ") \n (p = ", cp[2], ") \n")
    )
    x <- matrix(c(1, b[1], 0,
                  0, 1, 0,
                  a[1], cp[1] ,1), byrow=T, nrow = 3)

    # Create plot
    p2 <- qgraph::qgraph(x, layout = "circle",
                         shape = "square",
                         labels = c(mlab, ylab, xlab),
                         border.width = 2,
                         edge.color = "black",
                         edge.labels = edgelabels,
                         edge.label.cex = 1.2,
                         fade = FALSE)
    graphics::text(-1.2, 1.1,
         paste0("ab: M = ", ab[1], " (p = ", a[2], ")"), pos=4)
    graphics::text(-1.2, 0.9,
         paste0("c: M = ", c[1], " (p = ", c[2], ")"), pos=4)
    graphics::text(-1.2, 0.7,
         paste0("%me: M = ", pme[1], " (p = ", pme[2], ")"), pos=4)
}

#' Plot marginal posterior histograms
#'
#' Plot the model's estimated parameters as histograms or a coefficient plot.
#'
#' @param mod A Stanfit model estimated with \code{bmlm::mlm()}.
#' @param type Type of the plot, \code{hist} or \code{coefplot}.
#' @param level X level for Credible Intervals.
#' @param color Color for plots.
#' @param pars List of parameters to plot.
#'
#' @return A ggplot2 object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Experimental. Plots a path diagram of the mediation model,
#' with estimated average parameter values and posterior probabilities.
#'
#'@import ggplot2
#'
#' @export
mlm_pars_plot <- function(mod = NULL,
                          type = "hist",
                          color = "black",
                          level = 0.91,
                          pars = c("a", "b", "cp", "corrab", "ab", "c", "pme")){

    # Requires the reshape2 package
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("reshape2 package needed for this function. Please install it.",
             call. = TRUE)
    }

    # Ensure suitable model object passed
    if (!(class(mod) == "stanfit")) stop("Model is not a stanfit object.")

    d <- as.data.frame(mod)
    d <- reshape2::melt(d)
    if (type == "hist"){
        p1 <- ggplot2::ggplot(d, aes(x=value)) +
            stat_bin(aes(y=..ndensity..),
                     col="white", fill=color) +
            labs(x="", y="") +
            facet_wrap(~variable,
                       scales = "free",
                       ncol=3) +
            theme_minimal() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.background = element_rect(color="gray50"),
                  panel.grid = element_blank())
    } else {
        d <- dplyr::group_by(d, variable)
        d <- dplyr::summarize(d,
                              m = mean(value),
                              lwr = stats::quantile(value, probs = .5 - level/2),
                              upr = stats::quantile(value, probs = .5 + level/2))
        p1 <- ggplot2::ggplot(d, aes(x = variable, y = m)) +
            geom_pointrange(aes(y=m, ymin = lwr, ymax=upr)) +
            labs(x="Value", y="Parameter") +
            coord_flip() +
            theme_minimal() +
            labs(title="Posterior samples of bestan fit") +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.background = element_rect(color="gray50"),
                  panel.grid = element_blank())
    }
    return(p1)
}
