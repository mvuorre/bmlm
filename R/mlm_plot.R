#' Plot multilevel mediation
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
mlm_plot <- function(mod = NULL, xlab = "X", ylab = "Y", mlab = "M"){

    # Requires the qgraph package
    if (!requireNamespace("qgraph", quietly = TRUE)) {
        stop("Pkg needed for this function to work. Please install it.",
             call. = TRUE)
    }
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
