#' Plot \code{bmlm}'s mediation model as a path diagram
#'
#' Plots a path diagram for an estimated multilevel mediation model.
#'
#' @param mod A Stanfit model estimated with \code{mlm()}.
#' @param xlab Label for X
#' @param ylab Label for Y
#' @param mlab Label for M
#' @param level "Confidence" level for credible intervals. (Defaults to .95.)
#' @param random Should the "random" effects SDs be displayed? (Default = TRUE)
#' @param text Should additional parameter values be displayed?
#' (Defaults to FALSE.)
#' @param id Plot an individual-level path diagram by specifying ID number.
#' @param digits Number of significant digits to show on graph. (Default = 2.)
#' @param ... Other arguments passed on to \code{qgraph::qgraph()}.
#'
#' @return A qgraph object.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details Plots a path diagram of the mediation model,
#' with estimated parameter values and credible intervals. Can also
#' be used to draw a template diagram of the mediation model by not
#' specifying input to the \code{mod} argument.
#'
#' To modify various settings of the underlying qgraph object, see
#' \code{\link[qgraph]{qgraph}}.
#'
#' @examples
#' # Draw a template path diagram of the mediation model
#' mlm_path_plot()
#'
#' @export
mlm_path_plot <- function(mod = NULL, xlab = "X", ylab = "Y", mlab = "M",
                          level = .95,
                          random = TRUE,
                          text = FALSE,
                          id = NULL,
                          digits = 2,
                          ...){

    # Requires the qgraph package
    if (!requireNamespace("qgraph", quietly = TRUE)) {
        stop("qgraph package needed for this function. Please install it.",
             call. = FALSE)
    }

    if (is.null(mod)) {
        # If user wants a template diagram
        edgelabels <- c(" \n  a  \n ", " \n  b  \n ", " \n  c'  \n ")
        x <- matrix(c(1, 1, 0,
                      0, 1, 0,
                      1, 1 ,1), byrow=T, nrow = 3)
    } else {
        if (!(class(mod) == "stanfit")) {
            stop("Model is not a stanfit object.", call. = FALSE)
        }
        params <- c("a", "b", "cp", "me", "c", "pme",
                    "tau_a", "tau_b", "tau_cp", "covab")
        # Specify whether person-specific or average params given
        if (!is.null(id)){  # If person-specific model requested
            if (id > mod@sim$dims_oi$u_a) {
                stop("ID index out of bounds.", call. = FALSE)
            }
            random = FALSE
            sfit <- mlm_summary(
                mod,
                pars = paste0("u_", params[1:6], "[", id, "]"),
                level = level)
            sfit$Parameter <- params[1:6]
        } else {  # Give average model
            sfit <- mlm_summary(mod,
                                pars = params,
                                level = level,
                                digits = 6)
        }

        sfit <- subset(sfit, select = c(1,2,3,5,6))
        sfit[,2:5] <- signif(sfit[,2:5], digits)
        a <- sfit[sfit$Parameter == "a", c(2:5)]
        b <- sfit[sfit$Parameter == "b", c(2:5)]
        cp <- sfit[sfit$Parameter == "cp", c(2:5)]
        me <- sfit[sfit$Parameter == "me", c(2:5)]
        c <- sfit[sfit$Parameter == "c", c(2:5)]
        pme <- sfit[sfit$Parameter == "pme", c(2:5)]
        if (is.null(id)) {
            tau_a <- sfit[sfit$Parameter == "tau_a", c(2:5)]
            tau_b <- sfit[sfit$Parameter == "tau_b", c(2:5)]
            tau_cp <- sfit[sfit$Parameter == "tau_cp", c(2:5)]
            covab <- sfit[sfit$Parameter == "covab", c(2:5)]
        }

        edgelabels <- c(
            paste0("\na = ", a[1], "\n [", a[3], ", ", a[4], "] \n"),
            paste0("\nb = ", b[1], "\n [", b[3], ", ", b[4], "] \n"),
            paste0("\nc' = ", cp[1], "\n [", cp[3], ", ", cp[4], "] \n")
        )

        if (random) {
            edgelabels <- c(
                paste0("\na = ", a[1], "\n [", a[3], ", ", a[4], "] \n",
                       "SD = ", tau_a[1], "\n [", tau_a[3], ", ", tau_a[4], "] \n"),
                paste0("\nb = ", b[1], "\n [", b[3], ", ", b[4], "] \n",
                       "SD = ", tau_b[1], "\n [", tau_b[3], ", ", tau_b[4], "] \n"),
                paste0("\nc' = ", cp[1], "\n [", cp[3], ", ", cp[4], "] \n",
                       "SD = ", tau_cp[1], "\n [", tau_cp[3], ", ", tau_cp[4], "] \n")
            )
        }

        x <- matrix(as.numeric(c(1, b[1], 0,
                                 0, 1, 0,
                                 a[1], cp[1] ,1)), byrow=T, nrow = 3)
    }

    # Set bmlm default args to qgraph
    qargs <- list(...)
    qargs$input <- x
    qargs$labels <- c(mlab, ylab, xlab)
    if (is.null(qargs$border.width)) qargs$border.width <- 2
    if (is.null(qargs$edge.label.cex)) qargs$edge.label.cex <- 1.1
    if (is.null(qargs$edge.color)) qargs$edge.color <- "black"
    if (is.null(qargs$vsize)) qargs$vsize <- 16
    if (is.null(qargs$vsize2)) qargs$vsize2 <- 12
    if (is.null(qargs$asize)) qargs$asize <- 4
    if (is.null(qargs$esize)) qargs$esize <- 4
    if (is.null(qargs$label.cex)) qargs$label.cex <- 1.2
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
    if (text & !is.null(mod)){
        graphics::text(
            -1.2, 1.2,
            paste0("me = ", me[1], " [", me[3], ", ", me[4], "]"), pos=4)
        graphics::text(
            -1.2, 1.05,
            paste0("c = ", c[1], " [", c[3], ", ", c[4], "]"), pos=4)
        graphics::text(
            -1.2, 0.9,
            paste0("pme = ", pme[1], " [", pme[3], ", ", pme[4], "]"), pos=4)
        if (is.null(id)){
            graphics::text(
                -1.2, 0.75,
                paste0("cov(a,b) = ", covab[1], " [",
                       covab[3], ", ", covab[4], "]"), pos=4)
        }
    }
}

#' Plot estimated parameters of multilevel mediation model
#'
#' Plot the model's estimated parameters as histograms or a coefficient plot.
#'
#' @param mod A Stanfit model estimated with \code{mlm()}.
#' @param type Type of the plot, \code{hist}, \code{coef}, or \code{violin}.
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
#' @importFrom stats as.formula model.matrix quantile
#'
#' @export
mlm_pars_plot <- function(mod = NULL,
                          type = "hist",
                          color = "black",
                          p_shape = 15,
                          p_size = 1.2,
                          level = 0.95,
                          nrow = 3,
                          pars = c("a", "b", "cp", "covab", "me", "c", "pme")){

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

#' Plot fitted values of M and Y from multilevel mediation model
#'
#' Plot population-level fitted values and X% CI, and subject level fitted
#' values, for M and Y.
#'
#' @param mod A multilevel mediation model estimated with \code{mlm()}.
#' @param d A \code{data.frame} or a \code{data_frame} used in fitting model.
#' @param id Name of id variable (identifying subjects) in data (\code{d}).
#' @param x Name of X variable in \code{data}.
#' @param m Name of M variable in \code{data}.
#' @param y Name of Y variable in \code{data}.
#' @param level X level for Credible Intervals. (Defaults to .95.)
#' @param n Number of points along X to evaluate fitted values on.
#' See details.
#' @param binary_y Set to TRUE if the outcome variable (Y) is 0/1.
#' @param mx Should the X axis of the M-Y figure be "fitted" values,
#' or "data" values. Defaults to "fitted".
#' @param fixed Should the population-level ("fixed") fitted values be shown?
#' @param random Should the subject-level ("random") fitted values be shown?
#' @param h_jitter Horizontal jitter of points. Defaults to 0.
#' @param v_jitter Vertical jitter of points. Defaults to 0.
#' @param bar_width Width of the error bars. Defaults to 0.2.
#' @param bar_size Thickness of the error bars. Defaults to 0.75.
#' @param n_samples Number of MCMC samples to use in calculating fitted values.
#' See details.
#'
#' @return A list of two ggplot2 objects.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#' @details If \code{n = 2}, the fitted values will be represented as points
#' with X% CIs as "error bars". If \code{n > 2}, the representation will be a
#' line with a Confidence Ribbon instead.
#' If a very large model is fitted with a large number of MCMC iterations,
#' the function might take a long time to run. In these cases, users can set
#' \code{n_samples} to a smaller value (e.g. 1000), in which case the fitted
#' values (and the CIs) will be based on a random subset of \code{n_samples}
#' MCMC samples. The default value is NA, meaning that all MCMC samples are
#' used.
#'
#' @import ggplot2
#'
#' @export
mlm_spaghetti_plot <- function(mod=NULL, d=NULL,
                               id = "id", x="x", m="m", y="y",
                               level = .95,
                               n = 12,
                               binary_y = FALSE,
                               mx = "fitted",
                               fixed = TRUE,
                               random = TRUE,
                               h_jitter = 0,
                               v_jitter = 0,
                               bar_width = .2,
                               bar_size = .75,
                               n_samples = NA) {

    # Check for model
    if (is.null(mod)) stop("No model object entered.")
    # Check for data
    if (is.null(d)) stop("No data object entered.")
    if (class(d)[1] == "tbl_df") d <- as.data.frame(d)  # Allow tibbles
    # Make IDs sequential
    d$id = as.integer(as.factor(as.character(d[, id])))
    # At least one of fixed, random = TRUE
    if (!any(fixed, random)) stop("fixed or random (or both) must be TRUE.")

    if (fixed) {
        # Posterior draws of pop-level params to a data frame
        post <- as.data.frame(mod, pars = c("dy", "cp", "b", "dm", "a"))

        if (!is.na(n_samples)) {
            msg <- paste("Fitted values calculated on a subset of",
                         n_samples,
                         "MCMC samples.")
            message(msg)
            post <- post[sample(1:nrow(post), n_samples),]
        }

        # Calculate fitted values of M
        M <- data.frame(x = seq(min(d[,x]), max(d[,x]), length=n))
        M <- data.frame(matrix(ncol=0, nrow=n))
        M[,x] <- seq(min(d[,x]), max(d[,x]), length=n)
        MX <- model.matrix(as.formula(paste(" ~ 1 +", x)), data=M)
        Mfit <- matrix(ncol = nrow(post), nrow = nrow(MX))
        for (i in 1:nrow(post)) {Mfit[,i] <- MX %*% as.matrix(post)[i,c("dm", "a")]}
        M$m_fitted_lower <- apply(Mfit, 1, quantile, prob = .5 - level/2)
        M$m_fitted_mean <- apply(Mfit, 1, mean)
        M$m_fitted_upper <- apply(Mfit, 1, quantile, prob = .5 + level/2)

        # Calculate fitted values of Y based on fitted values of M
        if (mx == "fitted") {
            Y <- data.frame(m = seq(min(M$m_fitted_mean),
                                    max(M$m_fitted_mean),
                                    length = n))
        } else {
            Y <- data.frame(m = seq(min(d[,m]),
                                    max(d[,m]),
                                    length = n))
        }
        names(Y) <- m
        YX <- model.matrix(as.formula(paste(" ~ 1 +", m)), data=Y)
        Yfit <- matrix(ncol = nrow(post), nrow = nrow(YX))
        if (binary_y) {
            for (i in 1:nrow(post)) {Yfit[,i] <-
                1 / (1 + exp(-(YX %*% as.matrix(post)[i,c("dy", "b")])))}
        } else {
            for (i in 1:nrow(post)) {Yfit[,i] <- YX %*% as.matrix(post)[i,c("dy", "b")]}
        }
        Y$y_fitted_lower <- apply(Yfit, 1, quantile, prob = .5 - level/2)
        Y$y_fitted_mean <- apply(Yfit, 1, mean)
        Y$y_fitted_upper <- apply(Yfit, 1, quantile, prob = .5 + level/2)
    }

    if (random) {
        # Posterior means of sub-level parameters
        U <- data.frame(
            u_dm = rstan::summary(mod, "u_dm", probs=NA)$summary[,1],
            u_a = rstan::summary(mod, "u_a", probs=NA)$summary[,1],
            u_dy = rstan::summary(mod, "u_dy", probs=NA)$summary[,1],
            u_b = rstan::summary(mod, "u_b", probs=NA)$summary[,1])
        U[,id] <- 1:length(unique(d[,id]))

        # Calculate fitted values of M
        M_vary <- data.frame(matrix(ncol=2, nrow=0))
        names(M_vary) <- c(id, x)
        # Create even grid of fitted_m predictor per subject
        for (s in unique(d[,id])) {
            xx <- seq(min(d[,x][d[,id]==s]),
                      max(d[,x][d[,id]==s]),
                      length = n)
            tmp <- data.frame(id = s,
                              x = xx)
            names(tmp) <- c(id, x)
            M_vary <- rbind(M_vary, tmp)
        }
        M_vary[,id] = as.factor(as.character(M_vary[,id]))

        M_vary <- merge(M_vary, U[,c(id, "u_dm", "u_a")])
        M_vary$m_fitted_mean <- M_vary$u_dm + M_vary$u_a*M_vary[,x]

        # Calculate fitted values of Y
        Y_vary <- data.frame(matrix(ncol=2, nrow=0))
        names(Y_vary) <- c(id, m)
        # Create even grid of fitted_m predictor per subject
        for (s in unique(M_vary[,id])) {
            if (mx == "fitted") {
                m_fitted_mean <- seq(min(M_vary$m_fitted_mean[M_vary[,id]==s]),
                                     max(M_vary$m_fitted_mean[M_vary[,id]==s]),
                                     length = n)
            } else {
                m_fitted_mean <- seq(min(d[,m][d[,id]==s]),
                                     max(d[,m][d[,id]==s]),
                                     length = n)
            }
            tmp <- data.frame(id = s,
                              m = m_fitted_mean)
            names(tmp) <- c(id, m)
            Y_vary <- rbind(Y_vary, tmp)
        }
        Y_vary <- merge(Y_vary, U[,c(id, "u_dy", "u_b")])
        Y_vary$y_fitted_mean <- Y_vary$u_dy + Y_vary$u_b*Y_vary[,m]
        if (binary_y) {
            Y_vary$y_fitted_mean <- 1/(1+exp(-Y_vary$y_fitted_mean))
        }
    }

    # Create figures
    pm <- ggplot()
    pm <- pm + scale_y_continuous()
    pm <- pm + scale_x_continuous()
    if (random) {
        if (n > 2) {
            pm <- pm + geom_line(data=M_vary,
                                 aes_(x=as.name(x),
                                      y=as.name("m_fitted_mean"),
                                      group=as.name(id)),
                                 alpha=.25)
        } else {
            pm <- pm + geom_point(data=M_vary,
                                  aes_(x=as.name(x),
                                       y=as.name("m_fitted_mean"),
                                       group=as.name(id)),
                                  alpha=.25,
                                  position = position_jitter(h_jitter, v_jitter))
        }
    }
    if (fixed) {
        if (n > 2) {
            pm <- pm + geom_ribbon(data = M,
                                   aes_(x = as.name(x),
                                        ymin = as.name("m_fitted_lower"),
                                        ymax = as.name("m_fitted_upper")),
                                   alpha=.25, col=NA)
            pm <- pm + geom_line(data = M,
                                 aes_(x = as.name(x),
                                      y = as.name("m_fitted_mean")),
                                 size=1)
        } else {
            pm <- pm + geom_errorbar(data = M,
                                     aes_(x = as.name(x),
                                          ymin = as.name("m_fitted_lower"),
                                          ymax = as.name("m_fitted_upper")),
                                     alpha=.8,
                                     width = bar_width,
                                     size = bar_size)
            pm <- pm + geom_point(data = M,
                                  aes_(x = as.name(x),
                                       y = as.name("m_fitted_mean")),
                                  size=1)
        }
    }
    pm <- pm + labs(x=x, y=m)
    pm

    py <- ggplot()
    py <- py + scale_y_continuous()
    py <- py + scale_x_continuous()
    if (random) {
        if (n > 2) {
            py <- py + geom_line(data=Y_vary,
                                 aes_(x=as.name(m),
                                      y=as.name("y_fitted_mean"),
                                      group=as.name(id)),
                                 alpha=.25)
        } else {
            py <- py + geom_point(data=Y_vary,
                                  aes_(x=as.name(m),
                                       y=as.name("y_fitted_mean"),
                                       group=as.name(id)),
                                  alpha=.25,
                                  position = position_jitter(h_jitter, v_jitter))
        }
    }
    if (fixed) {
        if (n > 2) {
            py <- py + geom_ribbon(data = Y,
                                   aes_(x = as.name(m),
                                        ymin = as.name("y_fitted_lower"),
                                        ymax = as.name("y_fitted_upper")),
                                   alpha=.25, col=NA)
            py <- py + geom_line(data = Y,
                                 aes_(x = as.name(m),
                                      y = as.name("y_fitted_mean")),
                                 size=1)
        } else {
            py <- py + geom_errorbar(data = Y,
                                     aes_(x = as.name(m),
                                          ymin = as.name("y_fitted_lower"),
                                          ymax = as.name("y_fitted_upper")),
                                     alpha=.8,
                                     width = bar_width,
                                     size = bar_size)
            py <- py + geom_point(data = Y,
                                  aes_(x = as.name(m),
                                       y = as.name("y_fitted_mean")),
                                  size=1)
        }
    }
    py <- py + labs(x=m, y=y)
    py

    return(list(pm, py))
}
