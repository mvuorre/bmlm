#' Relationship between work stressors, work dissatisfaction,
#' and relationship dissatisfaction.
#'
#' @description Simulated data from Intensive Longitudinal Methods:
#' An Introduction to Diary and Experience Sampling Research.
#' (Bolger, & Laurenceau, 2013, chapter 9;
#' \url{http://www.intensivelongitudinal.com/index.html}).
#'
#' @docType data
#'
#' @usage data(BLch9)
#'
#' @format A data frame with 2100 rows and 8 variables:
#' \describe{
#'   \item{id}{ID of study participant}
#'   \item{time}{Time}
#'   \item{fwkstrs}{Number of work stressors}
#'   \item{fwkdis}{Work dissatisfaction rating}
#'   \item{freldis}{Relationship dissatisfaction}
#'   \item{x}{Subject-mean deviated number of work stressors}
#'   \item{m}{Subject-mean deviated work dissatisfaction rating}
#'   \item{y}{Subject-mean deviated relationship dissatisfaction}
#' }
#'
#' @source \url{http://www.intensivelongitudinal.com/datasets.html}
"BLch9"

#' Judgments of performance in a video game
#'
#' @description Data from an experiment where participants rated their
#' performance in a video game in two conditions.
#' (Experiment 1 in Metcalfe, Eich, & Castel, 2010;
#' \url{http://www.sciencedirect.com/science/article/pii/S0010027710001113}).
#'
#' @docType data
#'
#' @usage data(MEC2010)
#'
#' @format A data frame with 344 rows and 4 variables:
#' \describe{
#'   \item{subj}{Subject id number.}
#'   \item{lag}{Lag condition (0 = no lag, 1 = 250ms lag).}
#'   \item{hr}{Hit rate.}
#'   \item{jop}{Judgment of Performance.}
#' }
#'
#' @source Metcalfe, J., Eich, T. S., & Castel, A. D. (2010).
#' Metacognition of agency across the lifespan.
#' Cognition, 116(2), 267-282.
#' \url{https://doi.org/10.1016/j.cognition.2010.05.009}
"MEC2010"
