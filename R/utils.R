#' Get Stan code for bmlm model
#'
#' Prints Stan code for bmlm model that's used in calls to \code{bmlm::mlm()}
#'
#' @return A text string including the Stan code.
#'
#' @author Matti Vuorre \email{mv2521@columbia.edu}
#'
#'
#' @examples
#' x <- get_stancode()
#' cat(x)
#' print(x)
#' @export
get_stancode <- function(){
    model_file <- system.file("stan/bmlm.stan", package="bmlm")
    model_string <-readLines(model_file)
    return(model_string)
}
