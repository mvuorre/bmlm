#' @keywords internal
# Make IDs sequential (assumes they are ordered)
seq_ids <- function(x){
    as.integer(as.factor(as.character(x)))
}
