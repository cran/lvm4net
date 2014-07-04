#' @keywords internal

is.adjacency <- function(y){
	
	is.matrix(y) && nrow(y) == ncol(y) && diag(y) == 0 && y %in% c(0,1)

}