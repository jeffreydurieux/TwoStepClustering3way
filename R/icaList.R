# Mon Sep 16 15:49:54 2019
# Author: Jeffrey Durieux, MSc



#' @title icaList, wrapper function to do single subject ICA analyses on a list of matrices
#'
#' @param DataList, list with datasets. Each matrix is a dataset to do ICA on
#' @param nc number of components to select for ICA
#' @param outputS boolean, if TRUE only the ICA components are returned. If FALSE, all icafast output is returned
#' @param verbose boolean,if TRUE a progressbar is printed to the screen
#'
#' @return a list where each element is a matrix with single subject independent components
#' @export
#'
#'@seealso \code{\link{computeRVmat}}
#'
#' @examples
#'set.seed(42)
#'S <- vector('list', length = 4)
#'X <- vector('list', length = 4)
#'for(i in 1:4){
#' S[[i]] <- cbind(runif(1000), runif(1000))
#' A <- matrix(data = runif(4, -2, 2), nrow = 2)
#' X[[i]] <- S[[i]] %*% t(A)
#'}
#'Output <- icaList(X, 2, outputS = T, verbose = T)
#'lapply(seq_along(Output), function(lambda) cor(Output[[lambda]], S[[lambda]] ) )

icaList <- function(DataList, nc, outputS = TRUE, verbose = TRUE){

  if(verbose == TRUE){
    Result <- pbapply::pblapply(DataList, FUN = ica::icafast, nc = nc)
  }else{
    Result <- lapply(DataList, FUN = ica::icafast, nc = nc)
  }

  if(outputS == TRUE){
    Result <- lapply(1:length(Result), function(lambda) Result[[lambda]]$S)
  }

  return(Result)
}
