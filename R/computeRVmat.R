# Thu Jun 27 09:47:44 2019

# Author: Jeffrey Durieux, MSc

# What: function that computes all pairwise similarties (with the modified RV)
# input: a list object where each element is a matrix of one subject
# output: if argument dist == TRUE then a dissimilarity matrix else a similarity matrix

#' @title Compute a similarity or dissimilarity matrix with all pairwise modified RV-coefficients
#' @description This function computes all pairwise modified RV-coefficients between the matrices that are stored in the DataList object
#' @param DataList a list with N matrices
#' @param dist boolean, if TRUE a distance matrix is return, if FALSE a similarity matrix is return
#' @param verbose if TRUE a progresbar is printed to the console
#'
#' @return a NxN similarity matrix or a dist object (dependening on the dist argument)
#' @export
#'
#'@seealso \code{\link{modRV}, \link{icaList}}
#'
#' @examples
#'Mat <- matrix(rnorm(1000), 100)
#'SVD <- svd(Mat)
#'X <- vector(mode = 'list', length = 10)
#'for(i in 1:5){
#'X[[i]] <- SVD$u[,-i] %*% diag(SVD$d[-i]) %*% t(SVD$v[,-i])
#'}
#'X[[6]] <- matrix(rnorm(1000, mean = 0), 100)
#'SVD <- svd(X[[6]])
#'for(i in 6:10){
#'X[[i]] <- SVD$u[,-(i-5)] %*% diag(SVD$d[-(i-5)]) %*% t(SVD$v[,-(i-5)])
#'}
#'
#'RVmat <- computeRVmat(DataList = X, dist = FALSE, verbose = TRUE)
#'heatmap(RVmat)


computeRVmat <- function(DataList = DataList, dist = TRUE, verbose = TRUE){

  N <- length(DataList)

  comb <- t(utils::combn(1:N, 2))

  if(verbose == TRUE){
    pb <- txtProgressBar(min = 0, max = nrow(comb), initial = 0)

    RVsS <- matrix(data = NA, nrow = N , ncol = N)
    RVS <- numeric()

    for(i in 1:nrow(comb)){
      RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])

      res <- c(comb[i , ] , RVS[i] )

      RVsS[res[1]  , res[2] ] <- res[3]
      setTxtProgressBar(pb, i)
    }
  }else{
    RVsS <- matrix(data = NA, nrow = N , ncol = N)
    RVS <- numeric()

    for(i in 1:nrow(comb)){
      RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])

      res <- c(comb[i , ] , RVS[i] )

      RVsS[res[1]  , res[2] ] <- res[3]
    }
  }

  RVsS[lower.tri(RVsS)] = t(RVsS)[lower.tri(RVsS)]
  diag(RVsS) <- 1

  if(dist == TRUE){
    RVsS <- as.dist(1-RVsS)
  }
  return(RVsS)
}
