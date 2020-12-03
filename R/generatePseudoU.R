#' A pulpDistr function
#'
#' pseudoObs() takes as input a matrix or dataframe of observations
#' and returns a matrix of ranked pseudo-observations
#' [0, 1)^d where d is the number of columns in the input.
#'
#'  The observations are normalized such that the value 1 is never
#' returned, as this could generate some numerical difficulties
#' later on.
#'
#' @param observedSample Matrix or dataframe of observations
#' @keywords
#' @export
#' @examples
#' fitMarginFunction()
#'

pseudoObs <- function (observedSample){
  udata = matrix(0,dim(observedSample)[1],dim(observedSample)[2])

  n<-length(pulp_df[,1])
  for(i in 1:dim(pulp_df)[2] ){
    udata[,i]<-rank(pulp_df[,i])/(n+1)
  }
  return(pseudoObs)
}
