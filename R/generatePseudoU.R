#' A pulpDistr function
#'
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
