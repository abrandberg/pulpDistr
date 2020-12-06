#' Generate a new sample given a complete data set
#'
#' generatePulpSample() takes as input a dataframe
#'
#' @param pulpPath Path to the fiber characterization file
#' @param saveDirName Path to the place where the pre-computed copula is stored
#' @param outputName The name of the place where file is to be saved
#' @keywords
#' @export
#' @examples
#' generatePulpSample()
#'
generatePulpSample <- function(pulpPath,saveDirName,outputName){

  load(file = paste(saveDirName,".Rdata",sep = ""))


  Sim <- CDVineCondSim(RVM,condition)

  listOfModels = c("weibull","gamma","lnorm","norm","exp")
  print("Margin fit: Lc")
  fitLc      = fitMarginFunction(    pulp_cond$Lc, listOfModels, Sim[,3])
  print("Margin fit: Width")
  fitWidth   = fitMarginFunction( pulp_cond$Width, listOfModels, Sim[,4])
  print("Margin fit: Curl")
  fitCurl    = fitMarginFunction(  pulp_cond$Curl, listOfModels, Sim[,5])

  print("Conditional margin draw: WallTkn")
  fitWallTkn$marginDraw = drawFromMargin(fitWallTkn$marginGenerator,Sim[,1])
  print("Conditional margin draw: Fibrillation")
  fitFibril$marginDraw  = drawFromMargin(fitFibril$marginGenerator,Sim[,2])


  pulp_fit = matrix(nrow=length(conditioningData[,1]),ncol=5)
  pulp_fit[,3] = fitLc$marginDraw
  pulp_fit[,4] = fitWidth$marginDraw
  pulp_fit[,5] = fitCurl$marginDraw
  pulp_fit[,1] = fitWallTkn$marginDraw
  pulp_fit[,2] = fitFibril$marginDraw

  write.table(pulp_fit,file=outputName,quote = FALSE, sep = ",",eol = "\n",row.names = FALSE,col.names = FALSE)


}
