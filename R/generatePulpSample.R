#' Generate a new sample given a complete data set
#'
#' generatePulpSample() takes as input a dataframe
#'
#' @param pulpPath Path to the fiber characterization file
#' @param saveDirName Path to the place where the pre-computed copula is stored
#' @param outputName The name of the place where file is to be saved
#' @param numSamples The number of samples to generate
#' @keywords
#' @export
#' @examples
#' generatePulpSample()
#'
generatePulpSample <- function(pulpPath,saveDirName,outputName,numSamples){

  load(file = paste(saveDirName,".Rdata",sep = ""))


  Sim <- RVineSim(numSamples,RVM)

  print("Margin fit: Lc")
  fitLc$marginDraw = drawFromMargin(fitLc$marginGenerator,Sim[,3])
  print("Margin fit: Width")
  fitWidth$marginDraw = drawFromMargin(fitWidth$marginGenerator,Sim[,4])
  print("Margin draw: Curl")
  fitCurl$marginDraw = drawFromMargin(fitCurl$marginGenerator,Sim[,5])

  print("Conditional margin draw: WallTkn")
  fitWallTkn$marginDraw = drawFromMargin(fitWallTkn$marginGenerator,Sim[,1])
  print("Conditional margin draw: Fibrillation")
  fitFibril$marginDraw  = drawFromMargin(fitFibril$marginGenerator,Sim[,2])


  pulp_fit = matrix(nrow=numSamples,ncol=5)
  pulp_fit[,3] = fitLc$marginDraw
  pulp_fit[,4] = fitWidth$marginDraw
  pulp_fit[,5] = fitCurl$marginDraw
  pulp_fit[,1] = fitWallTkn$marginDraw
  pulp_fit[,2] = fitFibril$marginDraw

  write.table(pulp_fit,file=outputName,quote = FALSE, sep = ",",eol = "\n",row.names = FALSE,col.names = FALSE)


}
