#' Conditional sampling
#'
#' conditionalSamplingPulp() takes as input a dataframe
#'
#' @param saveDirName Path to the place where previous characterization was saved
#' @param conditioningData New data which does not contain the wall thickness or external fibrillation
#' @keywords
#' @export
#' @examples
#' conditionalSamplingPulp()
#'
conditionalSamplingPulp <- function(saveDirName,pulp_cond,pulp_raw){

  idxToRetain = length(pulp_cond[,1])
  conditioningCombined = do.call(rbind, list(pulp_cond, pulp_raw))

  conditioningData = generatePseudoU(pulp_cond)

  conditioningData = conditioningData[1:idxToRetain,]



  load( file = paste(saveDirName,".Rdata",sep = ""))
  # Load previously determined data

  d=dim(RVM$Matrix)[1]

  condition = conditioningData[, cbind(RVM$Matrix[(d+1)-1,(d+1)-1] , RVM$Matrix[(d+1)-2,(d+1)-2] , RVM$Matrix[(d+1)-3,(d+1)-3])-2]
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
  pulp_fit[,1] = fitLc$marginDraw
  pulp_fit[,2] = fitWidth$marginDraw
  pulp_fit[,3] = fitCurl$marginDraw
  pulp_fit[,4] = fitWallTkn$marginDraw
  pulp_fit[,5] = fitFibril$marginDraw


  characterizedPulp = list()
  characterizedPulp$fitData = pulp_fit
  return(characterizedPulp)
}
