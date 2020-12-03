#' A pulpDistr function
#'
#' characterizePulp() takes as input a dataframe
#'
#' @param pulpPath Path to the fiber characterization file
#' @param saveDirName Path to the place where files are saved
#' @keywords
#' @export
#' @examples
#' characterizePulp()
#'
characterizePulp <- function(pulpPath,saveDirName){
  ##########################################################################################
  # Data import and file restructuring
  #
  saveDirName = gsub("[\\]","/",saveDirName)

  pulp_raw <- read.csv(pulpPath, header = FALSE, sep = '\t', dec = '.')
  # Load and preprocess the pulp data

  pulp_raw <- pulp_raw[ c(3 ,4, 5, 6, 16)]
  pulp_raw <- pulp_raw[ c(1, 4, 5, 2, 3) ]
  # Remove columns that are not of interest. Retain length Lc, Width, WallTkn and Curl

  names(pulp_raw) <- c("Lc","Curl","Fibril","Width","Wall")
  # Assign names to the data frame

  pulp_raw$Curl <- pulp_raw$Curl/100
  # Standard format of Curl

  pulp_raw = pulp_raw[c(5,3,1,4,2)]
  # Wall Fibril Lc Width Curl
  # Swap order to get conditioning variables at the end


  udata = generatePseudoU(pulp_raw)
  # Generate pseudo-observations



  ##########################################################################################
  condVars = 3
  copType = "CVine"
  selMethod = "AIC"
  checkIndep = TRUE
  fitSet = c(0,1)#NA

  print("CDVineCondFit input options:")
  print(paste("Nx =",condVars))
  print(paste("type =",copType))
  print(paste("selectioncrit =",selMethod))
  print(paste("indeptest =",checkIndep))
  print(paste("familyset =",fitSet))

  RVM <- CDVineCondFit(udata,Nx=condVars, type=copType, selectioncrit=selMethod,
                       indeptest=checkIndep, level=0.05, familyset = fitSet)
  # Fit Copula



  print("******************** Margin fits ********************")
  listOfModels = c("weibull","gamma","lnorm","norm","exp")
  print("Margin fit: Lc")
  fitLc      = fitMarginFunction(    pulp_raw$Lc, listOfModels, Sim[,3])
  print("Margin fit: Width")
  fitWidth   = fitMarginFunction( pulp_raw$Width, listOfModels, Sim[,4])
  print("Margin fit: Curl")
  fitCurl    = fitMarginFunction(  pulp_raw$Curl, listOfModels, Sim[,5])
  print("Margin fit: WallTkn")
  fitWallTkn = fitMarginFunction(  pulp_raw$Wall, listOfModels, Sim[,1])
  print("Margin fit: Fibrillation")
  fitFibril  = fitMarginFunction(pulp_raw$Fibril, listOfModels, Sim[,2])



  save(list = "RVM",file = paste(saveDirName,".Rdata",sep = ""))
  # Save the RVM for future use.

  save(RVM,fitLc,fitWidth,fitCurl,fitWallTkn,fitFibril,file = paste(saveDirName,".Rdata",sep = ""))
  # Here we should allow for the files to be saved as well.

  characterizedPulp$rawData = pulp_raw
  return(characterizedPulp)
}




