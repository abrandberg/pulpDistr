#' File input and restructuring
#'
#' readAndFormatPulpFile() takes as input a path to a text file containing the raw fiber network,
#' and massages the shape so that it fits what the rest of the package expects
#'
#' @param pulpPath Path to the fiber characterization file
#' @param
#' @keywords
#' @export
#' @examples
#' readAndFormatPulpFile()
#'
readAndFormatPulpFile <- function(pulpPath){
  ##########################################################################################
  # Data import and file restructuring
  #


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

  return(pulp_raw)

}
