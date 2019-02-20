# readAllLevelFiles ------------------------------------------------------------

#' Read all Level Files
#' 
#' @param levelFiles vector of file paths
#' @param dbg if \code{TRUE}, debug messages are shown.
#' 
#' @export
#' 
readAllLevelFiles <- function(levelFiles, dbg = TRUE)
{
  levelData <- NULL
  
  for (levelFile in levelFiles) {
    
    if (dbg) {
      cat("Reading", levelFile, "...\n")
    }
    
    newLevelData <- kwb.logger::readLogger_Ori_MLog(
      levelFile, sep = ";", stopOnMissingColumns = FALSE)
    
    newLevelData <- data.frame(
      file = basename(levelFile),
      row = 1:nrow(newLevelData),
      newLevelData,
      stringsAsFactors = FALSE)
    
    if (dbg) {
      cat("- Columns:", paste(kwb.utils::hsQuoteChr(names(newLevelData))), "\n")
      cat("ok.\n")
    }
    
    levelData <- kwb.utils::safeRowBind(levelData, newLevelData)
  }
  
  levelData
}
