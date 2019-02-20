# getLevelFilesForSite ---------------------------------------------------------

#' Level Files for Site

#' @param config configuration object (list) with elements "dictionaryFile" and
#'   elements required by \code{\link[kwb.monitoring]{pathDictionary}}
#' @param station name of monitoring station
#' 
#' @return vector of file paths
#' 
#' @export
#' 
getLevelFilesForSite <- function(config, station)
{
  config$station <- station
  
  dictionary <- kwb.monitoring::pathDictionary(
    dictionaryFile = config$dictionaryFile,
    settings = config
  )
  
  getDswtFilePaths(
    kwb.utils::resolve("LEVEL_DATA_DIR", dictionary),
    DSWT_FILE_TYPES()$RADAR_PROBE_H
  )
}

# getDswtFilePaths -------------------------------------------------------------

#' Get DSWT File Paths
#' 
#' Browse for files of given type (DSWT-specific)
#' 
#' @param srcdir source directory
#' @param filetype one of the file types contained in DSWT_FILE_TYPES
#' @param recursive search in subdirectories?
#' 
#' @return vector of file paths
#' 
#' @export
#' 
getDswtFilePaths <- function(srcdir, filetype, recursive = FALSE)
{
  srcfiles <- dir(
    srcdir, pattern = filetype$pattern, full.names = TRUE, recursive = recursive
  )
  
  if (kwb.utils::isNullOrEmpty(srcfiles)) {
    
    warning(sprintf(
      "No files matching pattern '%s' found in %s\n", filetype$pattern, srcdir
    ))
    
    return()
  }
  
  srcfiles
}
