# dswtdir ----------------------------------------------------------------------

#' Path to Subfolder "DSWT" in tempdir()
#' 
#' Path to subfolder "DSWT" in tempdir(). If the subfolder does not yet exist
#'   it is created
#' 
#' @return path to subfolder "DSWT" in tempdir()
#' 
#' @export
#' 
dswtdir <- function()
{
  mydir <- file.path(tempdir(), "dswt")
  
  if (!file.exists(mydir)) {
    
    dir.create(mydir)
    cat(sprintf("directory \"%s\" created.\n", mydir))
  }
  
  mydir
}
