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
  kwb.utils::createDirectory(file.path(tempdir(), "dswt"))
}
