# .siteNameToDN ----------------------------------------------------------------

#' Find DN for Given Site
#' 
#' @param sitename name of monitoring site
#' 
#' @export
#' 
.siteNameToDN <- function(sitename) {
  
  if (kwb.utils::isNullOrEmpty(sitename)) {
    stop("No sitename given!")
  }
  
  DNs <- list(T = 300, C = 150)
  
  sitegroupCode <- substr(sitename, 1, 1)
  
  if (!(sitegroupCode %in% names(DNs))) {
    stop("No DN given for sitename: ", sitename)
  }
  
  DN <- DNs[[sitegroupCode]]
  
  cat("DN at site", sitename, ":", DN, "\n")
  
  DN
}
