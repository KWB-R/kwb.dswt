# dirUploadedFiles -------------------------------------------------------------

#' List Uploaded Files
#' 
#' You need to set the system environment variable "DSWT_FTP_LOGIN" to "user:pwd"
#' where "user" is the username and "pwd" the password for the account that is
#' allowed to access the FTP server where the files are stored.
#' 
#' @param full.names if \code{TRUE} the full absolute URLs are returned, 
#'   otherwise only the relative paths.
#'   
#' @return list with elements \emph{PN}, \emph{H}, \emph{RD}, \emph{F},
#'   \emph{LPR}, \emph{Q}, \emph{BPR} containing URLs to sampler files, water
#'   level files, rain data files, photos, laboratory protocol files, discharge
#'   files and operation protocol files, respectively, that are available at the
#'   DSWT server at sysprovide.de
#'   
#' @export
#' 
dirUploadedFiles <- function(full.names = FALSE)
{
  url <- "ftp://srv1-18557.srv-net.de/html/uploaded_files"
  subdirs <- c("PN", "H", "RD", "F", "LPR", "Q", "BPR")
  
  filepaths <- file.path(url, subdirs, "")

  # Get user name and password from environment variable
  userpwd <- Sys.getenv()["DSWT_FTP_LOGIN"]

  stats::setNames(lapply(filepaths, dirFtpPath, userpwd, full.names), subdirs)
}

# dirFtpPath -------------------------------------------------------------------

#' List Files on FTP Server 
#' 
#' @param url base url in which to look for files
#' @param userpwd user and password, separated by colon ":"
#' @param full.names logical (default: \code{FALSE}). Determines whether to
#'   return relative paths or the full URLs including the base \code{url}
#'   
#' @return vector of urls or relative paths
#' 
#' @export
#' 
dirFtpPath <- function(url, userpwd, full.names = FALSE)
{
  if (! requireNamespace("RCurl", quietly = TRUE)) {
    
    stop(
      "Please install the package 'RCurl' first with ",
      "install.packages(\"RCurl\") in order to run dirFtpPath()", 
      call. = FALSE
    )
  }
  
  filenames <- RCurl::getURL(
    url, userpwd = userpwd, ftp.use.epsv = FALSE, dirlistonly = TRUE
  )
  
  filenames <- setdiff(strsplit(filenames, "\r?\n")[[1]], c(".", ".."))
  
  if (full.names) {
    
    file.path(sub("/$", "", url), filenames)
    
  } else {
    
    filenames
  }
}
