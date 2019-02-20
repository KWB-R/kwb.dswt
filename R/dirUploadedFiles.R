# dirUploadedFiles -------------------------------------------------------------

#' List Uploaded Files
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
  names(filepaths)   <- subdirs
  
  uploadedFiles <- list()
  
  for (subdir in subdirs) {
    
    filenames <- dirFtpPath(
      filepaths[subdir], 
      userpwd = Sys.getenv()["DSWT_FTP_LOGIN"],
      full.names
    )
    
    uploadedFiles[[subdir]] <- filenames
  }
  
  uploadedFiles
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
    stop("Please install the package 'RCurl' first with ",
         "install.packages(\"RCurl\") in order to run dirFtpPath()")
  }
  
  filenames <- RCurl::getURL(url, userpwd = userpwd, ftp.use.epsv = FALSE,
                             dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")[[1]]
  filenames <- setdiff(filenames, c(".", ".."))
  
  if (full.names) {
    filenames <- file.path(sub("/$", "", url), filenames)
  }
  
  filenames
}
