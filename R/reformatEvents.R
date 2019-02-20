# reformatEvents ---------------------------------------------------------------

#' Reformat Event List
#' 
#' Reformat event list: convert to minutes and rename columns
#' 
#' @param events event list as retrieved by \code{kwb.event::hsEvents}
#' 
#' @return \emph{events} with \emph{tBeg} renamed \emph{Ereignisbeginn_UTC},
#'   \emph{tEnd} renamed \emph{Ereignisende_UTC}, \emph{dur} renamed
#'   \emph{Dauer_min}, \emph{pBefore} renamed \emph{Pause_davor_min} and
#'   \emph{pAfter} renamed \emph{Pause_danach_min} and original columns
#'   \emph{iBeg} and \emph{iEnd} removed
#' 
#' @export
#' 
reformatEvents <- function(events)
{
  events <- kwb.event::hsEventsToUnit(events, tUnit = "min")
  
  events <- kwb.utils::hsRenameColumns(events, list(
    tBeg = "Ereignisbeginn_UTC",
    tEnd = "Ereignisende_UTC",
    dur = "Dauer_min",
    pBefore = "Pause_davor_min",
    pAfter = "Pause_danach_min"
  ))
  
  cbind(Nr = 1:nrow(events), events[, -(1:2)])
}
