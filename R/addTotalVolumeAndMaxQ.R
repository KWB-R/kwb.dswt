# addTotalVolumeAndMaxQ --------------------------------------------------------

#' Add Total Volume and Max Q
#' 
#' @param qValues vector of discharge values given in L/s
#' @param events event information as retrieved by \code{kwb.event::hsEvents}
#' @param eventnr integer vector of same length as \emph{qValues} giving the
#'   number of the event to which the Q value belongs, as returned by
#'   \code{kwb.event::hsEventNumber}.
#' @param digitsV number of decimal places for V in m3
#' @param digitsMaxQ number of decimal places for max. Q in L/s
#'
#' @return \emph{events} with columns \emph{V_m3} and \emph{maxQ_L_s} added
#' 
#' @export
#' 
addTotalVolumeAndMaxQ <- function(
  qValues, events, eventnr, digitsV = 3, digitsMaxQ = 3
)
{
  # calculate total volume and max flow per event
  myby <- list(eventnr=eventnr)
  qsum <- stats::aggregate(qValues, by=myby, FUN=sum)
  qmax <- stats::aggregate(qValues, by=myby, FUN=max)
  
  signalWidth <- kwb.event::hsSigWidth(events)
  cat(sprintf(
    "A signal width of %d seconds was deduced from the event list.\n",
    signalWidth
  ))
  
  events$V_m3 <- round(qsum$x / 1000 * signalWidth, digitsV)
  events$maxQ_L_s <- round(qmax$x, digitsMaxQ)
  
  events
}
