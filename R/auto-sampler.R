# readDswtSamplerFileByName ----------------------------------------------------

#' Read File from Autosampler Used in Project DSWT
#' @keywords internal
readDswtSamplerFileByName <- function(samplerFile, bottlesToConsider)
{
  kwb.utils::catAndRun(
    sprintf("Reading sample data from \"%s\"", basename(samplerFile)), 
    expr = {
      sampleData.samples <- kwb.logger::readLogger_Ori_BasicEx1(
        samplerFile, infotype = "times"
      )
      sampleData.actions <- kwb.logger::readLogger_Ori_BasicEx1(
        samplerFile, infotype = "actions"
      )
    }
  )

  if (kwb.utils::isNullOrEmpty(sampleData.samples)) {
    
    warning(sprintf(
      "\n\n*** No samples in %s! The file is skipped.\n", basename(samplerFile)
    ))
    
    return (NULL)
  }

  # remove empty lines and trim action names
  time_available <- ! is.na(sampleData.actions$myDateTime)
  sampleData.actions <- sampleData.actions[time_available, ]
  sampleData.actions$Ereignis <- kwb.utils::hsTrim(sampleData.actions$Ereignis)
  sampleData.actions$Flasche <- kwb.utils::hsTrim(sampleData.actions$Flasche)

  # remove lines before 2013...
  sampleData.samples <- removeLinesBeforeYear(sampleData.samples, 2013)
  sampleData.actions <- removeLinesBeforeYear(sampleData.actions, 2013)

  sampleData.samples <- kwb.utils::removeColumns(
    sampleData.samples, c("Akku", "V9", "V10")
  )

  # identify sample events that are directly followed by the error "no water"
  rows <- grep("Kein Wasser", sampleData.actions$Ereignis)
  sampleData.noWater <- sampleData.actions[rows, ]

  noWaterIndexToSampleIndex <- function(i) {

    secondsBefore <-
      as.integer(kwb.datetime::hsToPosix(sampleData.noWater$myDateTime[i])) -
      as.integer(kwb.datetime::hsToPosix(sampleData.samples$myDateTime))

    row.sample <- which(
      secondsBefore >= 0
      & secondsBefore < 10
      & as.integer(sampleData.samples$Flasche)
      == as.integer(sampleData.noWater$Flasche[i])
    )

    ifelse(length(row.sample) > 0, max(row.sample), -1)
  }

  sampleIndices.noWater <- sapply(
    seq_len(nrow(sampleData.noWater)), FUN = noWaterIndexToSampleIndex
  )

  sampleIndices.noWater <- setdiff(sampleIndices.noWater, -1)

  sampleData.samples$result <- "SUCCESS"

  if (! kwb.utils::isNullOrEmpty(sampleIndices.noWater)) {
    
    sampleData.samples$result[sampleIndices.noWater] <- "NO WATER"
  }

  # sampleData$unit <- "ml"
  # sampleData$sample <- 1:nrow(sampleData)

  sampleDataExtended <- cbind(
    samplerFile = basename(samplerFile),
    sampleData.samples,
    stringsAsFactors = FALSE
  )

  sampleDataExtended <- kwb.utils::renameColumns(sampleDataExtended, list(
    myDateTime = "sampleTime",
    Flasche = "bottle",
    Fuellmenge = "volume"
  ))
  
  # filter for relevant bottles
  if (! all(is.na(bottlesToConsider))) {
    
    indices <- which(sampleDataExtended$bottle %in% bottlesToConsider)

    if (kwb.utils::isNullOrEmpty(indices)) {
      
      stop(sprintf(
        "No of the existing bottles (%s) are to be considered (%s).",
        kwb.utils::commaCollapsed(sampleDataExtended$bottle),
        kwb.utils::commaCollapsed(bottlesToConsider)))
    }

    sampleDataExtended <- sampleDataExtended[indices, ]
  }

  sampleDataExtended
}

# removeLinesBeforeYear --------------------------------------------------------

#' Remove Lines Before Year
#' @keywords internal
removeLinesBeforeYear <- function(sampleData, year)
{
  before <- which(as.integer(substr(sampleData$myDateTime, 1, 4)) < year)

  if (! kwb.utils::isNullOrEmpty(before)) {
    
    message(sprintf(
      "\n*** %d lines with dates before %d were removed:\n", 
      length(before), year
    ))
    
    print(sampleData[before, ])

    warning(sprintf(
      "%d lines with dates before %d were removed (see above).", 
      length(before), year
    ))
    
    sampleData <- sampleData[-before, ]
  }

  sampleData
}

# readAndPlotAutoSamplerFiles --------------------------------------------------

#' Read and Plot Autosampler Files
#' 
#' @param filePaths full path(s) to ORI Auto sampler log files
#'   PN_<yyyymmdd>_<station>.csv
#' @param removePattern regular expression pattern matching logged actions to be
#'   removed before plotting. Set to "" in order not to remove any action
#' @param to.pdf if TRUE, graphical output is directed to PDF
#' @param evtSepTime event separation time in seconds. Minimum time of 
#'   "no signal" between two distinct events.
#' @export
readAndPlotAutoSamplerFiles <- function(
  filePaths,
  removePattern = "Power|Bluetooth|Modem|SMS|Sonde",
  to.pdf = TRUE,
  evtSepTime = 30 * 60
)
{
  # Read all auto sampler actions from all files
  all.actions <- getActionsFromAutoSamplerFiles(filePaths)

  # Remove actions of a certain type
  if (removePattern != "") {

    actions.to.remove <- grep(
      removePattern, unique(all.actions$Ereignis), value = TRUE
    )

    actions <- all.actions[! all.actions$Ereignis %in% actions.to.remove, ]
  }

  # Plot all auto sampler actions
  plotAllAutoSamplerActions(actions, to.pdf = to.pdf, evtSepTime = evtSepTime)
}

# getActionsFromAutoSamplerFiles -----------------------------------------------

#' Get Actions from Autosampler Files
#' 
#' @param pnFiles full path(s) to ORI Auto sampler log files
#'   PN_<yyyymmdd>_<station>.csv
#' 
getActionsFromAutoSamplerFiles <- function(pnFiles)
{
  all.actions <- NULL

  for (i in seq_along(pnFiles)) {
    
    actions <- getActionsFromAutoSamplerFile(pnFiles[i], i)
    
    all.actions <- rbind(all.actions, actions)
  }

  all.actions
}

# getActionsFromAutoSamplerFile ------------------------------------------------

#' Get Actions from Autosampler File
#' 
#' @param pnFile full path to ORI Auto sampler log files
#'   PN_<yyyymmdd>_<station>.csv
#' @param fileNumber file number (will be included in the plot title). Useful if
#'   this function is called in a sequence for multiple files.
#' @param remove.errors if TRUE, actions containing "Fehler" are removed
#' 
getActionsFromAutoSamplerFile <- function(
  pnFile, fileNumber = 1, remove.errors = FALSE
)
{
  stopifnot(length(pnFile) == 1)

  kwb.utils::catAndRun(sprintf("Reading file %d: %s", fileNumber, pnFile), {
    actions <- kwb.logger::readLogger_Ori_BasicEx1(pnFile, infotype = "actions")
  })
  
  # Remove rows without time
  actions <- actions[! is.na(actions$myDateTime), ]

  if (kwb.utils::isNullOrEmpty(actions)) {
    warning(sprintf("No actions in %s.", pnFile))
    return(NULL)
  }

  # Remove errors (if required)
  if (remove.errors) {
    actions <- actions[! grepl("Fehler", actions$Ereignis), ]
    if (kwb.utils::isNullOrEmpty(actions)) {
      warning(sprintf("No non-error actions in %s.", pnFile))
      return(NULL)
    }
  }

  # Convert timestamps to POSIXct
  actions$myDateTime <- kwb.datetime::hsToPosix(actions$myDateTime)

  # Look for times before 2013
  invalid <- which(as.POSIXlt(actions$myDateTime)$year + 1900 < 2012)

  if (! kwb.utils::isNullOrEmpty(invalid)) {
    
    warning(
      sprintf("There are invalid timestamps (before 2012) in %s\n", pnFile),
      "  that were removed:\n  ",
      paste(actions$myDateTime[invalid], collapse = "\n  ")
    )
    
    actions <- actions[-invalid, ]
  }

  # order by timestamps and give a warning if actions were not ordered
  timeOrder <- order(actions$myDateTime)
  
  if (any(timeOrder != seq_len(nrow(actions)))) {
    
    warning(
      sprintf("Not all actions were ordered by time in %s\n", pnFile),
      "  and have been ordered."
    )
    
    actions <- actions[timeOrder, ]
  }

  actions$Ereignis <- kwb.utils::hsTrim(actions$Ereignis)

  # Define required columns
  columns <- c(
    "myDateTime", "Ereignis", "Fuellmenge", "Flasche", "Probe", "Akku"
  )

  actions <- kwb.utils::hsAddMissingCols(actions, columns)

  # in case of Probe: add bottle number
  indices.sample <- which(actions$Ereignis == "Probe")

  actions$Ereignis[indices.sample] <- sprintf(
    "Probe: Fl. %s", kwb.utils::hsTrim(actions$Flasche[indices.sample])
  )

  actions <- actions[, columns]

  actions$file <- basename(pnFile)

  actions
}

# plotAllAutoSamplerActions ----------------------------------------------------

#' Plot all Autosampler Actions
#' @keywords internal
plotAllAutoSamplerActions <- function(
  all.actions, to.pdf = TRUE, group.size = 6, evtSepTime = 30 * 60
)
{
  PDF <- kwb.utils::preparePdfIf(to.pdf, landscape = TRUE)
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, PDF))

  pnFiles <- unique(all.actions$file)

  numberOfFiles <- length(pnFiles)

  for (i in seq_len(numberOfFiles)) {
    
    kwb.utils::catAndRun(
      messageText = sprintf(
        "Plotting events of sampler file %d / %d : %s", 
        i, numberOfFiles, pnFiles[i]
      ), 
      expr = {
        actions.in.file <- all.actions[all.actions$file == pnFiles[i], ]
        plotAutoSamplerActions(
          actions.in.file, 
          fileNumber = i, 
          group.size = group.size,
          evtSepTime = evtSepTime
        )
      }
    )
  }
}

# plotAutoSamplerActions -------------------------------------------------------

#' Plot Autosampler Actions
#' @keywords internal
plotAutoSamplerActions <- function(
  actions, fileNumber, subevents.per.page = 3, density = 20, 
  context = c(0, 0.12), evtSepTime = 30 * 60, ...
)
{
  graphicalParameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(graphicalParameters))

  fileName <- unique(actions$file)
  stopifnot(length(fileName) == 1)

  subevents <- kwb.event::hsEvents(
    actions$myDateTime, evtSepTime = evtSepTime, signalWidth = 1
  )

  #rowsPerPage <- min(c(subevents.per.page, nrow(subevents))) + 1
  rowsPerPage <- subevents.per.page + 1

  top.plot.height.cm <- 4 # 3
  
  graphics::layout(
    matrix(seq_len(rowsPerPage), ncol = 1), 
    heights = c(graphics::lcm(top.plot.height.cm))
  )

  numberOfSubevents <- nrow(subevents)

  for (i in seq_len(numberOfSubevents)) {

    # Repeat overview on each new page
    if ((i - 1) %% subevents.per.page == 0) {
      
      graphics::par(mar=c(3,3,4,1))

      y1 <- 0
      y2 <- 1

      lastIndexOnPage <- min(numberOfSubevents, i+subevents.per.page-1)

      kwb.event::ganttPlotEvents(
        subevents, xlab = "", title = "Intervalle:", leftMargin = 0.05,
        density = density, ylim = c(0, 2), y1 = y1, y2 = y2, yLabel = 1.5,
        indicate=i:lastIndexOnPage, indicationColour = "red"
      )
      
      at <- pretty(actions$myDateTime, 12)
      graphics::axis(1, at = at, labels = format(at, format = "%d.%m. %H:%M"))

      graphics::title(sprintf(
        "Sampler-Datei #%d (%s), Intervalle %d - %d von %d",
        fileNumber, fileName,
        i, lastIndexOnPage, numberOfSubevents
      ))
      
      graphics::par(mar = c(3, 3, 0, 1))
    }

    actionsInEvent <- kwb.event::hsGetEvent(actions, subevents, i)

    if (kwb.utils::isNullOrEmpty(actionsInEvent)) {
      
      print(actions)
      print(subevents[i, ])
      stop("no actions in subevent!")
    }

    x <- actionsInEvent$myDateTime

    labelInfo <- logEventToLabelInfo(actionsInEvent$Ereignis, x)

    limits <- kwb.event::eventLimits(subevents[i, ], context=context)

    # Create plot area (empty plot)
    plot.args <- list(
      x = x, 
      y = rep(NA, length(x)), 
      xlim = c(limits$tBeg, limits$tEnd),
      ylim = c(-1, 1), 
      type = "n", # "h", 
      xaxt = "n", 
      yaxt = "n",
      xlab = "", 
      ylab = ""
    )

    do.call(graphics::plot, plot.args)

    kwb.plot::addTimeAxis(x, time.format = "%H:%M", add.grid = TRUE)

    # title to the left
    graphics::mtext(sprintf("Intervall %d", i), side = 2, line = 1)

    # indicate subevent limits (red dashed)
    graphics::abline(
      v = kwb.event::eventToXLim(subevents[i, ]), col = "red", lty = 2
    )

    # labelling
    kwb.plot::addLabels(
      x, labels = labelInfo$labels, alternating = TRUE, col = labelInfo$colours,
      col.line = "black", lty = 1, lty.horiz.line = 1, # plot horizontal line
      bandheight = 0.4, group.size = 6
    )
  }
}

# logEventToLabelInfo ----------------------------------------------------------

#' Create Label and Colour Information from Event
#' 
#' @return list with elements \emph{labels} and \emph{colours}
#' @keywords internal
logEventToLabelInfo <- function(
  ereignis, x, colour.error = "red", colour.sample = "blue"
)
{
  indices.sample <- grep("Probe", ereignis)
  indices.error <- grep("Fehler", ereignis)

  ereignis[indices.sample] <- sprintf(
    "%s (%s)", 
    ereignis[indices.sample], format(x[indices.sample], format = "%H:%M")
  )

  textColours <- rep("black", length(ereignis))

  textColours[indices.error] <- colour.error
  textColours[indices.sample] <- colour.sample

  list(labels = ereignis, colours = textColours)
}
