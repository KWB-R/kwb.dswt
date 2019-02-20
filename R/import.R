# prepareSingleVariableDataValuesForOdm ----------------------------------------

#' Prepare Single Variable Data Values for ODM
#' 
#' @param dataFrame data frame containing the data
#' @param colName column name
#' @param noDataValue value indicating "no data", default: -9999
#' @param dbg logical. If \code{TRUE}, debug messages are shown
#' 
#' @export
prepareSingleVariableDataValuesForOdm <- function(
  dataFrame, colName, noDataValue = -9999, dbg = TRUE
)
{
  kwb.utils::printIf(dbg, utils::head(dataFrame))

  columnNames <- c("BerlinDateTimeNoDST", colName)

  dataValues <- kwb.utils::selectColumns(dataFrame, columnNames)

  kwb.utils::printIf(dbg, utils::head(dataValues))

  names(dataValues)[2] <- "DataValue"

  # replace NA with noDataValue
  dataValues$DataValue[is.na(dataValues$DataValue)] <- noDataValue

  kwb.utils::printIf(dbg, utils::head(dataValues))

  dataValues <- insertLocalDateTimeColumns(dataValues)

  kwb.utils::printIf(dbg, utils::head(dataValues))

  dataValues <- insertUtcDateTimeColumn(dataValues)

  kwb.utils::printIf(dbg, utils::head(dataValues))

  dataValues <- kwb.utils::renameColumns(dataValues, list(
    BerlinDateTime = "LocalDateTime"
  ))

  kwb.utils::printIf(dbg, utils::head(dataValues))

  dataValues <- dataValues[, names(dataValues) != "BerlinDateTimeNoDST"]

  kwb.utils::printIf(dbg, utils::head(dataValues))

  dataValues
}
