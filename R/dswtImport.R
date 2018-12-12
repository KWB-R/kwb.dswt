# prepareSingleVariableDataValuesForOdm ----------------------------------------
prepareSingleVariableDataValuesForOdm <- function # prepareSingleVariableDataValuesForOdm
### prepareSingleVariableDataValuesForOdm
(
  dataFrame, colName, noDataValue = -9999, dbg = TRUE
)
{
  kwb.utils::printIf(dbg, head(dataFrame))

  columnNames <- c("BerlinDateTimeNoDST", colName)

  kwb.utils::checkForMissingColumns(dataFrame, columnNames, do.stop = TRUE)

  dataValues <- dataFrame[, columnNames]

  kwb.utils::printIf(dbg, head(dataValues))

  names(dataValues)[2] <- "DataValue"

  # replace NA with noDataValue
  dataValues$DataValue[is.na(dataValues$DataValue)] <- noDataValue

  kwb.utils::printIf(dbg, head(dataValues))

  dataValues <- insertLocalDateTimeColumns(dataValues)

  kwb.utils::printIf(dbg, head(dataValues))

  dataValues <- insertUtcDateTimeColumn(dataValues)

  kwb.utils::printIf(dbg, head(dataValues))

  dataValues <- kwb.utils::hsRenameColumns(
    dataValues,
    list(BerlinDateTime = "LocalDateTime")
  )

  kwb.utils::printIf(dbg, head(dataValues))

  dataValues <- dataValues[, names(dataValues) != "BerlinDateTimeNoDST"]

  kwb.utils::printIf(dbg, head(dataValues))

  dataValues
}

