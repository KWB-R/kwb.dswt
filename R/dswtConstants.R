# DSWT_DICTIONARY_FILE ---------------------------------------------------------

#' Path to Default Path Dictionary File
#' 
#' Default "dictionary" file describing the folder structure to be used in DSWT
#' 
DSWT_DICTIONARY_FILE <- function()
{
  system.file("extdata", "dswtPathDictionary.txt", package = "kwb.dswt")
}

# DSWT_H_OFFSETS ---------------------------------------------------------------

#' Water Level Offsets to be Subtracted from Measurement
#'
#' Water level offsets H_offset to be subtracted from measured level in order to
#' get the water level above the plume: H = H_raw - H_offset. H is then used to
#' calculate Q by using the relationship given by \code{\link{H_Q_Table}}
#'
#' @return list of named elements with names corresponding to the monitoring
#'   site and the value corresponding to the offest in metres.
#'   
DSWT_H_OFFSETS <- function()
{
  list(
    T_M1 = 0.02, 
    C_M1 = 0.012,
    C_M2 = 0.011,
    C_M3 = 0.0,
    C_M4 = 0.0,
    C_M5 = 0.011,
    C_M6 = 0.01
  )
}

# DSWT_H_OFFSETS_SINCE ---------------------------------------------------------

#' DSWT_H_OFFSETS_SINCE
DSWT_H_OFFSETS_SINCE <- function() 
{
  offsetFile <- system.file(
    "extdata", "dswt_h_offsets_cm.csv", package = "kwb.dswt"
  )
  
  read.table(offsetFile, sep = ";", header = TRUE, fill = TRUE)
}

# DSWT_FILE_TYPES --------------------------------------------------------------

#' List of File Type Definitions 
#' 
#' Containing e.g. a file name pattern
#' 
DSWT_FILE_TYPES <- function()
{
  to_pattern <- function(x) list(pattern = x)
  
  list (  
    RAIN_DOWNLOAD_FROM_DSWT_SERVER = 
      to_pattern("^\\d{4}\\-.*\\.csv$"),
    RAIN_PER_DAY_FROM_BWB_1 = 
      to_pattern("__BERICHT\\.csv$"),
    RAIN_PER_DAY_FROM_BWB_2 = 
      to_pattern("__BERICHT_\\(2\\)\\.csv$"),
    RAIN_PER_5_MIN_FROM_BWB = 
      to_pattern("Regenschreiberdaten\\-(2013|2014)\\-Q[1-4]\\.xlsx$"),
    AUTO_SAMPLER_PN = 
      to_pattern("^PN.*\\.csv$"),
    RADAR_PROBE_H = 
      to_pattern("^H_(\\d{4})(\\d{2})(\\d{2})_(C|T)_(M[1-6])\\.csv$")  
  )  
}

# DSWT_BWB_CODE_TO_SITE_CODE ---------------------------------------------------

#' DSWT_BWB_CODE_TO_SITE_CODE
DSWT_BWB_CODE_TO_SITE_CODE <- function() 
{
  # T = Treffurter Str., C = Clayallee 
  list(
    Mal = "T_Mal",  
    Hsch = "T_Hsch", 
    Lbg = "T_Lbg", 
    MarI = "T_Mar1", 
    BieI = "T_Bie", 
    Stg = "C_Stg", 
    Wil = "C_Wil",  
    Wila = "C_Wila", 
    ZhlI = "C_Zhl1"
  ) 
}

# DSWT_RAIN_GAUGES -------------------------------------------------------------

#' DSWT_RAIN_GAUGES
#' @return data frame with columns \emph{FUB_STATION}, \emph{FUB_SHORT}, 
#'   \emph{BWB_SHORT}       
#' 
DSWT_RAIN_GAUGES <- function()
{
  bwbShortNames <- c(
    "Hsch", "Mar I", "Mal", "Bie I", "Lbg", # Treffurter Str. 
    "Stg",  "Wil", "Wil a", "Zhl I"         # Clayallee
  )
  
  bwbRainGauges <- BWB_RAIN_GAUGES()
  bwbRainGauges[bwbRainGauges$BWB_SHORT %in% bwbShortNames, ]
}

# DSWT_SITES -------------------------------------------------------------------

#' DSWT_SITES
#' 
#' @return named vector of SiteID values in ODM database
#' 
DSWT_SITES <- function() 
{
  to_site <- function(x) list(SiteID = x)
  
  list(
    T_M1 = to_site(1),
    
    T_Hsch = to_site(27),
    T_Mar1 = to_site(26),
    T_Mal = to_site(28),
    T_Bie = to_site(24),
    T_Lbg = to_site(25),

    C_M1 = to_site(8),
    C_M2 = to_site(9),
    C_M3 = to_site(10),
    C_M4 = to_site(11),
    C_M5 = to_site(12),
    C_M6 = to_site(13),
    
    C_Stg = to_site(20),
    C_Wil = to_site(21),
    C_Wila = to_site(22),
    C_Zhl1 = to_site(23)
  )
}

# DSWT_TIMESERIES --------------------------------------------------------------

#' DSWT_TIMESERIES
DSWT_TIMESERIES <- function()
{
  list(
    
    LEVEL_TUB_RAW = list(
      MethodID = 64,  # Wasserstand_Wasserstandsmessung aus Radarsonde/mlog
      VariableID = 2, # Wasserhoehe 
      SourceID = 2,   # TUB
      QualityControlLevelID = 0 # Rohdaten
    ),
    
    FLOW_TUB_RAW = list(
      MethodID = 65,  # Errechnet auf Basis von Wasserhoehe_Durchfluss
      VariableID = 4, # Durchfluss
      SourceID = 2,   # TUB
      QualityControlLevelID = 0 # Rohdaten
    ),
    
    RAIN_FUB_RAW = list(
      MethodID = 70,  # von FUB/BWB, mm/h_Regendaten
      VariableID = 3, # Regenmenge
      SourceID = 5,   # FUB
      QualityControlLevelID = 0 # Rohdaten
    ),

    RAIN_BWB5min_RAW = list(
      MethodID = 60,  # BWB, mm/(5min)_Regendaten
      VariableID = 3, # Regenmenge
      SourceID = 1,   # BWB
      QualityControlLevelID = 0 # Rohdaten
    )
  )
}

# keyFields_DSWT ---------------------------------------------------------------

#' Key Field Values in DSWT Project
#' 
keyFields_DSWT <- function()
{
  dswtTimeseries <- DSWT_TIMESERIES()
  dswtSites <- DSWT_SITES()
  
  list(
    # Treffurter Strasse - Messschacht 1
    T_M1_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$T_M1),
    T_M1_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$T_M1),
               
    # Clayallee - Messschacht 1
    C_M1_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$C_M1),
    C_M1_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$C_M1),
    
    # Clayallee - Messschacht 2
    C_M2_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$C_M2),
    C_M2_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$C_M2),

    # Clayallee - Messschacht 3
    C_M3_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$C_M3),
    C_M3_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$C_M3),
    
    # Clayallee - Messschacht 4
    C_M4_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$C_M4),
    C_M4_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$C_M4),
    
    # Clayallee - Messschacht 5
    C_M5_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$C_M5),
    C_M5_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$C_M5),

    # Clayallee - Messschacht 6
    C_M6_H = c(dswtTimeseries$LEVEL_TUB_RAW, dswtSites$C_M6),
    C_M6_Q = c(dswtTimeseries$FLOW_TUB_RAW, dswtSites$C_M6),

    # Treffurter Strasse - RS Hsch
    T_Hsch_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Hsch),

    # Treffurter Strasse - RS Mar1
    T_Mar1_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Mar1),
    
    # Treffurter Strasse - RS Mal
    T_Mal_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Mal),
                     
    # Treffurter Strasse - RS Bie
    T_Bie_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Bie),
    
    # Treffurter Strasse - RS Lbg
    T_Lbg_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Lbg),
    
    # Clayallee - RS Stg
    C_Stg_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$C_Stg),
    
    # Clayallee - RS Wil
    C_Wil_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$C_Wil),
    
    # Clayallee - RS WilA
    C_Wila_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$C_Wila),
    
    # Clayallee - RS Zhl1
    C_Zhl1_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$C_Zhl1)
  )
}
