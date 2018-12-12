# DSWT_DICTIONARY_FILE ---------------------------------------------------------
DSWT_DICTIONARY_FILE <- function # default folder "dictionary" file
### default "dictionary" file describing the folder structure to be used in DSWT
(
)
{
  system.file("extdata", "dswtPathDictionary.txt", package = "kwb.dswt")
}

# DSWT_H_OFFSETS ---------------------------------------------------------------
DSWT_H_OFFSETS <- function # Water level offsets to be subtracted from measurment
### Water level offsets H_offset to be subtracted from measured level in order
### to get the water level above the plume: H = H_raw - H_offset. H is then used
### to calculate Q by using the relationship given by \code{\link{H_Q_Table}}
()
{
  list(T_M1 = 0.02, 
       C_M1 = 0.012,
       C_M2 = 0.011,
       C_M3 = 0.0,
       C_M4 = 0.0,
       C_M5 = 0.011,
       C_M6 = 0.01)
  ### list of named elements with names corresponding to the monitoring site and
  ### the value corresponding to the offest in metres.
}

# DSWT_H_OFFSETS_SINCE ---------------------------------------------------------
DSWT_H_OFFSETS_SINCE <- function # DSWT_H_OFFSETS_SINCE
### DSWT_H_OFFSETS_SINCE
() 
{
  offsetFile <- system.file(
    "extdata", "dswt_h_offsets_cm.csv", package = "kwb.dswt")
  
  read.table(offsetFile, sep = ";", header = TRUE, fill = TRUE)
}

# DSWT_FILE_TYPES --------------------------------------------------------------
DSWT_FILE_TYPES <- function # DSWT_FILE_TYPES
### list of file type definitions (containing e.g. a file name pattern) 
(
)
{
  list (  
    RAIN_DOWNLOAD_FROM_DSWT_SERVER = list(pattern = "^\\d{4}\\-.*\\.csv$"),
    RAIN_PER_DAY_FROM_BWB_1 = list(pattern = "__BERICHT\\.csv$"),
    RAIN_PER_DAY_FROM_BWB_2 = list(pattern = "__BERICHT_\\(2\\)\\.csv$"),
    RAIN_PER_5_MIN_FROM_BWB = list(pattern = "Regenschreiberdaten\\-(2013|2014)\\-Q[1-4]\\.xlsx$"),
    AUTO_SAMPLER_PN = list(pattern = "^PN.*\\.csv$"),
    RADAR_PROBE_H = list(pattern = "^H_(\\d{4})(\\d{2})(\\d{2})_(C|T)_(M[1-6])\\.csv$")  
  )  
}

# DSWT_BWB_CODE_TO_SITE_CODE ---------------------------------------------------
DSWT_BWB_CODE_TO_SITE_CODE <- function # DSWT_BWB_CODE_TO_SITE_CODE
### DSWT_BWB_CODE_TO_SITE_CODE
() 
{
  # T = Treffurter Str., C = Clayallee 
  list(Mal = "T_Mal",  
       Hsch = "T_Hsch", 
       Lbg = "T_Lbg", 
       MarI = "T_Mar1", 
       BieI = "T_Bie", 
       Stg = "C_Stg", 
       Wil = "C_Wil",  
       Wila = "C_Wila", 
       ZhlI = "C_Zhl1") 
}

# DSWT_RAIN_GAUGES -------------------------------------------------------------
DSWT_RAIN_GAUGES <- function # DSWT_RAIN_GAUGES
### DSWT_RAIN_GAUGES
()
{
  bwbShortNames <- c(
    "Hsch", "Mar I", "Mal", "Bie I", "Lbg", # Treffurter Str. 
    "Stg",  "Wil", "Wil a", "Zhl I"         # Clayallee
  )
  
  bwbRainGauges <- BWB_RAIN_GAUGES()
  bwbRainGauges[bwbRainGauges$BWB_SHORT %in% bwbShortNames, ]
    
  ### data frame with columns \emph{FUB_STATION}, \emph{FUB_SHORT}, 
  ### \emph{BWB_SHORT}       
}

# DSWT_SITES -------------------------------------------------------------------
DSWT_SITES <- function # DSWT_SITES
### DSWT_SITES
() 
{
  list(
    T_M1 = list(SiteID = 1),
    
    T_Hsch = list(SiteID = 27),
    T_Mar1 = list(SiteID = 26),
    T_Mal = list(SiteID = 28),
    T_Bie = list(SiteID = 24),
    T_Lbg = list(SiteID = 25),

    C_M1 = list(SiteID = 8),
    C_M2 = list(SiteID = 9),
    C_M3 = list(SiteID = 10),
    C_M4 = list(SiteID = 11),
    C_M5 = list(SiteID = 12),
    C_M6 = list(SiteID = 13),
    
    C_Stg = list(SiteID = 20),
    C_Wil = list(SiteID = 21),
    C_Wila = list(SiteID = 22),
    C_Zhl1 = list(SiteID = 23)
  )
  ### named vector of SiteID values in ODM database
}

# DSWT_TIMESERIES --------------------------------------------------------------
DSWT_TIMESERIES <- function # DSWT_TIMESERIES
### DSWT_TIMESERIES
(
)
{
  list(
    
    LEVEL_TUB_RAW = list(MethodID = 64,  # Wasserstand_Wasserstandsmessung aus Radarsonde/mlog
                         VariableID = 2, # Wasserhoehe 
                         SourceID = 2,   # TUB
                         QualityControlLevelID = 0 # Rohdaten
    ),
    
    FLOW_TUB_RAW = list(MethodID = 65,  # Errechnet auf Basis von Wasserhöhe_Durchfluss
                        VariableID = 4, # Durchfluss
                        SourceID = 2,   # TUB
                        QualityControlLevelID = 0 # Rohdaten
    ),
    
    RAIN_FUB_RAW = list(MethodID = 70,  # von FUB/BWB, mm/h_Regendaten
                        VariableID = 3, # Regenmenge
                        SourceID = 5,   # FUB
                        QualityControlLevelID = 0 # Rohdaten
    ),

    RAIN_BWB5min_RAW = list(MethodID = 60,  # BWB, mm/(5min)_Regendaten
                            VariableID = 3, # Regenmenge
                            SourceID = 1,   # BWB
                            QualityControlLevelID = 0 # Rohdaten
    )
  )
}

# keyFields_DSWT ---------------------------------------------------------------
keyFields_DSWT <- function # key field values in DSWT project
### key field values in DSWT project
(
)
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

    # Treffurter Straße - RS Hsch
    T_Hsch_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Hsch),

    # Treffurter Straße - RS Mar1
    T_Mar1_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Mar1),
    
    # Treffurter Straße - RS Mal
    T_Mal_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Mal),
                     
    # Treffurter Straße - RS Bie
    T_Bie_FUB = c(dswtTimeseries$RAIN_FUB_RAW, dswtSites$T_Bie),
    
    # Treffurter Straße - RS Lbg
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
