# QC parameters for NRCS - USFS FSMMN Partnership
# Author: Emily Piche (epiche@esf.edu)
# Last update: September 2025

remove_data <- read.csv("remove_data.csv")
remove_data$after <- as.POSIXct(as.character(remove_data$after), format = "%m/%d/%Y", tz = "UTC")

# Parameters for QC tests------------------------------------------------------

# --- Set sensor range and accuracy parameters  --------------------------------
## TEROS 11 and 12
## Volumetric Water Content, m3/m3 or percent
vwc_min <- 0.0001
vwc_max <- 0.7
vwc_round <- 3
## Soil Temperature, Degrees C
vwc_temp_min <- -40
vwc_temp_max <- 60
vwc_temp_round <- 2
## Saturation Extract EC, milliSiemens/cm 
satext_min <- 0
satext_max <- 20000
satext_round <- 3
## TEROS 21
## Matric Potential, kPa
matric_kPa_min <- -100000 # see manual section 3.3.3
matric_kPa_max <- 0
matric_kPa_round <- 1 
## Soil Temperature, Degrees C
matric_temp_min <- -40
matric_temp_max <- 60
matric_temp_round <- 2

# --- Highlight areas of low data density --------------------------------------
short_streak_max_hours <- 24*7
window_days <- 30
min_points_window <- (24*7)

# --- Temperature range threshold ----------------------------------------------
temperature_threshold <- 0.25

# --- Step/Precip test ---------------------------------------------------------
precip_window <- 12

# --- TEROS 21 installation buffer ---------------------------------------------
install_buffer <- 7 # days

# --- Set the flag identifiers for each qc step ------
flag_na            <- "n" 
flag_ts            <- "t"
flag_temp          <- "f"
flag_sensor_range  <- "r"
flag_step          <- "p" 
flag_no_precip     <- "."
flag_impossible    <- "z"
flag_singleton     <- "s"
flag_matric        <- "c"
flag_limited_data  <- "l"
flag_compromised   <- "X"
 
# Bind sensor range parameters in a dataframe for looping through later --------
sensor_range_parameters <- data.frame(
  vars = c('vwc', 'vwc_temp', 'matric_kPa', 'matric_temp', 'satext'),
  min = c(vwc_min, vwc_temp_min, matric_kPa_min, matric_temp_min, satext_min),
  max = c(vwc_max, vwc_temp_max, matric_kPa_max, matric_temp_max, satext_max),
  round_to = c(vwc_round, vwc_temp_round, matric_kPa_round, matric_temp_round, satext_round))

# Set dataset time step (other option is 'daily', which wont be used here)
time_step <- "hourly"
