# Apply QC functions for NRCS - USFS FSMMN Partnership
# Author: Emily Piche (epiche@esf.edu)
# last update: August 2025

# APPLY qc code using qc functions from "qc_functions.R" and
# the qc parameters set in "qc_parameters.R"

# Get functions (and packages) 
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)
source("qc_functions.R")
source("qc_parameters.R")

# Set which forest (directory) to apply qc to - 
# Can be run as a for loop to do multiple, or assign 'forest' as a single folder
#forests <- c("HubbardBrook", "Fernow", "Coweeta")
#for(forest in forests) {
{forest <- "HubbardBrook" 
  
#-------------------------------------------------------------------------------
# Set directory names and create qc_files folder if it does not exist 
dir <- paste0(dirname(dirname(wd)), "/", forest, "/sensor_data/")
master_dir <- paste0(dir, "master")
qc_output_dir <- paste0(dir, "qc_files")
if(!dir.exists(qc_output_dir)) {dir.create(qc_output_dir)}

# Find all logger file paths
file_list <- list.files(path = master_dir, pattern = ".csv")

# get precip data 
precip_data <- read.csv(paste0(dirname(dir), "/precip/", forest, "_precip.csv"))
precip_data$DateTime <- as.POSIXct(precip_data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Set up loop ------------------------------------------------------------------
for (file in file_list) {
#file<-file_list[16]
test_data <- read.csv(paste0(master_dir,"/", file))

# Get the qc column names for each variable in a list --------------------------

## Volumetric water content
vwc <- names(test_data)[grepl("^vwc", names(test_data)) &
                                 !grepl("^vwc_temp", names(test_data))]
## VWC temperature
vwc_temp <- names(test_data)[grepl("^vwc_temp", names(test_data))]
## Matric kPa
matric_kPa <- names(test_data)[grepl("^matric_kPa", names(test_data))]
## Matric temperature
matric_temp <- names(test_data)[grepl("^matric_temp", names(test_data))]
## Electrical conductivity
satext <- names(test_data)[grepl("^satext", names(test_data))]

## Bind lists of colnames into list of lists
list_of_var_cols <- list(vwc = vwc, 
                         vwc_temp = vwc_temp, 
                         matric_kPa = matric_kPa, 
                         matric_temp = matric_temp, 
                         satext = satext)

# First, round to correct data resolution --------------------------------------
test_data_rounded <- round_by_sensor(test_data, list_of_var_cols, sensor_range_parameters)

# add _qc to colnames in list of col headers by variable
for (i in 1:length(list_of_var_cols)){
  if (length(list_of_var_cols[[i]]) > 0){
    list_of_var_cols[[i]] <- paste0(list_of_var_cols[[i]], "_qc")
  }
}

# Add rows for missing datetimes and fill in datetimes -------------------------
## variables in those timestamps are filled as 'NA'
data_datetimes_filled <- fill_datetimes(test_data_rounded, time_step, flag_ts)

# remove logger battery columns (will stick these back on later)
if(any(grepl("logger", names(data_datetimes_filled)))){
logger_col_names <- grep("logger", names(data_datetimes_filled), value = TRUE)
logger_cols <- data_datetimes_filled %>% select(all_of(logger_col_names))
data_no_logger <- data_datetimes_filled %>% select(-all_of(logger_col_names))
} else {
  data_no_logger <- data_datetimes_filled
}

# Add qc and flag columns ------------------------------------------------------
data_qc <- create_qc_and_flag_columns(data_no_logger, start = 3)

# flag missing data ------------------------------------------------------------
data_ts_flagged <- flag_missing_data(data_qc, flag_na)

# zero out time series flags - flags will be merged later
data_full_ts <- data_ts_flagged %>%
  mutate(across(contains("flag"), ~ NA))

# flag singleton values under sufficient battery conditions --------------------
data_single_flagged <- flag_singletons(data_full_ts, logger_cols$logger_battPerc, 5, flag_singleton) 

# remove first x number of days of matric potential data (install_buffer)
data_matric_flagged <- matric_install_buffer(data_full_ts, unlist(list_of_var_cols["matric_kPa"]), install_buffer, flag_matric)

# zero out flags for next test
data_lim_input <- data_matric_flagged %>% 
  mutate(across(contains("flag"), ~ NA))

# flag small amounts of data within large spans of NA --------------------------
data_lim_flagged <- flag_sparse_short_streaks(data_lim_input, short_streak_max_hours, window_days,
                                      min_points_window, flag_limited_data) 

# zero out flags for next test
data_temp_input <- data_lim_flagged %>% 
  mutate(across(contains("flag"), ~ NA))

# Perform temperature test -----------------------------------------------------
data_temp_flagged <- temp_test_and_flag(data_temp_input, list_of_var_cols, 
                                        temperature_threshold, flag_temp)

# zero out temp flags 
imp_val_input <- data_temp_flagged %>% 
   mutate(across(contains("flag"), ~ NA))


# Remove impossibles (right now this is just satext <= 0) ----------------------
data_no_zeroes_flagged <- rmv_impossible_vals(imp_val_input, unlist(list_of_var_cols["satext"]), "satext", 
                                             sensor_range_parameters, flag_impossible)

# zero out flags again for subsequent tests (merge all flags later)
data_input <- data_no_zeroes_flagged %>%
  mutate(across(contains("flag"), ~ NA))


# Perform the sensor range test ------------------------------------------------
# flags values outside the sensor's range of accuracy but doesn't remove them
data_sensor_flagged <- sensor_test_and_flag(data_input, list_of_var_cols,
                                            sensor_range_parameters, flag_sensor_range)


# step and precip tests ---------------------------------------------------
## Make step z cols 
step_stats <- get_step_z(data_input, unlist(list_of_var_cols["vwc"]))

## Flag steps > threshold SD value with no precip within x hours
data_step_flagged <- step_flag(data_input, file, threshold = 3, step_stats, flag_step, precip_data, precip_window, flag_no_precip)



# remove data from sensors identified as malfunctioning
data_clean_flagged <- remove_compromised(data_input, remove_data, file, flag_compromised) 


## concatenate the flag outputs ------------------------------------------------
flag_dfs <- list(data_ts_flagged, # this has both timeseries and na flags
                 data_matric_flagged,
                 data_single_flagged,
                 data_no_zeroes_flagged, 
                 data_sensor_flagged, 
                 data_step_flagged, 
                 data_temp_flagged, 
                 data_lim_flagged,
                 data_clean_flagged)

data_save <- data_clean_flagged

flag_cols <- grep("flag", names(data_save), value = TRUE)

for (col in flag_cols) {
  combined_flags <- apply(sapply(flag_dfs, function(df) df[[col]]), 1, function(x) {
    vals <- x[!is.na(x) & x != ""]
    if (length(vals) == 0) NA else paste(vals, collapse = " ")
  })
  data_save[[col]] <- combined_flags
}

# add logger cols back if they exist -------------------------------------------
if(any(grepl("logger", names(data_datetimes_filled)))){
data_save <- cbind(data_save, logger_cols)
}

# make sure datetime is written to file correctly: posixct format will cause errors
data_save$datetime<-format(data_save$datetime, "%Y-%m-%d %H:%M:%S")

# Write CSV --------------------------------------------------------------------
write.csv(data_save, paste0(qc_output_dir, "/", gsub(".csv", "_qc.csv", file)), row.names = FALSE)
message(paste("Processed and saved: ", gsub(".csv", "_qc.csv", file)))
}

# Log the qc parameters applied
setwd(qc_output_dir)
log_lines <- c()
log_lines <- c(log_lines, paste("QC Tests Applied", Sys.time()))
log_lines <- c(log_lines, paste(length(file_list), " logger files processed"))
log_lines <- c(log_lines, "------------------------------")
log_lines <- c(log_lines, "\n---Flag Definitions---")
log_lines <- c(log_lines,
               paste(flag_na, "- Missing value"),
               paste(flag_ts, "- Missing timeseries"),
               paste(flag_matric, "- Matric Potential installation buffer"),
               paste(flag_singleton, "- Measurement skipping"),
               paste(flag_temp, "- Sensor temperature"),
               paste(flag_sensor_range, "- Out of sensor range of accuracy"),
               paste(flag_step, "- High step/precipitation test failed"),
               paste(flag_no_precip, "- High step/precipitation test incomplete"))
log_lines <- c(log_lines, "\n---Sensor Range Bounds---")
log_lines <- c(log_lines, 
               paste0("TEROS 11 and 12\n", "- Volumetric Water Content, m3/m3 \n-- min: ", 
                      vwc_min, ", max: ", vwc_max),
               paste0("- Soil Temperature, Degrees C \n-- min:", vwc_temp_min,
                     ", max: ", vwc_temp_max),
               paste0("- Saturation Extract EC, μS/cm \n-- min: ", satext_min, 
                      ", max: ", satext_max),
               paste0("TEROS 21\n- Matric Potential, kPa \n-- min: ", matric_kPa_min, 
                      ", max: ", matric_kPa_max),
               paste0("- Soil Temperature, Degrees C\n-- min:", matric_temp_min, 
                      ", max: ", matric_temp_max))
 log_lines <- c(log_lines,
                paste0("\nTime buffer for TEROS 21 installation: ", install_buffer, " days"))
 log_lines <- c(log_lines,
               paste0("\nTemperature threshold to end frozen soil flagging: ", temperature_threshold, " deg C"))
 log_filename <- paste0("QC_log.txt")
 writeLines(log_lines, con = log_filename)
}


