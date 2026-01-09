# QC functions for NRCS - USFS FSMMN Partnership
# Author: Emily Piche (epiche@esf.edu)
# last update: August 2025

# file contains qc functions for soil moisture sensor data 
# downloaded from METER loggers

# install any missing packages and load required
packages_needed <- c("tidyverse", "data.table")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) {install.packages(packages_to_install)}

# load packages with lapply 
lapply(packages_needed, library, character.only = TRUE)

# Function - round values to correct resolution for each variable --------------

# loop through sensor parameters df and round each column - no flags here
round_by_sensor <- function(df, list_of_vars, parameters) {
  sensor_df <- df
  # get sensor range parameters for each data type
  for (i in 1:nrow(parameters)) {
    var <- parameters$vars[i]
    round_to <- parameters$round_to[i]
    # cols for this data type
    cols_to_round <- sub("_qc", "", list_of_vars[[var]])
    if(length(cols_to_round) > 0) {
      # round columns based on data type
      sensor_df <- sensor_df %>%
        mutate(across(all_of(cols_to_round), ~ round(., round_to)))
    }
  }
  return(sensor_df)
}


# Function - fill missing timeseries rows --------------------------------------

# creates rows for missing datetime values, fills datetime,
# and fills in the rest of the row with NA
fill_datetimes <- function(df, timestep, ts_flag) {
  ts_flag<-ts_flag
  if(timestep == "hourly"){
    # Ensure the datetime_col is converted to POSIXct 
    df <- df %>% 
      mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
      # some of the early measurements are 15 min - remove readings that are not on the hour
      filter(format(datetime, "%M") == "00") %>%
      # add flag column (temporarily fill to mark original rows)
      mutate(flag = 0) %>% select(datetime, flag, everything())
    # fill missing datetimes
    df_complete <- df %>%
      complete(datetime = seq(from = min(datetime), to = max(datetime), by = "hour")) %>%
      mutate(flag = ifelse(is.na(flag), ts_flag, NA))
  } else {
    # make sure date is formatted correctly
    df <- df %>% 
      # add flag column (temporarily fill to mark original rows)
      mutate(flag = 0) %>% select(date, flag, everything())
    # fill missing datetimes
    df_complete <- df %>%
      complete(date = seq(from = min(date), to = max(date), by = "day")) %>%
      mutate(flag = ifelse(is.na(flag), ts_flag, NA))
  }
  return(df_complete)
}


# Function - create qc and flag columns --------------------------------------

# no test applied here, just initiating columns in the df
create_qc_and_flag_columns <- function(df, start) {
  # don't make qc and flag cols for cols that come before start col index
  cols_to_keep <- names(df)[1:start-1]
  measurement_cols <- names(df)[-c(1:start-1)]
  # create qc and flag cols
  df <- df %>% mutate(across(all_of(measurement_cols),
                             .fns = list(qc = ~ .,
                                         flag = ~ NA),
                             .names = "{.col}_{.fn}"))
  # reorder
  correct_order <- cols_to_keep
  for(col in measurement_cols) {
    correct_order <- c(correct_order, col, paste0(col, "_qc"), paste0(col, "_flag"))
  }
  df <- df[, correct_order]
  return(df)
}


# Function - move timeseries flags to the qc columns ---------------------------

# copies time series flags over to all flag columns, also 
# flags where values were missing but datetime *was* recorded by logger
flag_missing_data <- function(df, na_flag) {
  flag_cols <- grep("_flag", colnames(df), value = TRUE)
  for (new_flag in flag_cols) {
    base_col <- sub("_flag", "", new_flag)
    qc_col <- paste0(base_col, "_qc")
    df[[new_flag]] <- df$flag
    df[[new_flag]] <- ifelse(is.na(df[[qc_col]]) & is.na(df$flag), na_flag, df[[new_flag]])
  }
  df$flag <- NULL
  return(df)
}


# Function - Flag singletons under sufficient battery --------------------------

# flag points that are logged with NA on either side and battery is above 5%
flag_singletons<- function(df,
                           battery_col,
                           batt_thresh    = 5,
                           singleton_flag) {
  # get flag cols
  qc_cols <- grep("_qc$", names(df), value = TRUE)
  # loop over them
  for (qcol in qc_cols) {
    base_col  <- sub("_qc$", "", qcol)
    flag_col  <- paste0(base_col, "_flag")
    test_col  <- df[[qcol]]
    batt <- battery_col
    # get previous and next values 
    prev_val <- lag(test_col)
    next_val <- lead(test_col)
    # identify singleton points
    is_singleton <- !is.na(test_col) &
      is.na(prev_val) &
      is.na(next_val) &
      batt > batt_thresh
    # flag single points
    df[[flag_col]][is_singleton] <- singleton_flag
  }
  df
}


# Function - account for matric potential installation soil disturbance --------

# remove the first week of data from matric_kPa columns
matric_install_buffer <- function(df, cols_to_test, buffer_length, matric_flag) {
  clean_df <- df
  day1 <- df$datetime[1]
  day7 <- day1 + days(buffer_length)
  # loop through and remove first week of matric_kPa values
  for (col in cols_to_test) {
    qc_col <- col
    base_col <- gsub("_qc", "", qc_col)
    flag_col <- paste0(base_col, "_flag")
    # where datetime is min through min + 7 days, remove and flag
    clean_df[[qc_col]][clean_df$datetime < day7] <- NA_real_
    clean_df[[flag_col]][clean_df$datetime < day7] <- matric_flag
  }
  return(clean_df)
}


# Function - Flag small streaks in empty windows -----------------------------

flag_sparse_short_streaks <- function(df, short_streak_max_hours, window_days,
                                      min_points_window, limited_data_flag) {
  datetime <- df$datetime 
  qc_cols <- grep("qc", names(df), value = TRUE)
  
  for (qc_col in qc_cols) {
    flag_col <- sub("qc", "flag", qc_col)
    # Logical vector, TRUE where there is data
    is_data <- !is.na(df[[qc_col]])
    # identify runs of non-NA data with rle
    runs <- rle(is_data)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1
    # dataframe of data streaks to get indices to check window around
    streak_info <- data.frame(start_idx = starts,
                              end_idx = ends,
                              is_data = runs$values) %>%
      filter(is_data) %>%
      mutate(start_time = datetime[start_idx],
             end_time = datetime[end_idx],
             duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
             center_time = start_time + (end_time - start_time) / 2) %>%
      filter(duration_hours <= short_streak_max_hours)
    
    # check local data density for each short streak
    for (i in seq_len(nrow(streak_info))) {
      center <- streak_info$center_time[i]
      window_mask <- abs(difftime(datetime, center, units = "days")) <= (window_days/2)
      window_non_na_count <- sum(!is.na(df[[qc_col]][window_mask]))
      # flag where surrounding data is insufficient
      if (window_non_na_count < min_points_window) {
        idx_to_flag <- streak_info$start_idx[i]:streak_info$end_idx[i]
        df[[flag_col]][idx_to_flag] <- limited_data_flag 
      }
    }
  }
  return(df)
}


# Function - Temperature Test --------------------------------------------------

# flags values that were recorded when the sensor temp was < 0
temp_test_and_flag <- function(df, var_cols, temp_threshold = 1, temp_flag) {
  # Helper function: forward-fill NAs (LOCF)
  na_fill_forward <- function(x) {
    if (length(x) == 0) return(x)
    for (i in 2:length(x)) {
      if (is.na(x[i])) {
        x[i] <- x[i - 1]
      }
    }
    return(x)
  }
  # helper function for frozen soil flagging:
  # flag everything that comes AFTER a value of <= 0 that is still below threshold
  # once a value goes >= threshold, streak is broken
  flag_frozen_soil <- function(temp_vector, threshold, impute = TRUE) {
    if (impute) {
      temp_vector <- na_fill_forward(temp_vector)
    }
    flag_indices <- rep(FALSE, length(temp_vector))
    in_streak <- FALSE  # indicates that a freezing event has occurred
    for (i in seq_along(temp_vector)) {
      # skip if na
      if(is.na(temp_vector[i])) next
      if (temp_vector[i] <= 0) {
        # freezing event resets or starts the streak
        in_streak <- TRUE
        # <= 0 values are already flagged elsewhere
      } else if (temp_vector[i] > 0 && temp_vector[i] < threshold) {
        # if in a streak from a recent freezing event, flag it
        if (in_streak) {
          flag_indices[i] <- TRUE
        }
      } else if (temp_vector[i] >= threshold) {
        # once temp is above or equal to threshold, break streak
        in_streak <- FALSE
      }
    }
    return(flag_indices)
  }
  df_temp <- df
  # for TEROS 21:
  if ("matric_temp" %in% names(var_cols)) {
    for (depth in seq_along(var_cols$matric_temp)) {
      qc_col   <- var_cols$matric_temp[depth]
      base_col   <- sub("_qc", "", var_cols$matric_temp[depth])
      mtemp_flag <- sub("qc", "flag", var_cols$matric_temp[depth])
      kpa_qc   <- var_cols$matric_kPa[depth]
      kpa_flag   <- sub("qc", "flag", var_cols$matric_kPa[depth])
      # flag values where temperature <= 0 and values after until threshold is reached
      # set matric kPa to NA where temp was <= 0 + but not at threshold
      frozen_flags <- flag_frozen_soil(df_temp[[base_col]], temp_threshold)      
      # flag and remove matric potential values recorded during freeze
      df_temp[[kpa_qc]][df_temp[[base_col]] <= 0] <- NA
      df_temp[[kpa_flag]][df_temp[[base_col]] <= 0] <- temp_flag
      df_temp[[kpa_qc]][frozen_flags] <- NA
      df_temp[[kpa_flag]][frozen_flags] <- temp_flag
    }
  }
  # for TEROS 11 and 12 (wont trigger errors if TEROS 10 are used in future)
  if ("vwc_temp" %in% names(var_cols)) {
    for (depth in seq_along(var_cols$vwc_temp)) {
      qc_col    <- var_cols$vwc_temp[depth]
      base_col    <- sub("_qc", "", var_cols$vwc_temp[depth])
      vtemp_flag  <- sub("qc", "flag", var_cols$vwc_temp[depth])
      vwc_qc    <- var_cols$vwc[depth]
      vwc_flag    <- sub("qc", "flag", var_cols$vwc[depth])
      # Flag values where temperature <= 0 and values after until threshold is reached
      frozen_flags <- flag_frozen_soil(df_temp[[qc_col]], temp_threshold)
      # teros 11/12 - flag and remove vwc values recorded during freeze
      df_temp[[vwc_qc]][df_temp[[base_col]] <= 0] <- NA
      df_temp[[vwc_flag]][df_temp[[base_col]] <= 0] <- temp_flag
      df_temp[[vwc_qc]][frozen_flags] <- NA
      df_temp[[vwc_flag]][frozen_flags] <- temp_flag
      # teros 12 - flag and remove ec values recorded during freeze
      if(length(var_cols$satext > 0)){
      satext_qc <- var_cols$satext[depth]
      satext_flag <- sub("qc", "flag", var_cols$satext[depth])
      df_temp[[satext_qc]][df_temp[[base_col]] <= 0] <- NA
      df_temp[[satext_flag]][df_temp[[base_col]] <= 0] <- temp_flag
      df_temp[[satext_qc]][frozen_flags] <- NA
      df_temp[[satext_flag]][frozen_flags] <- temp_flag
      }
    }
  }
  return(df_temp)
}


# Function - Flag and remove impossible values ---------------------------------

# right now this is just used for soil ec/satext, can be updated for 
# other variables if needed
rmv_impossible_vals <- function(df, cols_to_test, var, parameters, impossible_flag) {
  clean_df <- df
  # select parameters from df
  min <- parameters$min[which(parameters$vars == var)]
  # loop through and flag values outside min and max for each var type
  for (col in cols_to_test) {
    qc_col <- col
    base_col <- gsub("_qc", "", qc_col)
    flag_col <- paste0(base_col, "_flag")
    # where qc col is > max or < min, flag with sensor range flag
    clean_df[[flag_col]][clean_df[[qc_col]] <= min] <- impossible_flag
    clean_df[[qc_col]][clean_df[[qc_col]] <= min] <- NA_real_
  }
  return(clean_df)
}


# Function - Sensor range test and flag ---------------------------------------

# flag values outside of sensor's range of accuracy
sensor_test_and_flag <- function(df, list_of_vars, parameters, sensor_range_flag) {
  sensor_df <- df
  # get sensor range parameters for each data type
  for (i in 1:nrow(parameters)) {
    var <- parameters$vars[i]
    min <- parameters$min[i]
    max <- parameters$max[i]
    # cols for this data type
    cols_to_test <- list_of_vars[[var]]
    # loop through and flag values outside min and max for each var type
    for (col in cols_to_test) {
      qc_col <- col
      base_col <- gsub("_qc", "", qc_col)
      flag_col <- paste0(base_col, "_flag")
      # where qc col is > max or < min, flag with sensor range flag
      sensor_df[[flag_col]][sensor_df[[qc_col]] < min] <- sensor_range_flag
      sensor_df[[flag_col]][sensor_df[[qc_col]] > max] <- sensor_range_flag
    } 
  }
  return(sensor_df)
}


# Function - Step test ---------------------------------------------------------

# looks at step from value and lag value, 
# checks mean and sd of *all steps*, finds z score of each step value
# if z score above 3 and no precip in last x hours, then flag
get_step_z <- function(df_data, cols_to_test) {
  step_df <- df_data %>% 
    select(datetime, any_of(grep("qc", colnames(df_data), value = TRUE)))
  
  for (qc_col in cols_to_test) {
    # make colnames
    z_wet    <- sub("qc", "z_wet", qc_col)
    z_dry    <- sub("qc", "z_dry", qc_col)
    step_col <- sub("qc", "step", qc_col)
    step_dir <- sub("qc", "step_d", qc_col)
    flag_col <- sub("qc", "flag", qc_col)
    
    # get step direction and absolute values
    step_df[[step_col]] <- ifelse(!is.na(step_df[[qc_col]]), step_df[[qc_col]] - lag(step_df[[qc_col]]), NA)
    # direction of step
    step_df[[step_dir]] <- ifelse(step_df[[step_col]] > 0, "w", ifelse(step_df[[step_col]] < 0, "d", NA_character_))
    
    wet_idx <- which(step_df[[step_dir]] == "w")
    dry_idx <- which(step_df[[step_dir]] == "d")

    w_mean <- if(length(wet_idx)) mean(step_df[[step_col]][wet_idx], na.rm = TRUE) else NA_real_
    w_sd   <- if(length(wet_idx)) sd(step_df[[step_col]][wet_idx], na.rm = TRUE) else NA_real_
    
    d_mean <- if(length(dry_idx)) mean(abs(step_df[[step_col]][dry_idx]), na.rm = TRUE) else NA_real_
    d_sd   <- if(length(dry_idx)) sd(abs(step_df[[step_col]][dry_idx]), na.rm = TRUE) else NA_real_
    
    step_df[[z_wet]] <- NA_real_
      if(length(wet_idx) && !is.na(w_sd) && w_sd > 0) {
        step_df[[z_wet]][wet_idx] <- (step_df[[step_col]][wet_idx] - w_mean) / w_sd
      }
    
    step_df[[z_dry]] <- NA_real_
    if(length(dry_idx) && !is.na(d_sd) && d_sd > 0) {
      step_df[[z_dry]][dry_idx] <- (step_df[[step_col]][dry_idx] - d_mean) / d_sd
    }
  } 

  step_df <- step_df %>% select(1, sort(names(step_df)[-1]))
  return(step_df)
}


# Function - Verify Precip -----------------------------------------------------

# check if there is precip before very high soil moisture increases
step_flag <- function(df_data, file, threshold, df_step, step_flag, 
                      df_precip, p_window, no_precip_flag) {
  df_flagged <- df_data
  # extra step for HB data, which has a rain gauge in each watershed
    if ("watershed" %in% names(df_precip)) {
      # get wshed # from filename
      wshed <- sub(".*?(\\d+).*", "\\1",  file)
      # filter precip for the correct wshed
      precip <- df_precip %>%
        mutate(watershed = sub("W", "", watershed)) %>%
        filter(watershed == wshed)
    } else {
      precip <- df_precip
    }

  # get flag cols to iterate over
  flag_cols <- grep("vwc.*flag", colnames(df_data), value = TRUE)
  flag_cols <- grep("temp", flag_cols, value = TRUE, invert = TRUE)
  # iterate through each col and flag
  for(flag_col in flag_cols) {
    step_z <- sub("flag", "z_wet", flag_col)
    # indices where step test fails
    idx_step <- which(!is.na(df_step[[step_z]]) & df_step[[step_z]] > threshold) 
    # if step is high, check precip against it
    for(i in idx_step) {
      t_i <- df_flagged$datetime[i]
      # get precip window
      window <- precip %>%
        filter(DateTime  > t_i - hours(p_window),
               DateTime <= t_i)
      # no precip data - flag as an incomplete test
      if(nrow(window) == 0) {
        df_flagged[[flag_col]][i] <- no_precip_flag
      } else if(all(is.na(window$precip), na.rm = FALSE)) { 
        df_flagged[[flag_col]][i] <- no_precip_flag
      } else if(all(window$precip == 0, na.rm = TRUE)) {
        # no precip in selected window - step/precip flag
        df_flagged[[flag_col]][i] <- step_flag
      }
    }
  }
  return(df_flagged)
}


## function - remove sensors identified in "remove_data" df --------------------

remove_compromised <- function(df, removal_info, file, compromised_flag) {
  data <- df
  current_logger <- sub(".csv", "", file)
  if (current_logger %in% removal_info$logger) {
    removal_info <- removal_info %>% filter(logger == current_logger)
    for (i in 1:length(removal_info$logger)) {
      col <- removal_info$header[i]
      flag_col <- sub("qc",  "flag", col)
      after <- removal_info$after[i]
      if (is.na(after)) {
        data[[col]] <- NA 
        data[[flag_col]] <- compromised_flag
      } else {
        clear_idx <- data$datetime > as.POSIXct(after, tz = "UTC") 
        data[[col]][clear_idx] <- NA
        data[[flag_col]][clear_idx] <- compromised_flag
      }
    }
  }
  return(data)
}

