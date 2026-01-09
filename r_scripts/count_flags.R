# Scripts for NRCS - USFS FSMMN Partnership
# Emily Piche
# Last Updated - Oct 24 2025

# Get a count of all the flags applied to the dataset

library(dplyr)
library(tidyr)
library(stringr)

wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

# labels for flags snagged from app server code
flag_labels <- c("t" = "Missing timestamp",
                 "n" = "Missing value",
                 "c" = "Sensor installation buffer",
                 "z" = "Impossible Value",
                 "s" = "Singletons",
                 "p" = "Step/Precip",
                 "f" = "Temperature",
                 "r" = "Sensor range",
                 "M" = "Manually selected",
                 "I" = "Interpolated",
                 "l" = "Limited data",
                 "X" = "Sensor compromised",
                 "." = "Incomplete precip test")

# empty list to put results from all three forests
all_counts_long <- list()
all_counts_wide <- list()

#forests <- c("HubbardBrook", "Fernow", "Coweeta")
#for (forest in forests) {
{forest <- "HubbardBrook"

  dir    <- paste0(dirname(wd), "/", forest, "/sensor_data/")
  metadata <- read.csv(paste0(dirname(wd), "/", forest, "/sensor_metadata.csv"))
  meta <- metadata %>% select(siteID, log_start) %>% arrange(siteID, desc(log_start)) %>%
    group_by(siteID) %>% slice_head(n=1) %>% ungroup() %>% rename(logger = siteID)
  qc_dir <- paste0(dir, "qc_files")
  file_list <- list.files(path = qc_dir, pattern = ".csv", full.names = FALSE)
  
  # empty lists for current forest
  forest_long  <- list()
  forest_wide  <- list()
  forest_widem <- list()
  
  # loop through each logger
  for (file in file_list) {
    df <- read.csv(file.path(qc_dir, file))
    logger <- sub("_qc.csv", "", file)
    count_rows <- nrow(df)
    
    # get just the flag cols
    flag_cols <- grep("_flag", names(df), value = TRUE)
    
    long_flags <- df %>%
      select(all_of(flag_cols)) %>%
      pivot_longer(everything(), names_to = "sensor_col", values_to = "flags_raw") %>%
      mutate(
        sensor_raw = sub("_flag$", "", sensor_col),
        flags_raw  = trimws(as.character(flags_raw))
      )
    
    # expand flags (one row per flag)
    expanded <- long_flags %>%
      filter(!is.na(flags_raw), flags_raw != "") %>%
      mutate(flags_compact = gsub("\\s+", "", flags_raw)) %>%
      mutate(flag_code = strsplit(flags_compact, "")) %>%
      select(sensor_raw, flag_code) %>%
      unnest(flag_code)
    
    # get depth and duplicate from column headers
    parsed    <- str_match(expanded$sensor_raw, ".*_(\\d+)([A-Za-z])?$")
    depth_vec <- suppressWarnings(as.integer(parsed[, 2]))
    dup_vec   <- parsed[, 3]
    dup_vec[dup_vec == ""] <- NA_character_
    
    # shorten sensor names (order matters)
    sensor_base <- case_when(
      str_detect(expanded$sensor_raw, "^vwc_temp(?:\\b|_)")    ~ "vwc_temp",
      str_detect(expanded$sensor_raw, "^matric_temp(?:\\b|_)") ~ "matric_temp",
      str_detect(expanded$sensor_raw, "^vwc(?:\\b|_)")         ~ "vwc",
      str_detect(expanded$sensor_raw, "^matric(?:\\b|_)")      ~ "matric",
      str_detect(expanded$sensor_raw, "^satext(?:\\b|_)")      ~ "satext",
      TRUE ~ expanded$sensor_raw
    )
    
    expanded <- expanded %>%
      mutate(sensor    = sensor_base,
             depth     = depth_vec,
             duplicate = dup_vec)
    
    # attach flag descriptions
    desc_tbl <- tibble(flag_code = names(flag_labels),
                       flag_description = unname(flag_labels))
    
    # long df
    counts_long <- expanded %>%
      group_by(sensor, depth, duplicate, flag_code) %>%
      summarise(n = n(), .groups = "drop") %>%
      left_join(desc_tbl, by = "flag_code") %>%
      mutate(logger = logger) %>%
      left_join(meta, by = "logger") %>%
      relocate(logger, sensor, depth, duplicate, log_start,
               flag_code, flag_description, n) %>%
      arrange(sensor, flag_code) 

    
    # wide df
    counts_wide <- counts_long %>%
      mutate(flag_col = paste0("flag_", flag_code)) %>%
      select(logger, sensor, depth, duplicate, log_start, flag_col, n) %>%
      tidyr::pivot_wider(names_from = flag_col, values_from = n, values_fill = 0) %>%
      mutate(total_rows = count_rows) %>%
      arrange(logger, sensor, depth) %>% relocate(total_rows, .after = log_start)
    
    # stick in lists of counts for this forest
    forest_long[[logger]]  <- counts_long
    forest_wide[[logger]]  <- counts_wide
  }
  
  # bind current forest 
  all_counts_long[[forest]]  <- bind_rows(forest_long)
  all_counts_wide[[forest]]  <- bind_rows(forest_wide)
}

# bind all forests
flag_counts_long <- bind_rows(all_counts_long) 
flag_counts_wide <- bind_rows(all_counts_wide) 

# get missing and removed percents
flag_counts_missing <- flag_counts_wide %>%
  mutate(
    raw_missing_n   = coalesce(flag_n, 0L) + coalesce(flag_t, 0L),
    raw_missing_pct = round(100 * raw_missing_n / pmax(total_rows, 1L), 2),
    rmv_n   = coalesce(flag_z, 0L) + coalesce(flag_X, 0L) + coalesce(flag_c, 0L),
    rmv_pct = round(100 * rmv_n / pmax(total_rows, 1L), 2)
  ) %>%
  relocate(raw_missing_n, raw_missing_pct, rmv_n, rmv_pct, .after = log_start)

flag_counts_missing <- flag_counts_missing %>% 
  mutate(depth = ifelse(!is.na(duplicate), paste0(depth, duplicate), depth)) %>%
  select(-raw_missing_n, -rmv_n, -duplicate) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

View(flag_counts_missing)

## filter for just vwc and smp
vwc_smp_flags <- flag_counts_missing %>% filter(sensor %in% c("matric", "vwc"))

View(vwc_smp_flags)

# write to csv if needed
#write.csv(vwc_smp_flags, "flag_summaries.csv", row.names = FALSE)
