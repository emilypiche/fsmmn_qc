
* DESCRIPTION *

This repository contains files for applying quality control (QC) procedures developed for the USDA Forest Soil Moisture Monitoring Network (FSMMN). Raw files from the Hubbard Brook loggers are provided as example inputs for the user to execute the code and view the flagging procedure.


* CONTENTS & STRUCTURE * 

A folder named `HubbardBrook` contains: 
* `sensor_data/raw` folder (raw sensor measurements downloaded from the datalogger)
* `precip` folder (data from doi.org/10.6073/pasta/e79bc39b1bb83b5c2bb17e2925d9c83b)
* `shapefiles` folder (instrumented watershed boundaries)
* `sensor_metadata.csv` (sensor metadata)
* `pedons.csv` (soil profile descriptions)


Code to run the QC procedures can be found in fsmmn_qc/r_scripts/QAQC, which contains:
* `reformat_ZL6_output.Rmd`
* `qc_apply.R`
* `qc_functions.R`
* `qc_parameters.R`
* `remove_data.csv`


An additional file named `count_flags.R` has been provided in fsmmn_qc/r_scripts to help the user inspect data flagged by the QC process. This script will summarize total hours since the sensor began recording (`total_rows`), percent raw data missing from each data stream (`raw_missing_pct`), percent of data removed from each data stream due to QC procedures (`rmv_pct`), and a count of each QC flag. Definitions for each QC flag are provided in the `flag_codes.txt` file.


* USAGE *

The code must be executed in the following order:

(1) `reformat_ZL6.Rmd` :  This reformats raw data files (from the `raw` sensor data subfolder) so that the QC procedure can be applied. This markdown file contains three code blocks, which should be run consecutively. The output of this code will be two new folders: `reformatted` and `master`. A new .csv corresponding to each raw .xlsx logger file will be written to the `reformatted` folder. The `master` folder will contain a master .csv file for each logger - all downloads from the logger identified in the filename will be combined within that file.

(2) `qc_apply.R` : This script will access the files from the `master` folder. This R code uses functions stored in `qc_functions.R`, settings from `qc_parameters.R`, and additional inputs from `remove_data.csv` and files in the `precip` folder. 

(3) New QC-processed files will be written to `fsmmn_qc/HubbardBrook/sensor_data/qc_files`. 

(4) `count_flags.R` : This script will create a summary of all data that was flagged during the QC procedure. 


* CONTACT *
Emily Piche (epiche@esf.edu)
Amanda Pennino (apennino@esf.edu)




