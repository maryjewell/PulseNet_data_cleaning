# Author: Mary Jewell
# Date created: 3/19/2026
# Last updated: 3/19/2026
# Notes: Extract data from WGS Sample Tracking sheet and format for CSV upload 
# to CDC RedCAP.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries
library(googledrive)
library(googlesheets4)
library(dplyr)


###Authorize google drive access###
###RUN THIS SECTION FIRST###
drive_auth()
gs4_auth(token = drive_token())

# Read the ARLN_regional sheet from the WGS Sample Tracking sheet on Google
wgs_tracking  <- read_sheet("https://docs.google.com/spreadsheets/d/1JurSedOCjkDEnlu_rFjUVrpEQeKoCutb9LwBQYl-lno/edit?gid=6322761#gid=6322761",
                            sheet = "ARLN_regional")

# Clean column names 
wgs_tracking <- janitor::clean_names(wgs_tracking)

# Filter for rows successfully sequenced (valid sequencing run ID)
wgs_tracking <- wgs_tracking %>% filter(!is.na(successful_sequencing_run))

# Filter rows since the beginning of 2026
wgs_tracking <- wgs_tracking %>% filter(extraction_date > "2025-12-30")

# Create dataframe of variables needed for CDC submission
cdc_submit <- data.frame(record_id = wgs_tracking$lab_accession,
                         arln_specimen_id = wgs_tracking$lab_accession,
                         phl = "UT",
                         wgs_status = "WGS Successful",
                         wgs_id = arln_wgs_id,
                         srr_number = sra_number,
                         wgs_date_put_on_sequencer = paste0("20", stringr::str_sub(x, -6, -1)))


# Filter submission dataset by rows not found in last submission
last_submission <- read.csv("path/to/file")
result <- anti_join(cdc_submit, df2, by = "record_id")

# Write submission CSV
write.csv(result, "path/to/file")

# record_id = lab accession number
# arln_specimen_id = lab accession number
# phl = UT
# wgs_status = if non NA value in successful sequencing run, then "WGS Successful" All other rows not included
# wgs_id = arln_wgs_id
# srr_number = sra number
# wgs_date_put_on_sequencer = 20 + last 6 digits of the run name


