# Author: Mary Jewell
# Date created: 3/19/2026
# Last updated: 3/20/2026
# Notes: Extract data from WGS Sample Tracking sheet and format for CSV upload 
# to CDC RedCAP.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
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
# and not previously submitted (blank value in uploaded_to_red_cap)
wgs_tracking <- wgs_tracking %>% 
  filter(is.na(uploaded_to_red_cap) & # Not previously uploaded
           !is.na(successful_sequencing_run) &  # Successfully sequenced
           lab_accession != "DO NOT EDIT THIS LINE!!!!") # Not the blank line


# Create dataframe of variables needed for CDC submission
cdc_submit <- data.frame(record_id = as.character(wgs_tracking$lab_accession),
                         arln_specimen_id = as.character(wgs_tracking$lab_accession),
                         phl = "UT",
                         wgs_status = "WGS Successful",
                         wgs_id = as.character(wgs_tracking$arln_wgs_id),
                         srr_number = as.character(wgs_tracking$sra_number),
                         wgs_date_put_on_sequencer = paste0("20", stringr::str_sub(wgs_tracking$successful_sequencing_run, -6, -1)))



# Write submission CSV
write.csv(result, "path/to/file")


