# Author: Mary Jewell
# Date created: 9/11/2024
# Last updated: 10/3/2024
# Notes: Format PulseNet 2.0 data from LIMS export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load packages
rm(list = ls())
pacman::p_load(
  tidyverse, # data manipulation
  janitor,   # clean column names
  readxl,    # read Excel files
  writexl,   # write Excel files
  lubridate  # date formatting
)

## Read data
# Replace these file paths with the correct paths on your computer
# Important note: all slashes in file path should be / forward slashes!
lims <- read_xls("G:/MICRO/MOLECULAR LABORATORY/PFGEProgram/LIMS Data/2021712v2.xls", col_names = T, skip = 1)
lims <- janitor::clean_names(lims)

## Create/clean variables
# Key
lims$Key <- lims$samp_number

# Location
lims$SourceCountry <- lims$country
lims$SourceState <- "UT"
lims$SourceType <- "Human"

# Format age variables
lims$PatientAgeDays <- ""
lims$PatientAgeMonths <- ifelse(grepl("mo", lims$age), lims$age, "")
lims$PatientAgeMonths <- gsub("mo", "", lims$PatientAgeMonths)
lims$PatientAgeYears <- ifelse(grepl("mo", lims$age) == F, lims$age, "")

# Patient sex
lims$PatientSex <- ifelse(lims$sex == "M", "MALE", "FEMALE")

# Source site
lims$SourceSite <- str_to_title(lims$specimen_source)

# Date variables
# Isolation Date
lims$date_collected <- as.character(lims$date_collected)
lims$IsolatDate <- sub(" \\d{2}:\\d{2}:\\d{2}$", "", lims$date_collected)
lims$IsolatDate <- format(ymd(lims$IsolatDate), "%Y-%m-%d")
# Received Date
lims$date_received <- as.character(lims$date_received)
lims$ReceivedDate <- sub(" \\d{2}:\\d{2}:\\d{2}$", "", lims$date_received)
lims$ReceivedDate <- format(ymd(lims$ReceivedDate), "%Y-%m-%d")
# Lab Received Date?
lims$WGS_Lab_ReceivedDate <- ""

# Genus
lims <- mutate(lims,
               Genus = case_when(
                 bacteriology_result == "CAMPYLOBACTER jejuni" ~ "jejuni",
                 bacteriology_result == "ESCHERICHIA coli, O157:H7" ~ "E. coli O157",
                 bacteriology_result == "SHIGELLA flexneri" ~ "SHIGELLA flexneri",
                 bacteriology_result == "SHIGELLA sonnei" ~ "SHIGELLA sonnei"
               ))


# Sequencer Run ID
lims$SequencerRun_id <- ""


## Select columns
cleaned_data <- lims %>% select(Key, SourceCountry, SourceState, SourceType,
                                PatientAgeDays, PatientAgeMonths, PatientAgeYears,
                                PatientSex, SourceSite, IsolatDate, ReceivedDate,
                                WGS_Lab_ReceivedDate, Genus, SequencerRun_id)

# Remove footer
cleaned_data <- cleaned_data %>% filter(Key != "UT_Folder_Summary_Test_PFGE.rpt")


## Write finished data
write_xlsx(cleaned_data, "G:/MICRO/MOLECULAR LABORATORY/PFGEProgram/LIMS Data/2021712readyfor2.0.xlsx")



