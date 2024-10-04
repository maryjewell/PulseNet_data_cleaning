# Author: Mary Jewell
# Date created: 9/11/2024
# Last updated: 10/4/2024
# Notes: Merge PulseNet 2.0 data with PHI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load packages
# install.packages("pacman")
pacman::p_load(
  tidyverse, # data manipulation
  janitor,   # clean column names
  openxlsx,   # read and write Excel files
  readxl
)


## Read data
# Replace these file paths with the correct paths on your computer
# Important note: all slashes in file path should be / forward slashes!
pulsenet <- read_excel("G:/MICRO/MOLECULAR LABORATORY/PFGEProgram/WGS/Epi reports/2024/PN 2.0 Reports/Salmonella PN 2.0 10.3.24.xlsx")
phi <- read.csv("L:/Shared_Files/Mirth/COVID_data_import/Ent_Summary_10_02_2024.csv")

## Clean column names
pulsenet <- janitor::clean_names(pulsenet)
phi <- janitor::clean_names(phi)

## Select relevant column names from each file
phi <- phi %>% select(sample_number, last_name, first_name, dob, customer_name)

pulsenet <- pulsenet %>% select(key, source_type, source_site, patient_sex, patientageyears, 
                                patientagemonths, isolat_date, received_date, serotype_wgs, 
                                date_published, allele_code, outbreak, pulse_net_upload_date, 
                                wgs_id, ncbi_accession, type_details, rep_code, source_state)

## Merge data
merged_data <- merge(pulsenet, phi,
                     by.x = c("key"),
                     by.y = c("sample_number"),
                     all.x = T)


# Remove columns with 100% missingness
merged_data <- merged_data %>% purrr::discard(~sum(is.na(.x))/length(.x)*100 == 100)

# Formatting dates

merged_data$dob <- as.POSIXct(merged_data$dob, format="%m/%d/%Y %I:%M:%S %p")
merged_data$dob <- format(merged_data$dob, "%m/%d/%Y")

merged_data$isolat_date <- as.Date(merged_data$isolat_date,
                                   format = "%m/%d/%y")
merged_data$received_date <- as.Date(merged_data$received_date,
                                     format = "%m/%d/%y")
merged_data$pulse_net_upload_date <- as.Date(merged_data$pulse_net_upload_date,
                                             format = "%m/%d/%y")


# Order final data by matching ids with pulsenet data
ordered_data <- merged_data[match(pulsenet$key, merged_data$key), ]
## Write out data
# Change file name!
write.xlsx(ordered_data, "G:/MICRO/MOLECULAR LABORATORY/PFGEProgram/WGS/Epi reports/2024/PN 2.0 Reports/EHEC R results.xlsx")

