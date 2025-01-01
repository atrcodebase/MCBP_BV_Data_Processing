##### Data Processing Script #####
# Install/load required packages -------------------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(janitor)) install.packages("janitor")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")

source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Declaring Global Variables -----------------------------------------------------------------------
benef_tool_path <- "input/tools/MCBP+Beneficiary+Verification+Tool.xlsx"
elder_tool_path <- "input/tools/MCBP+Community+Elder+Confirmation+Tool.xlsx"
benef_sheets <- c("data", "scope_card_hh_rep", "male_household_members", "Male_Members_Details", 
               "female_household_member", "Female_Members_Details")
# Survey CTO Download link extension
download_link <- "https://artftpm.surveycto.com/view/submission-attachment/"

# Read data ----------------------------------------------------------------------------------------
# Beneficiary Verification
benef_ver = read_xlsx_sheets("input/raw_data/MCBP Beneficiary Verification Tool.xlsx")
comm_elder = read_excel("input/raw_data/MCBP Community Elder Confirmation Tool.xlsx", guess_max = 100000, na = c("N/A", "-", " "))

# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQBw0D-xoQ9QGKbeT6hxrVtCmdOiT4ZlUmhQeBrN64jQcyBAla4yLQQR1aR560xgRgP34a1ffMstmOM/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=169480259&single=true&output=csv"), col_types = "c")
detailed_check <- readr::read_csv(paste0(url, "gid=614089188&single=true&output=csv"), col_types = "c", guess_max = 500000)
correction_log <- readr::read_csv(paste0(url, "gid=946969657&single=true&output=csv"), col_types = "c")
# translation_log <- readr::read_csv(paste0(url, "gid=1671390620&single=true&output=csv"), col_types = "c")
rejection_log <- readr::read_csv(paste0(url, "gid=356272294&single=true&output=csv"), col_types = "c")

# Cross-check sheet
crosscheck_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSkoJ08XEiJNIPby7p5iyLCq9oSEF50Syk989BB5ip6BCBGpgxrnkcIZpj4qx0XdcjEVK1Qzs38QK2L/pub?gid=1343596752&single=true&output=csv", col_types = "c")
# crosscheck_log %>% count(Found_In_WFP_List, Beneficiary_Category)

# Join QA Status -----------------------------------------------------------------------------------
count(qa_log, Tool, QA_Status)
qa_log_sub <- qa_log %>%
  select(KEY=KEY_Unique, qa_status=QA_Status) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status
  )) %>% unique()

## Beneficiary Verification
benef_ver$data <- benef_ver$data %>%
  left_join(filter(qa_log_sub), by="KEY") #%>% select(-Tool)
## Community Elder Confirmation
comm_elder <- comm_elder %>%
  left_join(filter(qa_log_sub), by="KEY") #%>% select(-Tool)

benef_ver$data %>% count(qa_status)

# apply correction log -----------------------------------------------------------------------------
correction_log %>% count(Tab_Name)
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R") # Add Community rep if needed
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Remove Rejected data ----------------------------------------------------------------------------
# # file.edit("R/remove_rejected_data.R")
source("R/remove_rejected_data.R")

# Relevancy check ----------------------------------------------------------------------------------
# file.edit("R/check_relevancy_rules.R")
source("R/check_relevancy_rules.R")

## Attach labels -----------------------------------------------------------------------------------
# file.edit("R/attach_labels.R")
source("R/attach_labels.R")

# # apply Translation log ----------------------------------------------------------------------------
# translation_log %>% count(Tab_Name)
# # file.edit("R/apply_translation_log.R")
# source("R/apply_translation_log.R")
# if(nrow(translation_log_discrep) !=0){
#   print("Correction Logs not applied -------------------")
#   correction_log_discrep
# }

## Recode ------------------------------------------------------------------------------------------
# file.edit("R/recode.R") 
source("R/recode.R") # Update here

# produce qa-backlog -------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(qa_status=QA_Status, KEY=KEY_Unique) %>% mutate(Tool="Beneficiary_Verification")
## Filter
QA_backlog_keys <- rbind(
  # Beneficiary_Verification
  benef_ver$data %>% 
    select(SubmissionDate, KEY) %>%
    left_join(qa_log_sub, by = "KEY") %>% 
    mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="Beneficiary_Verification")
  ) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>%
  filter(qa_status %notin% c("Approved", "Rejected")) # Filter Keys not yet QAed
# Count
QA_backlog <- QA_backlog_keys %>%
  group_by(SubmissionDate, Tool) %>% count(qa_status, name = "freq") %>%
  # mutate(percentage = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% arrange(SubmissionDate) %>%
  pivot_wider(names_from = "Tool", values_from = "freq")
# Print
print(knitr::kable(QA_backlog, format = "simple"))

## Filter Approved data ----------------------------------------------------------------------------
count(qa_log, QA_Status)
approved_qa_status <- c("Approved")
# # file.edit("R/filter_approved_data.R")
source("R/filter_approved_data.R")

## Logic check -------------------------------------------------------------------------------------
# file.edit("R/logic_check.R")
source("R/logic_check.R") # check the age and education level LCs

## Compare dataset responses with the Tools --------------------------------------------------------
# file.edit("R/dataset_responses_check.R")
source("R/dataset_responses_check.R")

## Remove Extra columns ----------------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/remove_extra_columns.R") # Check NA columns in the end # ** Remove PII

# generate data with missing translations ----------------------------------------------------------
# file.edit("R/check_missing_translation.R")
source("R/check_missing_translation.R") # Temporary filter for QA at the end

# Anyonimize Client Data ---------------------------------------------------------------------------
# file.edit("R/modify_client_data.R")
# source("R/modify_client_data.R")

# Beneficiary Matching -----------------------------------------------------------------------------
# file.edit("beneficiary_matching.R")
# source("beneficiary_matching.R")

# Export -------------------------------------------------------------------------------------------
## QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)

## export cleaned datasets
check_path("output/cleaned_data") # create the output path
archive_datasets("output/cleaned_data") # Move previous datasets to Archive
writexl::write_xlsx(benef_ver, paste0("output/cleaned_data/MCBP_Beneficiary_Verification_", lubridate::today(), ".xlsx"))
writexl::write_xlsx(comm_elder, paste0("output/cleaned_data/MCBP_Community_Elder_Confirmation_", lubridate::today(), ".xlsx"))

## export client datasets
check_path("output/client_data") # create the output path
archive_datasets("output/client_data") # Move previous datasets to Archive
export_datasets(benef_ver_approved, paste0("output/client_data/MCBP_Beneficiary_Verification_", lubridate::today(), ".xlsx"))
export_datasets(list(data=comm_elder_approved), paste0("output/client_data/MCBP_Community_Elder_Confirmation_", lubridate::today(), ".xlsx"))

## export additional files
writexl::write_xlsx(correction_log, "output/correction_log.xlsx", format_headers = F) # correction
writexl::write_xlsx(correction_log_issues, "output/correction_log_issues.xlsx", format_headers = F) # correction log issues
# writexl::write_xlsx(translation_log_issues, "output/translation_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)
writexl::write_xlsx(relevancy_issues, "output/relevancy_issues.xlsx", format_headers = F)
writexl::write_xlsx(SM_issues, "output/Select_multiple_issues.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_QA_log, "output/Missing_audio_translation_&_image_QA.xlsx", format_headers = F)
writexl::write_xlsx(qa_backlog_list, "output/QA_backlog.xlsx", format_headers = F)
writexl::write_xlsx(response_log_list, "output/dataset_response_mismatch_with_tool.xlsx", format_headers = F)
writexl::write_xlsx(logical_issues_list, "output/Logical_issues.xlsx", format_headers = F) # Add later

