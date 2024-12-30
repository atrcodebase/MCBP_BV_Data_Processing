# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
tabs <- benef_sheets
# tool_names <- c()
sm_variables <- read_excel(benef_tool_path) %>% filter(grepl("select_multiple", type)) %>% pull(name)

## Filter empty rows
correction_log_filtered <- correction_log %>%
  filter(!(is.na(KEY_Unique) & is.na(Question) & is.na(old_value))) %>%
  mutate(New_Value = case_when(
    Question %in% sm_variables ~ str_replace_all(New_Value, "-|,|  | - ", " ") %>% str_squish(),
    TRUE ~ str_squish(New_Value)
  ),
  Tools = "Beneficiary_Verification", Log_type = "Correction_Log" # QA: logs only added for IE & not representative
  # KEY= case_when(
  #   is.na(KEY) & !is.na(`Full_ KEY`) ~ str_squish(`Full_ KEY`),
  #   TRUE ~ str_squish(KEY)
  # )
  ) %>% 
  select(key=KEY, KEY=KEY_Unique, Tools, Tab_Name, question=Question, old_value, 
         new_value=New_Value, QAed_by=`Logged by`, Remarks, Log_type)

# New Log: should be applied before correction log
detailed_check_log <- detailed_check %>% 
  mutate(Question=case_when(
    # The image QA status column needs to be standardized
    Check_Type %in% c("image") & Question %notin% c("doortag_photo", "doortag_photo2", "Selfie_With_Door") ~ paste0(Question, "_qa"),
    Check_Type %in% c("image") & Question %in% c("doortag_photo", "doortag_photo2", "Selfie_With_Door") ~ paste0(Question, "_QA"),
    Check_Type %in% c("audio") ~ paste0(Question,"_translation"),
    TRUE ~ as.character(Question)
  ),
  New_Value=case_when(
    Check_Status %in% "Verified" & Check_Type %in% c("audio", "image") ~ "Verified",
    Check_Status %in% "Error/Irrelevant" & Check_Type %in% c("audio", "image") ~ "Error/Irrelevant",
    TRUE ~ as.character(New_Value)
  ), Tool="Beneficiary_Verification", Log_type = "Detailed_Check") %>% 
  filter(Check_Type %notin% "audio audit") %>% # Audio Audit is only used for QA)
  filter(Check_Status %in% "Error/Irrelevant" | Check_Type %in% c("audio", "image")) %>% 
  select(key=KEY, KEY=KEY_Unique, Tab_Name, Check_Type, `QA'ed By`, 
         question=Question, old_value=Value, new_value=New_Value, Check_Status, Tool, Log_type)

# Merge logs
correction_log_filtered <- plyr::rbind.fill(
  detailed_check_log, # should be applied first
  correction_log_filtered
)

# Identify issues
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    # is.na(Tools) & Tools %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    Tab_Name %in% "data" & question %notin% names(benef_ver$data) ~ "question",
    Tab_Name %in% "data" & KEY %notin% benef_ver$data$KEY ~ "KEY",
    Tab_Name %in% "scope_card_hh_rep" & question %notin% names(benef_ver$scope_card_hh_rep) ~ "question",
    Tab_Name %in% "scope_card_hh_rep" & KEY %notin% benef_ver$scope_card_hh_rep$KEY ~ "KEY",
    Tab_Name %in% "male_household_members" & question %notin% names(benef_ver$male_household_members) ~ "question",
    Tab_Name %in% "male_household_members" & KEY %notin% benef_ver$male_household_members$KEY ~ "KEY",
    Tab_Name %in% "Male_Members_Details" & question %notin% names(benef_ver$Male_Members_Details) ~ "question",
    Tab_Name %in% "Male_Members_Details" & KEY %notin% benef_ver$Male_Members_Details$KEY ~ "KEY",
    Tab_Name %in% "female_household_member" & question %notin% names(benef_ver$female_household_member) ~ "question",
    Tab_Name %in% "female_household_member" & KEY %notin% benef_ver$female_household_member$KEY ~ "KEY",
    Tab_Name %in% "Female_Members_Details" & question %notin% names(benef_ver$Female_Members_Details) ~ "question",
    Tab_Name %in% "Female_Members_Details" & KEY %notin% benef_ver$Female_Members_Details$KEY ~ "KEY"
    )) # Add tool name based on Log names

correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("KEY", "question", "Log_type")], fromLast = T) | duplicated(correction_log_filtered[, c("KEY", "question", "Log_type")])

# Filter issues
correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question) 
# %>% # Check if this is necessary
#   mutate(question=case_when( # Temp
#     Check_Type %in% "image" ~ str_remove(question, "_qa_status$"),
#     Check_Type %in% "audio" ~ str_remove(question, "_translation$"),
#     TRUE ~ question
#   ))

correction_log_filtered <- correction_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue)) 

# Make audio translation columns character
benef_media_cols <- read_excel(benef_tool_path, "survey", guess_max = 100000)
benef_media_cols <- benef_media_cols %>%
  filter(type %in% c("audio", "image")) %>%
  mutate(name= case_when(
    type %in% c("image") & name %notin% c("doortag_photo", "doortag_photo2", "Selfie_With_Door") ~ paste0(name, "_qa"),
    type %in% c("image") & name %in% c("doortag_photo", "doortag_photo2", "Selfie_With_Door") ~ paste0(name, "_QA"),
    type %in% c("audio") ~ paste0(name,"_translation"),
  )) %>% pull(name)

benef_ver$data <- benef_ver$data %>%
  mutate(across(any_of(benef_media_cols), as.character))
benef_ver$scope_card_hh_rep <- benef_ver$scope_card_hh_rep %>%
  mutate(across(any_of(benef_media_cols), as.character))
benef_ver$male_household_members <- benef_ver$male_household_members %>%
  mutate(across(any_of(benef_media_cols), as.character))
benef_ver$Male_Members_Details <- benef_ver$Male_Members_Details %>%
  mutate(across(any_of(benef_media_cols), as.character))
benef_ver$female_household_member <- benef_ver$female_household_member %>%
  mutate(across(any_of(benef_media_cols), as.character))
benef_ver$Female_Members_Details <- benef_ver$Female_Members_Details %>%
  mutate(across(any_of(benef_media_cols), as.character))

# apply the correction-log -------------------------------------------
## Beneficiary Verification
benef_ver_copy <- benef_ver

# data
benef_ver$data <- apply_log(data = benef_ver$data, log= filter(correction_log_filtered, Tab_Name %in% "data"), 
                                    data_KEY = "KEY",
                                    log_columns = c(question = "question",
                                                    old_value = "old_value",
                                                    new_value = "new_value",
                                                    KEY = "KEY"))
# scope_card_hh_rep
benef_ver$scope_card_hh_rep <- apply_log(data = benef_ver$scope_card_hh_rep, log= filter(correction_log_filtered, Tab_Name %in% "scope_card_hh_rep"), 
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))
# male_household_members
benef_ver$male_household_members <- apply_log(data = benef_ver$male_household_members, log= filter(correction_log_filtered, Tab_Name %in% "male_household_members"), 
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))
# Male_Members_Details
benef_ver$Male_Members_Details <- apply_log(data = benef_ver$Male_Members_Details, log= filter(correction_log_filtered, Tab_Name %in% "Male_Members_Details"), 
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))
# female_household_member
benef_ver$female_household_member <- apply_log(data = benef_ver$female_household_member, log= filter(correction_log_filtered, Tab_Name %in% "female_household_member"), 
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))
# Female_Members_Details
benef_ver$Female_Members_Details <- apply_log(data = benef_ver$Female_Members_Details, log= filter(correction_log_filtered, Tab_Name %in% "Female_Members_Details"), 
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))

# Verify correction log ----------------------------------------------------------------------------
message("Verifying Correction log, please wait!")

correction_log_discrep <- rbind(
  ## beneficiary_verification
  compare_dt(df1 = benef_ver_copy$data, df2 = benef_ver$data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "beneficiary_verification", Tab_Name="data"),
  ## scope_card_hh_rep
  compare_dt(df1 = benef_ver_copy$scope_card_hh_rep, df2 = benef_ver$scope_card_hh_rep,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "beneficiary_verification", Tab_Name="scope_card_hh_rep"),
  ## male_household_members
  compare_dt(df1 = benef_ver_copy$male_household_members, df2 = benef_ver$male_household_members,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "beneficiary_verification", Tab_Name="male_household_members"),
  ## Male_Members_Details
  compare_dt(df1 = benef_ver_copy$Male_Members_Details, df2 = benef_ver$Male_Members_Details,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "beneficiary_verification", Tab_Name="Male_Members_Details"),
  ## female_household_member
  compare_dt(df1 = benef_ver_copy$female_household_member, df2 = benef_ver$female_household_member,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "beneficiary_verification", Tab_Name="female_household_member"),
  ## Female_Members_Details
  compare_dt(df1 = benef_ver_copy$Female_Members_Details, df2 = benef_ver$Female_Members_Details,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "beneficiary_verification", Tab_Name="Female_Members_Details")
  ) 

# Removing extra spaces from new_value before joining 
correction_log_discrep <- correction_log_discrep %>%
  anti_join(correction_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(benef_ver_copy, sm_variables, tabs, correction_log_filtered, benef_media_cols)



