# Log missing audio translation and missing image QA -----------------------------------------------
## Beneficiary Verification
benef_tool <- read_excel(benef_tool_path, "survey", guess_max = 100000)
benef_audio_cols <- benef_tool %>% filter(type %in% c("audio")) %>% pull(name) #  & name %in% names(impact_evaluation_approved$data)
benef_image_cols <- benef_tool %>% filter(type %in% c("image")) %>% pull(name)

image_cols_diff <- list(
  "doortag_photo"="doortag_photo_QA",
  "doortag_photo2"="doortag_photo2_QA",
  "Selfie_With_Door"="Selfie_With_Door_QA"
)

benef_missing_log <- rbind(
  # Translation
  log_questions(data=benef_ver_approved$data,
                columns=benef_audio_cols[benef_audio_cols %in% names(benef_ver_approved$data)],
                suffix="translation", sheet="data"),
  log_questions(data=benef_ver_approved$scope_card_hh_rep,
                columns=benef_audio_cols[benef_audio_cols %in% names(benef_ver_approved$scope_card_hh_rep)],
                suffix="translation", sheet="scope_card_hh_rep"),
  log_questions(data=benef_ver_approved$male_household_members,
                columns=benef_audio_cols[benef_audio_cols %in% names(benef_ver_approved$male_household_members)],
                suffix="translation", sheet="male_household_members"),
  log_questions(data=benef_ver_approved$Male_Members_Details,
                columns=benef_audio_cols[benef_audio_cols %in% names(benef_ver_approved$Male_Members_Details)],
                suffix="translation", sheet="Male_Members_Details"),
  log_questions(data=benef_ver_approved$female_household_member,
                columns=benef_audio_cols[benef_audio_cols %in% names(benef_ver_approved$female_household_member)],
                suffix="translation", sheet="female_household_member"),
  log_questions(data=benef_ver_approved$Female_Members_Details,
                columns=benef_audio_cols[benef_audio_cols %in% names(benef_ver_approved$Female_Members_Details)],
                suffix="translation", sheet="Female_Members_Details"),
  
  # Image QA
  log_questions(data=benef_ver_approved$data, 
                columns=benef_image_cols[benef_image_cols %in% names(benef_ver_approved$data)], 
                suffix="qa", sheet="data", columns_different=image_cols_diff),
  log_questions(data=benef_ver_approved$scope_card_hh_rep, 
                columns=benef_image_cols[benef_image_cols %in% names(benef_ver_approved$scope_card_hh_rep)], 
                suffix="qa", sheet="scope_card_hh_rep"),
  log_questions(data=benef_ver_approved$male_household_members, 
                columns=benef_image_cols[benef_image_cols %in% names(benef_ver_approved$male_household_members)], 
                suffix="qa", sheet="male_household_members"),
  log_questions(data=benef_ver_approved$Male_Members_Details, 
                columns=benef_image_cols[benef_image_cols %in% names(benef_ver_approved$Male_Members_Details)], 
                suffix="qa", sheet="Male_Members_Details"),
  log_questions(data=benef_ver_approved$female_household_member, 
                columns=benef_image_cols[benef_image_cols %in% names(benef_ver_approved$female_household_member)], 
                suffix="qa", sheet="female_household_member"),
  log_questions(data=benef_ver_approved$Female_Members_Details, 
                columns=benef_image_cols[benef_image_cols %in% names(benef_ver_approved$Female_Members_Details)], 
                suffix="qa", sheet="Female_Members_Details")
  )

## Log Missing Translation -------------------------------------------------------------------------
excluded_cols <- c("Village", "Address", "first_name_of_hh", "last_name_of_hh", "fathers_name_hh",
                   "male_hhm_name_pre", "male_hhm_name", "female_hhm_name_pre", "female_hhm_name",
                   "fm_respondent_lastname",  "fm_respondent_fathername", "fm_respondent_registered_name",
                   "fm_respondent_registered_father_name", "fm_respondent_registered_why_diff_name_fathername", 
                   "name_youngest_child_under_2", "name_respondent", "name_of_hh_QA_purpose")

missing_translation_log <- rbind(
  missing_translation(data = benef_ver_approved$data, KEY = "KEY", excluded_cols),
  missing_translation(data = benef_ver_approved$scope_card_hh_rep, KEY = "KEY", excluded_cols),
  missing_translation(data = benef_ver_approved$male_household_members, KEY = "KEY", excluded_cols),
  missing_translation(data = benef_ver_approved$Male_Members_Details, KEY = "KEY", excluded_cols),
  missing_translation(data = benef_ver_approved$female_household_member, KEY = "KEY", excluded_cols),
  missing_translation(data = benef_ver_approved$Female_Members_Details, KEY = "KEY", excluded_cols)
  ) %>% mutate(Tool = "Beneficiary_Verification")


## Export List -------------------------------------------------------------------------------------
missing_translation_QA_log <- rbind(
  benef_missing_log %>% mutate(Tool = "Beneficiary_Verification")
  )

## Separate translation and image logs
missing_translation_QA_log_sub <- missing_translation_QA_log %>% 
  filter(question_type == "translation")

# Export list
missing_translation_QA_log <- list(
  Image_log=filter(missing_translation_QA_log, question_type=="qa_status"),
  Audio_log=missing_translation_QA_log_sub
)

# remove extra objects -----------------------------------------------------------------------------
rm(benef_tool, benef_image_cols, benef_audio_cols, missing_translation_QA_log_sub)

