### Logic Checks
# Note: check if need to add rule for: highest_level_of_schooling_completed fm_highest_level_of_schooling_completed
# See if any checks can be added for: pregnant	breastfeeding	have_a_child_under_2. A person is not likely to be pregnant and also breastfeeding at the time
# which_household_members_participated

# Check: Investigate if we have female interviews when male didn't gave consent or vice versa
#

## Beneficiary Verification ------------------------------------------------------------------------
# Logic checks
BV_logical_issues <- rbind(
  # Gender
  benef_ver_approved$data %>% 
    filter(Interview_Type_SV %in% c("Interview with female members of the HH") & (gender_respondent0 %in% "Male" | gender_respondent  %in% "Male")) %>% 
    mutate(issue="Interview type and respondent gender are inconsistent",
           Questions = "Interview_Type_SV - gender_respondent0 - gender_respondent",
           Values = paste0(Interview_Type_SV, " - ", gender_respondent0, " - ", gender_respondent)) %>%
    select(Questions, Values, issue, KEY),
  # Age vs Education
  benef_ver_approved$Male_Members_Details %>% 
    filter(highest_level_of_schooling %in% c("Post-graduate (Grade 17 and above)") & male_hhm_age < 24) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "highest_level_of_schooling - male_hhm_age",
           Values = paste0(highest_level_of_schooling, " - ", male_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Male_Members_Details %>% 
    filter(highest_level_of_schooling %in% c("Teacher college (Grade 13-14)", "Technical college (Grade 13-14)", "University (Grade 13-16)") & male_hhm_age < 19) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "highest_level_of_schooling - male_hhm_age",
           Values = paste0(highest_level_of_schooling, " - ", male_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Male_Members_Details %>% 
    filter(highest_level_of_schooling %in% "Upper Secondary (Grade 10-12)" & male_hhm_age < 16) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "highest_level_of_schooling - male_hhm_age",
           Values = paste0(highest_level_of_schooling, " - ", male_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Male_Members_Details %>% 
    filter(which_grade %in% c("11.0", "10.0", "12.0") & male_hhm_age < 14) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "which_grade - male_hhm_age",
           Values = paste0(which_grade, " - ", male_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Male_Members_Details %>% 
    filter(which_grade %in% c("Teacher college (grade 13-14)", "University (13-16)") & male_hhm_age < 18) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "which_grade - male_hhm_age",
           Values = paste0(which_grade, " - ", male_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  # Female Members
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_highest_level_of_schooling %in% c("Post-graduate (Grade 17 and above)") & female_hhm_age < 25) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_highest_level_of_schooling - female_hhm_age",
           Values = paste0(fm_highest_level_of_schooling, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_highest_level_of_schooling %in% c("Teacher college (Grade 13-14)", "Technical college (Grade 13-14)", "University (Grade 13-16)") & female_hhm_age < 20) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_highest_level_of_schooling - female_hhm_age",
           Values = paste0(fm_highest_level_of_schooling, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_highest_level_of_schooling %in% "Upper Secondary (Grade 10-12)" & female_hhm_age < 16) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_highest_level_of_schooling - female_hhm_age",
           Values = paste0(fm_highest_level_of_schooling, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_which_grade %in% c("11.0", "10.0", "12.0") & female_hhm_age < 14) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_which_grade - female_hhm_age",
           Values = paste0(fm_which_grade, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_which_grade %in% c("Teacher college (grade 13-14)", "University (13-16)") & female_hhm_age < 18) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_which_grade - female_hhm_age",
           Values = paste0(fm_which_grade, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_which_grade %in% c("11.0", "10.0", "12.0") & female_hhm_age < 14) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_which_grade - female_hhm_age",
           Values = paste0(fm_which_grade, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_which_grade %in% c("Teacher college (grade 13-14)", "University (13-16)") & female_hhm_age < 18) %>% 
    mutate(issue="Member Age and education level does not match, please double-check!",
           Questions = "fm_which_grade - female_hhm_age",
           Values = paste0(fm_which_grade, " - ", female_hhm_age)) %>%
    select(Questions, Values, issue, KEY),
  
  
  # Inconsistent Values
  benef_ver_approved$data %>% 
    filter((gender_respondent0	%in% "Female" & resp_role_in_hh %in% c("Father of household head",
                                                                     "Father-in-law of household head",
                                                                     "Husband of household head",
                                                                     "Brother of household head",
                                                                     "Uncle of household head",
                                                                     "Son of household head")) | 
             (gender_respondent0	%in% "Male" & resp_role_in_hh %in% c("Mother of household head",
                                                                      "Mother-in-law of household head",
                                                                      "Wife of household head",
                                                                      "Sister of household head",
                                                                      "Aunt of household head",
                                                                      "Daughter of household head"))) %>% 
    mutate(issue="Inconsistent Values!",
           Questions = "gender_respondent0 - resp_role_in_hh",
           Values = paste0(gender_respondent0, " - ", resp_role_in_hh)) %>%
    select(Questions, Values, issue, KEY),
  # Different Genders for same respondent
  benef_ver_approved$data %>% 
    filter((gender_respondent0 %in% "Male" & gender_respondent %in% "Female") |
             (gender_respondent0 %in% "Female" & gender_respondent %in% "Male") ) %>% 
    mutate(issue="Inconsistent Values!",
           Questions = "gender_respondent0 - gender_respondent",
           Values = paste0(gender_respondent0, " - ", gender_respondent)) %>%
    select(Questions, Values, issue, KEY),
  # Head of household but said no you can't speak to HHH
  benef_ver_approved$data %>% 
    filter(speak_to_hh %in% "No" & resp_role_in_hh %in% "I am the household head/myself") %>% 
    mutate(issue="Inconsistent Values!",
           Questions = "speak_to_hh - resp_role_in_hh",
           Values = paste0(speak_to_hh, " - ", resp_role_in_hh)) %>%
    select(Questions, Values, issue, KEY),
  
  # Inconsistent Gender of HHH
  benef_ver_approved$data %>% 
    filter(speak_to_hh %in% "Yes, I am the household head" & gender_respondent0 %in% "Female" & gender_hh %in% "Male") %>% 
    mutate(issue="Inconsistent Values!",
           Questions = "speak_to_hh - gender_respondent0 - gender_hh",
           Values = paste0(speak_to_hh, " - ", gender_respondent0, " - ", gender_hh)) %>%
    select(Questions, Values, issue, KEY),
  
  
  benef_ver_approved$Female_Members_Details %>% 
    filter(fm_married_divorced_widowed_label != fm_married_divorced_widowed & fm_married_divorced_widowed_label %notin% "Single") %>% 
    mutate(issue="Inconsistent Values!",
           Questions = "fm_married_divorced_widowed_label - fm_married_divorced_widowed",
           Values = paste0(fm_married_divorced_widowed_label, " - ", fm_married_divorced_widowed)) %>%
    select(Questions, Values, issue, KEY)

  
)


# Checking Response Consistency --------------------------------------------------------------------
# Male_Members_Details vs male_household_members sheet
Inconsistent_male_info <- benef_ver_approved$Male_Members_Details %>% 
  select(PARENT_KEY, indx=indx_male, male_hhm_name_pre=male_hhm_name, male_hhm_age_pre=male_hhm_age) %>% 
  pivot_longer(-c(PARENT_KEY, indx), names_to = "Question", values_to = "Values_Male_Member", values_transform = as.character)  %>% 
  left_join(
    benef_ver_approved$male_household_members %>% 
      select(PARENT_KEY, indx,	male_hhm_name_pre, male_hhm_age_pre) %>% 
      pivot_longer(-c(PARENT_KEY, indx), names_to = "Question", values_to = "Values_male_hh_mem", values_transform = as.character),
    by=c("PARENT_KEY", "indx", "Question")
  ) %>% filter(Values_Male_Member!=Values_male_hh_mem)

# Female_Members_Details vs female_household_member sheet
Inconsistent_female_info <- benef_ver_approved$Female_Members_Details %>% 
  select(PARENT_KEY, indx_female_pre=indx_female, female_hhm_name_pre=female_hhm_name, 
         female_hhm_age_pre=female_hhm_age, fm_married_divorced_widowed_pre=fm_married_divorced_widowed) %>% 
  pivot_longer(-c(PARENT_KEY, indx_female_pre), names_to = "Question", values_to = "Values_Female_Member", values_transform = as.character)  %>% 
  left_join(
    benef_ver_approved$female_household_member %>% 
      select(PARENT_KEY, indx_female_pre,	female_hhm_name_pre, female_hhm_age_pre,	fm_married_divorced_widowed_pre) %>% 
      pivot_longer(-c(PARENT_KEY, indx_female_pre), names_to = "Question", values_to = "Values_female_hh_mem", values_transform = as.character),
    by=c("PARENT_KEY", "indx_female_pre", "Question")
  ) %>% filter(Values_Female_Member!=Values_female_hh_mem)


# Data Checks --------------------------------------------------------------------------------------
# benef_ver_approved$data %>% count(Site_Visit_ID) 
# benef_ver_approved$data %>% 
#   count(Site_Visit_ID, Interview_Type_SV, open_closed_hh)

# Missing Interviews
Missing_interview_type <- benef_ver_approved$data %>% 
  group_by(Site_Visit_ID) %>% 
  summarize(Missing_Interview_Type = paste(setdiff(c("Interview with female members of the HH", "Interview with HHH/MKP"), 
                                                   unique(Interview_Type_SV)), collapse = "-"),
            Interview_Type_SV=paste0(Interview_Type_SV, collapse = " - ")) %>% ungroup() %>% 
  relocate(Missing_Interview_Type, .after=Interview_Type_SV) %>% 
  filter(Missing_Interview_Type %notin% c("", NA))

inconsistent_availability <- benef_ver_approved$data %>% 
  group_by(Site_Visit_ID) %>% 
  summarize(Interview_Type_SV=paste0(Interview_Type_SV, collapse = " - "),
            open_closed_hh=paste0(open_closed_hh, collapse = " - ")) %>% 
  ungroup() %>% #count(open_closed_hh)
  filter(open_closed_hh %in% c("Door is closed or no male/female is available for the interview - Yes", 
                               "Yes - Door is closed or no male/female is available for the interview")) %>% 
  mutate(Remarks="In one of the interview types it says the repondent is availabel but in other it says Door is closed. Please check if it's an issue!")
  
inconsistent_closed_type <- benef_ver_approved$data %>% 
  group_by(Site_Visit_ID) %>% 
  summarize(Interview_Type_SV=paste0(Interview_Type_SV, collapse = " - "),
            open_closed_hh=paste0(open_closed_hh, collapse = " - "),
            closed_temp_perm=paste0(closed_temp_perm, collapse = " - ")) %>% 
  ungroup() %>% # count(open_closed_hh, closed_temp_perm) %>% View
  filter(closed_temp_perm %in% c("Temporarily - Permanently", "Permanently - Temporarily")) %>% 
  mutate(Remarks="In one interview it says the house is temporarily closed but in the other permenantly!") 

  


# Repeat Sheet mismatch ----------------------------------------------------------------------------
repeatsheet_mismatch <- rbind(
  # scope_card_hh_rep
  benef_ver_approved$scope_card_hh_rep %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="scope_card_hh_rep") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=how_many_scope_card_hh, KEY) %>%
                mutate(Sheet="scope_card_hh_rep", Question="how_many_scope_card_hh"), by=c("KEY", "Sheet")),
  # male_household_members
  benef_ver_approved$male_household_members %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="male_household_members") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=how_many_male_members_hh, KEY) %>%
                mutate(Sheet="male_household_members", Question="how_many_male_members_hh"), by=c("KEY", "Sheet")),
  
  # male_household_members
  benef_ver_approved$male_household_members %>%
    filter(male_hhm_age_pre>=6) %>% 
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="male_household_members") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=count_over5, KEY) %>%
                mutate(Sheet="male_household_members", Question="count_over5"), by=c("KEY", "Sheet")),
  
  # Male_Members_Details
  benef_ver_approved$Male_Members_Details %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Male_Members_Details") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=how_many_male_members_hh, KEY) %>%
                mutate(Sheet="Male_Members_Details", Question="how_many_male_members_hh"), by=c("KEY", "Sheet")),
  # female_household_member
  benef_ver_approved$female_household_member %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="female_household_member") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=fm_how_many_female_members_hh, KEY) %>%
                mutate(Sheet="female_household_member", Question="fm_how_many_female_members_hh"), by=c("KEY", "Sheet")),
  # Female_Members_Details
  benef_ver_approved$Female_Members_Details %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Female_Members_Details") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=fm_how_many_female_members_hh, KEY) %>%
                mutate(Sheet="Female_Members_Details", Question="fm_how_many_female_members_hh"), by=c("KEY", "Sheet")),
  
  # Female_Members_Details
  benef_ver_approved$Female_Members_Details %>%
    mutate(ben_not_received_sc_calculated=case_when(
      was_issued_a_scope_card %in% c("No", "Have a different WFP SCOPE card, but is not for MCBP", 
                                     "Was issued a SCOPE card but does not have it now",
                                     "I don't know", "Don’t wish to respond") & benef_preg_bf_child %in% "Yes" ~ "Yes",
      TRUE ~ "No")) %>% 
    filter(ben_not_received_sc_calculated!="No") %>% 
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Female_Members_Details") %>%
    full_join(benef_ver_approved$data %>% select(main_sheet_count=n_of_ben_not_received_sc, KEY) %>%
                mutate(Sheet="Female_Members_Details", Question="n_of_ben_not_received_sc"), by=c("KEY", "Sheet"))
) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & !(is.na(repeat_sheet_count) & main_sheet_count == 0)) %>% 
  mutate(Tool="Beneficiary_Verification")

# Export list --------------------------------------------------------------------------------------
logical_issues <- plyr::rbind.fill(
  BV_logical_issues
)

# ## Check and filter QA commented issues
# logical_issues <- logical_issues %>% 
#   left_join(
#     QA_comments_issues %>% select(Questions, Values, issue, KEY, QA_Remarks),
#     by=c("Questions", "Values", "issue", "KEY")
#   ) %>% 
#   filter(is.na(QA_Remarks))


logical_issues_list <- list(
  logical_issues=logical_issues,
  Inconsistent_male_info=Inconsistent_male_info,
  Inconsistent_female_info=Inconsistent_female_info,
  repeatsheet_mismatch=repeatsheet_mismatch,
  Missing_interview_type=Missing_interview_type,
  inconsistent_availability=inconsistent_availability,
  inconsistent_closed_type=inconsistent_closed_type
)

# Remove extra objects -----------------------------------------------------------------------------
rm()


