### Filter Approved data for client
benef_ver_approved <- benef_ver

## Beneficiary Verification
benef_ver_approved$data <- benef_ver_approved$data %>% filter(qa_status %in% approved_qa_status)

benef_ver_approved$scope_card_hh_rep <- benef_ver_approved$scope_card_hh_rep %>% 
  filter(PARENT_KEY %in% benef_ver_approved$data$KEY)
benef_ver_approved$male_household_members <- benef_ver_approved$male_household_members %>% 
  filter(PARENT_KEY %in% benef_ver_approved$data$KEY)
benef_ver_approved$Male_Members_Details <- benef_ver_approved$Male_Members_Details %>% 
  filter(PARENT_KEY %in% benef_ver_approved$data$KEY)
benef_ver_approved$female_household_member <- benef_ver_approved$female_household_member %>% 
  filter(PARENT_KEY %in% benef_ver_approved$data$KEY)
benef_ver_approved$Female_Members_Details <- benef_ver_approved$Female_Members_Details %>% 
  filter(PARENT_KEY %in% benef_ver_approved$data$KEY)


## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)

