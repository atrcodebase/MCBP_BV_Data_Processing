### Remove Rejected QA status and keys -------------------------------------------------------------
rejected_qa_status <- "Rejected"
scto_rejected <- "REJECTED"

## Beneficiary Verification
benef_ver$data <- benef_ver$data %>% 
  filter(qa_status %notin% rejected_qa_status &
           KEY %notin% rejection_log$KEY_Unique &
           review_status %notin% scto_rejected)
# scope_card_hh_rep
benef_ver$scope_card_hh_rep <- benef_ver$scope_card_hh_rep %>%
  filter(PARENT_KEY %in% benef_ver$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
# male_household_members
benef_ver$male_household_members <- benef_ver$male_household_members %>%
  filter(PARENT_KEY %in% benef_ver$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
# male_household_members
benef_ver$male_household_members <- benef_ver$male_household_members %>%
  filter(PARENT_KEY %in% benef_ver$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
# Male_Members_Details
benef_ver$Male_Members_Details <- benef_ver$Male_Members_Details %>%
  filter(PARENT_KEY %in% benef_ver$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
# female_household_member
benef_ver$female_household_member <- benef_ver$female_household_member %>%
  filter(PARENT_KEY %in% benef_ver$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
# Female_Members_Details
benef_ver$Female_Members_Details <- benef_ver$Female_Members_Details %>%
  filter(PARENT_KEY %in% benef_ver$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)


## Community Elder Confirmation
comm_elder <- comm_elder %>%
  filter(qa_status %notin% rejected_qa_status &
           KEY %notin% rejection_log$KEY_Unique &
           review_status %notin% scto_rejected)

## Remove extra objects ----------------------------------------------------------------------------
rm(rejected_qa_status, scto_rejected)

