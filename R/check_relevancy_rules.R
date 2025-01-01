# Check Relevancy Rules ----------------------------------------------------------------------------
## Read
BV_tool_relevancy <- read_excel("input/tool_relevancy_rules/MCBP_BV_relevancy_rules.xlsx")
elder_tool_relevnacy <- read_excel("input/tool_relevancy_rules/MCBP_CEC_relevancy_rules.xlsx")
  
### Beneficiary Verification
## Join main columns with repeat sheets
BV_data_sub <- benef_ver$data %>%
  select(Surveyor_Name, consent_hhh_mkp, consent_fm, Interview_Type_SV, how_many_scope_card_hh,
         how_many_male_members_hh, fm_how_many_female_members_hh, KEY)
scope_card_joined <- benef_ver$scope_card_hh_rep %>%
  left_join(BV_data_sub, by=c("PARENT_KEY"="KEY"))
male_hh_joined <- benef_ver$male_household_members %>%
  left_join(BV_data_sub, by=c("PARENT_KEY"="KEY"))
Male_mem_joined <- benef_ver$Male_Members_Details %>%
  left_join(BV_data_sub, by=c("PARENT_KEY"="KEY"))
female_hh_joined <- benef_ver$female_household_member %>%
  left_join(BV_data_sub, by=c("PARENT_KEY"="KEY"))
Female_mem_joined <- benef_ver$Female_Members_Details %>%
  left_join(BV_data_sub, by=c("PARENT_KEY"="KEY"))


BV_relevancy_issues <- rbind(
  check_relevancy_rules(benef_ver$data, BV_tool_relevancy, sheet_name="data"),
  check_relevancy_rules(scope_card_joined, BV_tool_relevancy, sheet_name="scope_card_hh_rep"),
  check_relevancy_rules(male_hh_joined, BV_tool_relevancy, sheet_name="male_household_members"),
  check_relevancy_rules(Male_mem_joined, BV_tool_relevancy, sheet_name="Male_Members_Details"),
  check_relevancy_rules(female_hh_joined, BV_tool_relevancy, sheet_name="female_household_member"),
  check_relevancy_rules(Female_mem_joined, BV_tool_relevancy, sheet_name="Female_Members_Details")
)

BV_relevancy_issues <- BV_relevancy_issues %>%
  mutate(key = str_split_fixed(KEY, "/", 2)[,1], .after = KEY) %>%
  left_join(select(benef_ver$data, qa_status, key=KEY)) %>% 
  filter(question %notin% "name_of_hh_QA_purpose")



## Community Elder Confirmation
elder_relevancy_issues <- check_relevancy_rules(comm_elder, elder_tool_relevnacy, sheet_name="data")

# Update Select_multiple series columns ------------------------------------------------------------
## Beneficiary Verification
BV_SM_issues <- c()
for(sheet in benef_sheets){
  benef_ver[[sheet]] <- benef_ver[[sheet]] %>%
    update_series_cols(tool_path = benef_tool_path,
                       question_separator="_")
  # Check if updated correctly
  BV_SM_issues <- rbind(
    BV_SM_issues,
    check_select_multiple(data=benef_ver[[sheet]],
                          tool_path = benef_tool_path,
                          question_separator="_")
  )
}

## Export List -------------------------------------------------------------------------------------
# Relevancy
relevancy_issues <- plyr::rbind.fill(
  BV_relevancy_issues %>% mutate(Tool="Beneficiary_Verification"),
  elder_relevancy_issues %>% mutate(Tool="Community_Elder")
) 

## Select Multiple issues
SM_issues <- list(
  BV_SM_issues=BV_SM_issues
)

# remove extra objects -----------------------------------------------------------------------------
rm(BV_data_sub, scope_card_joined, male_hh_joined, Male_mem_joined, female_hh_joined, Female_mem_joined)

