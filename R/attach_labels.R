# apply the value labels

## Beneficiary Verification ------------------------------------------------------------------------
benef_ver$data <- labeler(data = benef_ver$data,
                           tool = benef_tool_path,
                           survey_label = "label",
                           choice_lable = "label",
                           multi_response_sep = ";")
# scope_card_hh_rep
benef_ver$scope_card_hh_rep <- labeler(data = benef_ver$scope_card_hh_rep,
                          tool = benef_tool_path,
                          survey_label = "label",
                          choice_lable = "label",
                          multi_response_sep = ";")
# male_household_members
benef_ver$male_household_members <- labeler(data = benef_ver$male_household_members,
                          tool = benef_tool_path,
                          survey_label = "label",
                          choice_lable = "label",
                          multi_response_sep = ";")
# Male_Members_Details
benef_ver$Male_Members_Details <- labeler(data = benef_ver$Male_Members_Details,
                          tool = benef_tool_path,
                          survey_label = "label",
                          choice_lable = "label",
                          multi_response_sep = ";")
# female_household_member
benef_ver$female_household_member <- labeler(data = benef_ver$female_household_member,
                          tool = benef_tool_path,
                          survey_label = "label",
                          choice_lable = "label",
                          multi_response_sep = ";")
# Female_Members_Details
benef_ver$Female_Members_Details <- labeler(data = benef_ver$Female_Members_Details,
                                             tool = benef_tool_path,
                                             survey_label = "label",
                                             choice_lable = "label",
                                             multi_response_sep = ";")

# remove extra objects -------------------------------------------
rm()

