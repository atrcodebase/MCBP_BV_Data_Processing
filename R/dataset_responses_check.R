## Check for any values in the dataset that cannot be found in the tool ---------------------------- 
## Beneficiary Verification
for(sheet in benef_sheets){
  benef_response_log <- benef_ver_approved[[sheet]] %>%
    check_responses(tool_path=benef_tool_path, sheet=sheet)
}


## Community Elder Confirmation
elder_response_log <- comm_elder %>% check_responses(tool_path=elder_tool_path, sheet="data")


# Export List
response_log_list <- rbind(
  benef_response_log %>% mutate(Tool="Beneficiary_Verification"),
  elder_response_log %>% mutate(Tool="Community_Elder"))

