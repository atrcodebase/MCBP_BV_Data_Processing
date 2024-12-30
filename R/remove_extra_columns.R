### Extra columns
extra_cols <- read_excel("input/extra_columns.xlsx", sheet="extra_columns")
extra_cols %>% count(Tool, Sheet)

unnecessary_cols <- benef_ver_approved$data %>% select(starts_with("passcode_correct[1]"), starts_with("SET-OF-"), ends_with("_Caption")) %>% names()

# Temp: including all PII for internal use only
extra_cols <- extra_cols %>% filter(Reason_for_removal %notin% "PII")

## Remove Extra columns ----------------------------------------------------------------------------
## Beneficiary Verification
for(sheet in benef_sheets){
  benef_ver_approved[[sheet]] <- benef_ver_approved[[sheet]] %>%
    select(-any_of(c(extra_cols$questions[extra_cols$Tool %in% "Beneficiary_Verification" & extra_cols$Sheet %in% sheet], unnecessary_cols)))
}

# remove extra objects -----------------------------------------------------------------------------
rm(extra_cols)


