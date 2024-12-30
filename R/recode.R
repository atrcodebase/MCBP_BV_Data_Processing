# Change/Recode variables
relabel_98_99 <- function(x) {
  x = case_when(
    x %in% c(9999, "9999") ~ "Refuse to respond",
    x %in% c(8888, "8888") ~ "I don't know",
    TRUE ~ as.character(x)
  )}

# Numeric cols
benef_numeric_cols <- read_excel(benef_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name)

## Beneficiary Verificatio -------------------------------------------------------------------------
benef_ver$data <- benef_ver$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")) %>% 
  mutate(gender_respondent = case_when(
    gender_respondent %in% 1 ~ "Male",
    gender_respondent %in% 2 ~ "Female",
    TRUE ~ as.character(gender_respondent)
  ))
benef_ver$Female_Members_Details <- benef_ver$Female_Members_Details %>%
  mutate(fm_married_divorced_widowed = case_when(
    fm_married_divorced_widowed %in% c(1) ~ "Married",
    fm_married_divorced_widowed %in% c(2) ~ "Divorced",
    fm_married_divorced_widowed %in% c(3) ~ "Widowed",
    fm_married_divorced_widowed %in% c(0) ~ "None of the above",
    TRUE ~ as.character(fm_married_divorced_widowed)
  ),
  cf_code=case_when(
    cf_code %in% 1 ~ "Yes",
    cf_code %in% 0 ~ "No",
    TRUE ~ as.character(cf_code)
  ),
  benef_preg_bf_child=case_when(
    benef_preg_bf_child %in% 1 ~ "Yes",
    benef_preg_bf_child %in% 0 ~ "No",
    TRUE ~ as.character(benef_preg_bf_child)
  ))

# Main sheet subset
benef_sub <- benef_ver$data %>% 
  select(Site_Visit_ID, Province, District, Village, Region, Interview_Type_SV, KEY)

# Apply changes on all sheets
for(sheet in names(benef_ver)){
  # Update links
  key_col <- ifelse(sheet %in% "data", "KEY", "PARENT_KEY")
  benef_ver[[sheet]] <- update_media_links(data=benef_ver[[sheet]], 
                                            tool_path = benef_tool_path, 
                                            download_link=download_link,
                                            key_col) # No need if data is downloaded from SCTO website
  
  # Join main Sheet cols
  if(sheet!="data"){
    benef_ver[[sheet]] <- benef_ver[[sheet]] %>% 
      left_join(benef_sub, by=c("PARENT_KEY"="KEY")) %>% 
      relocate(Site_Visit_ID:Interview_Type_SV, .before = 1)
  }
}

# remove extra objects -----------------------------------------------------------------------------
rm(relabel_98_99, benef_numeric_cols, benef_sub)

