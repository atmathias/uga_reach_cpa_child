library(tidyverse)
library(lubridate)
library(glue)


# read data ---------------------------------------------------------------

# tool data
data_nms <- names(readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "UGA2109_Cross-Sectoral Child...", n_max = 100))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = district_name,
         i.check.point_number = point_number,
         start = as_datetime(start),
         end = as_datetime(end)) %>% 
  filter(consent_two == "yes", respondent_age >= 12, i.check.start_date > as_date("2022-01-30"), 
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))
  )
# cleaning log
df_cleaning_log <- read_csv("inputs/combined_checks_child.csv") %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log)) %>%
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>% 
  mutate(sheet = NA, index = NA, relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)
# survey tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "choices")

# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))

# get new name and choice pairs to add to the choices sheet
new_vars <- df_cleaning_log %>% 
  filter(type %in% c("change_response", "add_option")) %>% 
  left_join(df_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>%
  rename(choice = value ) %>%
  select(name, choice) %>%
  distinct() %>% # to make sure there are no duplicates
  arrange(name)

# create kobold object ----------------------------------------------------
risks_mentioned = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "risks_mentioned")
harm_mentioned = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "harm_mentioned")
child_age_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "child_age_info")

kbo <- kobold::kobold(survey = df_survey, 
                      choices = df_choices, 
                      data = df_raw_data, 
                      cleaning = df_cleaning_log,
                      risks_mentioned,
                      harm_mentioned,
                      child_age_info
)

# modified choices for the survey tool
df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)

# special treat for variables for select_multiple, we need to add the columns to the data itself
df_survey_sm <- df_survey %>% 
  mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                            str_detect(string = type, pattern = "select_one|select one") ~ "so",
                            TRUE ~ type)) %>% 
  select(name, q_type)

# construct new columns for select multiple
new_vars_sm <- new_vars %>% 
  left_join(df_survey_sm, by = "name") %>% 
  filter(q_type == "sm") %>% 
  mutate(new_cols = paste0(name,"/",choice))

# add new columns to the raw data
df_raw_data_modified <- df_raw_data %>% 
  butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )