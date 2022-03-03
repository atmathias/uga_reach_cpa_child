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