# -------------------- #
## Data Consolidation ##
# -------------------- #

# -------------------------------------------------------------------------------
# Desc: Joins wrangled datasets together.
# Naming convention: N/A
# Credit: None
# Script Dependencies: 
  # 1. '1.1 - dataLoad.R'
  # 2. '1.2 - dataWrangle.R'
# Packages Used: tidyverse, tm, stringdist
# Notes: Uses text analysis techniques like stemming, and fuzzy-matching to join
#         data from HESA to Complete Universities Guide together.
# ------------------------------------------------------------------------------

# Join HESA SFR dataframes together
temp_hesa <- temp_firstDegree %>% 
                left_join(y = temp_postgradDegree[, c("UKPRN", "PG FT student count")], by = "UKPRN") %>% 
                left_join(y = temp_sex[, c("UKPRN", "Male Female ratio")], by = "UKPRN") %>% 
                left_join(y = temp_region[, c("UKPRN", "EU to non-EU ratio")], by = "UKPRN") %>% 
                left_join(y = temp_qualifiers[, c("UKPRN", "Pass Fail ratio")], by = "UKPRN") %>% 
                left_join(y = temp_subjects[, c("UKPRN", "STEM non-STEM ratio")], by = "UKPRN") %>% 
                left_join(y = temp_underRepGroup[, c("UKPRN", "State Private ratio")], by = "UKPRN")
# Join Complete University's Guide dataframes together
temp_CUG <- temp_otherUniTableData %>%
              rename(`Graduate Prospects 2017` = `Graduate Prospects`) %>% 
              left_join(y = temp_gradProspects, by = "Name") %>% 
              rename(`Graduate Prospects 2018` = `Graduate Prospects`)

# Clean text data
temp_uniName <- func_textClean(data_hesa$`HE provider`)
temp_name <- func_textClean(data_CUG$Name)                  

# Column bind text cleaned columns to joined dataframes.
# Column bind okay because order of rows was preserved by func_textClean
data_hesa <- temp_hesa %>% 
              cbind(temp_uniName) %>% 
              dplyr::select(c(`UKPRN`, `HE provider`, `Name`, `Country of HE provider`:`State Private ratio`))
data_CUG <- temp_CUG %>%
              rename(`University Name` = `Name`) %>% 
              cbind(temp_name) %>% 
              dplyr::select(`University Name`, `Name`, `Rank`, `Entry Standards`:`Graduate Prospects 2018`)

# Remove unwanted objects
rm(temp_firstDegree, temp_postgradDegree, temp_sex, temp_region, temp_qualifiers, temp_subjects, temp_underRepGroup,
   temp_otherUniTableData, temp_gradProspects, temp_uniName, temp_name, temp_hesa, temp_CUG)
              