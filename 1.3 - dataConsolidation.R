# -------------------- #
## Data Consolidation ##
# -------------------- #

# -------------------------------------------------------------------------------
# Desc: Joins wrangled datasets together.
# Naming convention: N/A
# Credit: None
# Script Dependencies: 
  # 1. '0.1 - functions.R'
  # 2. '1.1 - dataLoad.R'
  # 3. '1.2 - dataWrangle.R'
# Packages Used: 
  # 1. tidyverse
  # 2. tm (implicit for user-created function)
  # 3. SnowballC (implicit for user-created function)
  # 4. stringdist
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
              left_join(y = temp_gradProspects, by = "Institution") %>% 
              rename(`Graduate Prospects 2018` = `Graduate Prospects`)

# Clean text data
temp_uniName <- func_textClean(temp_hesa$`HE provider`)
temp_name <- func_textClean(temp_CUG$Institution)                  

# Column bind text cleaned columns to joined dataframes.
# Column bind okay because order of rows was preserved by func_textClean
data_hesa <- temp_hesa %>% 
              cbind(temp_uniName) %>% 
              dplyr::select(c(`UKPRN`, `HE provider`, `Name`, `Country of HE provider`:`State Private ratio`))
data_CUG <- temp_CUG %>%
              rename(`University Name` = `Institution`) %>% 
              cbind(temp_name) %>% 
              dplyr::select(`University Name`, `Name`, `Rank`, `Entry Standards`:`Graduate Prospects 2018`)

# Fuzzy-matching
# Create grid of names to match - get all permutations
temp <- expand.grid(data_hesa$Name, data_CUG$Name) %>% 
          rename(`hesa_name` = `Var1`, `CUG_name` = `Var2`) %>% 
          arrange(`hesa_name`)
# Use Longest Common Subsequence (LCS) algorithm
temp$dist_lcs <- stringdist(temp$hesa_name, temp$CUG_name, method = "lcs")
# Rank the LCS distances  smaller the better
temp <- temp %>%
          transform(rank_dist_lcs = ave(`dist_lcs`, `hesa_name`,
                                        FUN = function(x) rank(x, ties.method = "first"))) %>% 
          # Take only the best rank
          filter(rank_dist_lcs == 1) %>% 
          # From visual inspection of matches, any fields with dist_lcs > 8 seem to be wrong,
          # with exception of LSE
          filter(dist_lcs <= 8 | `CUG_name` == "london school econom")

# temp dataframe thus acts as lookup table joining
# data_hesa to data_CUG
data_master <- temp[, c("hesa_name", "CUG_name")] %>% 
                left_join(y = data_hesa, by = c("hesa_name" = "Name")) %>% 
                left_join(y = data_CUG, by = c("CUG_name" = "Name")) %>% 
                # Choose relevant columns
                dplyr::select(c(`UKPRN`:`State Private ratio`, `Rank`:`Graduate Prospects 2018`)) %>% 
                # Reorder columns
                dplyr::select(c(`UKPRN`:`Research Intensity`, `Student-Staff Ratio`:`Overall Score`,
                                `Graduate Prospects 2017`, `Graduate Prospects 2018`))

# Remove unwanted objects
rm(temp_firstDegree, temp_postgradDegree, temp_sex, temp_region, temp_qualifiers, temp_subjects, temp_underRepGroup,
   temp_otherUniTableData, temp_gradProspects, temp_uniName, temp_name, temp_hesa, temp_CUG, temp,
   data_CUG, data_hesa)
              