#----------------#
## Data Wrangle ##
#----------------#

# --------------------------------------------------------------------
# Desc: Manipulates dataframes for analysis.
# Naming convention: N/A
# Credit: None
# Script Dependencies: '1.1 - dataLoad.R'
# Packages Used: tidyverse
# Notes: Data is stored in list object. The indexing works as follows:
  # 1. 2017 Admin (First degree full-time, postgrad full-time, sex, region)
  # 2. 2017 Qualifiers
  # 3. 2017 Subject
  # 4. 2017 Under-represented groups
  # 5. 2018 Graduate prospects
  # 6. 2017 University Guide (Student-staff ratio, academic services spend, 
  #         good honours, and facility spend
# --------------------------------------------------------------------

# -----------------------------------------
# University Guide: Graduate Prospects 2018
# -----------------------------------------
# DESC: Clean the parsed HTML table, data_list[[5]], to obtain
#       graduate prospects field.
# Take column of interest
 # Store 'Graduate Prospects...' column name as col_gradPros
 # then refer to it by name when choosing columns
name_gradPros <- paste(grep(pattern = "^Graduate", x = names(data_list[[5]]), value = TRUE))
data_list[[5]] <- data_list[[5]][, c("University Name", name_gradPros)]

# Clean
data_list[[5]] <- func_cleanHTMLTable(x = data_list[[5]], colNames = c("Name", "Graduate Prospects"))

# ---------------------------------
# University Guide: Other Data 2017
# ---------------------------------
# DESC: Clean the parsed HTML table, data_list[[6]], to obtain
#       the following fields:
#         - Rank
#         - Rank Change
#         - Name
#         - Entry Standards
#         - Student Satisfaction
#         - Research Quality
#         - Research Intensity
#         - Graduate Prospects
#         - Student-Staff Ratio
#         - Academic Services Speeds
#         - Facilities Spend
#         - Good Honours
#         - Degree Completion
#         - Overall Score
# Remove unecessary columns
data_list[[6]] <- data_list[[6]][, 2:15]

# Clean
name_cols <- c("Rank", "Rank Change", "Name", "Entry Standards", "Student Satisfaction", "Research Quality",
                  "Research Intensity", "Graduate Prospects", "Stu-Staff Ratio", "Acad Services Speeds",
                  "Facilities Spend", "Good Honours", "Degree Completion", "Overall Score")
data_list[[6]] <- func_cleanHTMLTable(x = data_list[[6]], colNames = name_cols)

# -------------------------------------------------
# Admin Table: First-degree Full-time Student Count
# -------------------------------------------------
# DESC: Cleans the Admin table, data_list[[1]], to obtain
#       First degree (FD) Full-time (FT) student count field.
temp_firstDegree <- func_cleanAdminTable(x = data_list[[1]]) %>% 
                      # Select and rename columns
                      dplyr::select(c(UKPRN, `HE provider`, `Country of HE provider`, 
                                      `Region of HE provider`, `Student count`)) %>% 
                      dplyr::rename(`FD FT student count` = `Student count`)

# -------------------------------------------------
# Admin Table: Postgraduate Full-time Student Count
# -------------------------------------------------
# DESC: Cleans the Admin table, data_list[[1]], to obtain
#       Postgraduate (PG) Full-time (FT) student count field.
temp_postgradDegree <- func_cleanAdminTable(x = data_list[[1]],
                             filter_lvlstudy = c("Postgraduate (research)", "Postgraduate (taught)")) %>% 
                        spread(key = `Level of study`, value = `Student count`) %>% 
                        # Treat universities with `Postgraduate (taught)` or `Postgraduate (research)` as NA like 0s
                        # in sense they don't offer that programme
                        mutate(`PG FT student count` = ifelse(is.na(`Postgraduate (research)`), `Postgraduate (taught)`,
                                                                     ifelse(is.na(`Postgraduate (taught)`), `Postgraduate (research)`,
                                                                            `Postgraduate (research)` + `Postgraduate (taught)`))) %>% 
                        dplyr::select(UKPRN, `HE provider`, `Country of HE provider`,
                                      `Region of HE provider`, `PG FT student count`)

# -----------------------------------------------------
# Admin Table: First-degree Full-time Sex Student Count
# -----------------------------------------------------
# DESC: Cleans the Admin table, data_list[[1]], to obtain
#       First-degree (FD) Full-time (FT) female, male, other count field.
temp_sex <- func_cleanAdminTable(x = data_list[[1]], filter_sexdomicile = "Sex") %>% 
          spread(key = `Sex/Domicile`, value = `Student count`) %>% 
          mutate(`Male Female Ratio` = ifelse(`Female` == 0, NA,`Male`/`Female`)) %>% 
          dplyr::select(c(`UKPRN`, `HE provider`, `Country of HE provider`,
                          `Region of HE provider`, `Male Female Ratio`))

# --------------------------------------------------------
# Admin Table: First-degree Full-time Region Student Count
# --------------------------------------------------------
# DESC: Cleans the Admin table, data_list[[1]], to obtain
#       First-degree (FD) Full-time (FT) female, male, other count field.
temp_region <- func_cleanAdminTable(x = data_list[[1]], filter_sexdomicile = "Domicile") %>% 
          spread(key = `Sex/Domicile`, value = `Student count`) %>% 
          mutate(`EU to non-EU Ratio` = ifelse(`Non-European Union` == 0, NA,
                                               (`UK` + `Other European Union`)/`Non-European Union`)) %>% 
          dplyr::select(c(`UKPRN`, `HE provider`,
                          `Country of HE provider`, `Region of HE provider`, `EU to non-EU Ratio`))

# ----------------
# Qualifiers Table
# ----------------
# DESC: Cleans the Qualifiers table, data_list[[2]], to obtain
#       Pass Fail Ratio field.



rm(name_cols, name_gradPros)
