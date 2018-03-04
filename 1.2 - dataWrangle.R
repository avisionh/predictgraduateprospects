# -------------- #
## Data Wrangle ##
# -------------- #

# --------------------------------------------------------------------
# Desc: Manipulates dataframes for analysis.
# Naming convention: Temporary objects prefixed with 'temp_'
# Credit: None
# Script Dependencies: '1.1 - dataLoad.R'
# Packages Used: tidyverse
# Notes: Data is stored in list object. The indexing works as follows:
  # 1. 2017 Admin (First degree full-time, postgrad full-time, sex, region)
  # 2. 2017 Qualifiers
  # 3. 2017 Subject
  # 4. 2017 Under-represented groups (State and Private Schools)
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
temp_gradProspects <- func_cleanHTMLTable(x = data_list[[5]], colNames = c("Name", "Graduate Prospects"))

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
temp_otherUniTableData <- func_cleanHTMLTable(x = data_list[[6]], colNames = name_cols)

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
          mutate(`Male Female ratio` = ifelse(`Female` == 0, NA,`Male`/`Female`)) %>% 
          dplyr::select(c(`UKPRN`, `HE provider`, `Country of HE provider`,
                          `Region of HE provider`, `Male Female ratio`))

# --------------------------------------------------------
# Admin Table: First-degree Full-time Region Student Count
# --------------------------------------------------------
# DESC: Cleans the Admin table, data_list[[1]], to obtain
#       First-degree (FD) Full-time (FT) female, male, other count field.
temp_region <- func_cleanAdminTable(x = data_list[[1]], filter_sexdomicile = "Domicile") %>% 
          spread(key = `Sex/Domicile`, value = `Student count`) %>% 
          mutate(`EU to non-EU ratio` = ifelse(`Non-European Union` == 0, NA,
                                               (`UK` + `Other European Union`)/`Non-European Union`)) %>% 
          dplyr::select(c(`UKPRN`, `HE provider`,
                          `Country of HE provider`, `Region of HE provider`, `EU to non-EU ratio`))

# ----------------
# Qualifiers Table
# ----------------
# DESC: Cleans the Qualifiers table, data_list[[2]], to obtain
#       Pass Fail Ratio field for first-degree students.
#       May not be for first-degree full-time students.
# Non-English universities
temp_1 <- data_list[[2]] %>% 
          filter(`HE provider` != "Total" &
                   `Country of HE provider` != "All" & `Country of HE provider` != "England" &
                   `Qualification/Classification marker` == "First degree classification")
# English universities
temp_2 <- data_list[[2]] %>% 
          filter(`HE provider` != "Total" &
                   `Country of HE provider` == "England" & `Region of HE provider` != "All" &
                   `Qualification/Classification marker` == "First degree classification")
# Union temp dataframes together
temp_qualifiers <- temp_1 %>% 
                    rbind(x = temp_2) %>% 
                    spread(key = `Level of qualification/Degree classification`,
                           value = `Student count`) %>% 
                    mutate(`Pass Fail ratio` = 
                             ifelse((`Lower second class honours` + `Third class honours/Pass` + `Unclassified`) == 0,
                                    NA, (`First class honours` + `Upper second class honours`) /
                                      (`Lower second class honours` + `Third class honours/Pass` + `Unclassified`))) %>% 
                    dplyr::select(c(`UKPRN`, `HE provider`,
                                    `Country of HE provider`, `Region of HE provider`, `Pass Fail ratio`))

# --------------
# Subjects Table
# --------------
# DESC: Cleans the Subjects table, data_list[[3]], to obtain
#       STEM Non-STEM ratio field.
#       May not be for first-degree full-time students.
# Vector with STEM titles
name_science <- grep(pattern = "medicine|science|tech", x = names(data_list[[3]]),
                            ignore.case = TRUE, value  = TRUE)
# Compute STEM and Non-STEM ratio
temp_subjects <- data_list[[3]] %>%
                  mutate(`STEM Total` = rowSums(data_list[[3]][, name_science], na.rm = TRUE),
                         `Non-STEM Total` = `Subject area total` - `STEM Total`,
                         `STEM non-STEM ratio` = ifelse(`Non-STEM Total` == 0, NA, 
                                                        `STEM Total` / `Non-STEM Total`)) %>% 
                  filter(`HE provider` != "Total") %>% 
                  dplyr::select(c(`UKPRN`, `HE provider`, `STEM non-STEM ratio`))


# ------------------------------
# State vs. Private School Table
# ------------------------------
# DESC: Cleans the Subjects table, data_list[[4]], to obtain
#       State Private ratio field.
#       May not be for first-degree full-time students.
name_total <- grep(pattern = "total", x = data_list[[4]]$`HE provider`, ignore.case = TRUE, value = TRUE)
temp_underRepGroup <- data_list[[4]] %>% 
                        dplyr::select(c(`UKPRN`, `HE provider`, `Percentage from state schools or colleges`)) %>% 
                        filter(!(`HE provider` %in% name_total)) %>% 
                        rename(`State Private ratio` = `Percentage from state schools or colleges`)

# Remove unwanted objects
rm(url, name_cols, name_gradPros, name_science, name_total, temp_1, temp_2)
