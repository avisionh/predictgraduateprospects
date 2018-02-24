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
  # 1. 2017 First degree full-time, postgrad full-time, sex, region
  # 2. 2017 Qualifiers
  # 3. 2017 Subject
  # 4. 2017 Under-represented groups
  # 5. 2018 Graduate prospects value
  # 6. 2017 Student-staff ratio, academic services spend, good honours, and facility spend
# --------------------------------------------------------------------

# Clean the parsed HTML table, data_list[[5]]
# Functionalise below
# Rename columns
names_colNew <- c("Rank", "Rank Change","Name", "Entry Standards", "Student Satisfaction", "Research Quality",
                  "Research Intensity", "Graduate Prospects", "Stu-Staff Ratio", "Acad Services Speeds",
                  "Facilities Spend", "Good Honours", "Degree Completion", "Overall Score")
colnames(data_list[[5]]) <- names_colNew

# Take column of interest
data_list[[5]] <- data_list[[5]][, c("Name", "Graduate Prospects")]

# Remove rows with entries starting with 'googletag' in Rank column
data_list[[5]] <- data_list[[5]][!grepl(pattern = "^googletag", x = data_list[[5]]$Rank), ]
# Take every odd row
data_list[[5]] <- data_list[[5]][c(TRUE, FALSE), ]

# data_list[[6]]
# Remove unecessary columns
data_list[[5]] <- data_list[[5]][, ]

# Remove rows with entries starting with 'googletag' in Rank column
data_list[[5]] <- data_list[[5]][!grepl(pattern = "^googletag", x = data_list[[5]]$Rank), ]
# Take every odd row
data_list[[5]] <- data_list[[5]][c(TRUE, FALSE),]

# Rename columns

colnames(data_list[[6]]) <- names_colNew

# Remove rows with entries starting with 'googletag' in Rank column
data_list[[5]] <- data_list[[5]][!grepl(pattern = "^googletag", x = data_list[[5]]$Rank), ]

# Take every odd row
data_list[[5]] <- data_list[[5]][c(TRUE, FALSE),]

# COLUMN: First degree FTE student count
# For data_list[[1]], enrolment, only interested in the following:
  # Level of study == 'First degree'
  # Mode of study == 'Full time'
# Table is in very long format, meaning we also need to filter on the following fields:
  # Country of HE provider
  # Region
  # Sex/Domicile marker == 'Totals'
# Where for universities not in England, their region is only 'All',
# but for unveristies in England, their regions is 'All' and somewhere else,
# meaning need to separate univeristies in England from those not in England,
# and append.

# Universities not in England
temp1 <- data_list[[1]] %>% 
          filter(`HE provider` != 'Total' & 
                   `Country of HE provider` != 'All' & `Country of HE provider` != 'England' & 
                   `First year marker` == 'All' & `Level of study` == 'First degree' & 
                   `Mode of study` == 'Full-time' & `Sex/Domicile marker` == 'Totals')
# Universities in England
temp2 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' &
           `Country of HE provider` == 'England' & `Region of HE provider` != 'All' & 
           `First year marker` == 'All' & `Level of study` == 'First degree' & 
           `Mode of study` == 'Full-time' & `Sex/Domicile marker` == 'Totals')
# Union these dataframes together
temp_firstDegree <- temp1 %>% 
                rbind(x = temp2) %>% 
                dplyr::select(c(UKPRN, `HE provider`, `Country of HE provider`, 
                                `Region of HE provider`, `Student count`)) %>% 
                dplyr::rename(`First degree FTE student count` = `Student count`)

# COLUMN: Postgrad FTE student count
# Universities not in England
temp1 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' & `First year marker` == 'All' &
           `Country of HE provider` != 'All' & `Country of HE provider` != 'England' & 
           (`Level of study` == 'Postgraduate (research)' | `Level of study` == 'Postgraduate (taught)') &
           `Mode of study` == 'Full-time' & `Sex/Domicile marker` == 'Totals') 
# Universities in England
temp2 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' & `First year marker` == 'All' &
          `Country of HE provider` == 'England' & `Region of HE provider` != 'All' & 
           (`Level of study` == 'Postgraduate (research)' | `Level of study` == 'Postgraduate (taught)') & 
          `Mode of study` == 'Full-time' & `Sex/Domicile marker` == 'Totals')
# Union these dataframes together and compute postgraduate totals
temp_postgrad <- temp1 %>% 
                  rbind(x = temp2) %>%  
                  spread(key = `Level of study`, value = `Student count`) %>% 
                  # Treat universities with `Postgraduate (taught)` or `Postgraduate (research)` as NA like 0s
                  # in sense they don't offer that programme
                  mutate(`Postgrad FTE student count` = ifelse(is.na(`Postgraduate (research)`), `Postgraduate (taught)`,
                                                               ifelse(is.na(`Postgraduate (taught)`), `Postgraduate (research)`,
                                                                      `Postgraduate (research)` + `Postgraduate (taught)`))) %>% 
                  dplyr::select(UKPRN, `HE provider`, `Country of HE provider`,
                                `Region of HE provider`, `Postgrad FTE student count`)

# COLUMN: First degree FTE sex count
# Universities not in England
temp1 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' & `First year marker` == 'All' &
           `Country of HE provider` != 'All' & `Country of HE provider` != 'England' & 
           `Mode of study` == 'Full-time' & `Level of study` == 'First degree' &
           `Sex/Domicile marker` == 'Sex') 
# Universities in England
temp2 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' & `First year marker` == 'All' &
           `Country of HE provider` == 'England' & `Region of HE provider` != 'All' & 
           `Mode of study` == 'Full-time' & `Level of study` == 'First degree' &
           `Sex/Domicile marker` == 'Sex') 
# Union these dataframes together and compute postgraduate totals
temp_sex <- temp1 %>% 
              rbind(x = temp2) %>% 
              spread(key = `Sex/Domicile`, value = `Student count`) %>% 
              dplyr::select(c(UKPRN, `HE provider`, `Country of HE provider`,
                              `Region of HE provider`, `Female`, `Male`, `Other`)) %>% 
              dplyr::rename(`Female count` = `Female`, 
                            `Male count` = `Male`, 
                            `Other count` = `Other`)

# COLUMN: First degree FTE region count
# Universities not in England
temp1 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' & `First year marker` == 'All' &
           `Country of HE provider` != 'All' & `Country of HE provider` != 'England' & 
           `Mode of study` == 'Full-time' & `Level of study` == 'First degree' &
           `Sex/Domicile marker` == 'Domicile') 
# Universities in England
temp2 <- data_list[[1]] %>% 
  filter(`HE provider` != 'Total' & `First year marker` == 'All' &
           `Country of HE provider` == 'England' & `Region of HE provider` != 'All' & 
           `Mode of study` == 'Full-time' & `Level of study` == 'First degree' &
           `Sex/Domicile marker` == 'Domicile') 
# Union these dataframes together and compute postgraduate totals
temp_region <- temp1 %>% 
  rbind(x = temp2) %>% 
  spread(key = `Sex/Domicile`, value = `Student count`) %>% 
  dplyr::select(c(UKPRN, `HE provider`, `Country of HE provider`, `Region of HE provider`, 
                  `Non-European Union`, `Not known`, `Other European Union`, UK)) %>% 
  dplyr::rename(`Non-EU count` = `Non-European Union`,
                `Unknown count` = `Not known`,
                `Other EU count` = `Other European Union`, 
                `UK count` = `UK`)

# COLUMN: First degree degree classification count
temp1 <- data_list[[2]] %>% 
          filter()