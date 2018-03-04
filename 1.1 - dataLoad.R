# ----------- #
## Data Load ##
# ----------- #

# --------------------------------------------------------------------
# Desc: Loads flat .csv data files in.
# Naming convention: Data prefixed with 'data_'.
# Credit: HESA Statistical First Release (SFR247) 2016/17 
# Script Dependencies: None
# Packages Used: tidyverse, rvest(implicit for user-created function)
# Notes: None
# -------------------------------------------------------------------

# Put these dataframes into a list object to work more easily with them
# and delete the dataframes.
# Create length of list beforehand to speed up code running.
data_list <- list()
length(data_list) <- 5
data_list[[1]] <- read_csv(file = 'Data/he_admin.csv', col_names = TRUE, skip = 12)
data_list[[2]] <- read_csv(file = 'Data/he_qualifiers.csv', col_names = TRUE, skip = 12)
data_list[[3]] <- read_csv(file = 'Data/he_subject.csv', col_names = TRUE, skip = 14)
data_list[[4]] <- read_csv(file = 'Data/he_urp.csv', col_names = TRUE, skip = 12)

# Web-scrape from Unviersity League Tables website
url = "https://www.thecompleteuniversityguide.co.uk/league-tables/rankings?v=wide&y=2018"
data_list[[5]] <- func_readHTMLTable(url = url)
url = "https://www.thecompleteuniversityguide.co.uk/league-tables/rankings?v=wide&y=2017"
data_list[[6]] <- func_readHTMLTable(url = url)
