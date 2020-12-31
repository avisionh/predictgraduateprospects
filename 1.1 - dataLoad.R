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
data_list[[1]] <- read_csv(file = 'data/he_admin.csv', col_names = TRUE, skip = 12)
data_list[[2]] <- read_csv(file = 'data/he_qualifiers.csv', col_names = TRUE, skip = 12)
data_list[[3]] <- read_csv(file = 'data/he_subject.csv', col_names = TRUE, skip = 14)
data_list[[4]] <- read_csv(file = 'data/he_urp.csv', col_names = TRUE, skip = 12)

# Download from: 
# https://www.thecompleteuniversityguide.co.uk/league-tables/rankings?tabletype=full-table
data_list[[5]] <- read_csv(file = 'data/archive/graduate_prospects_2018.csv',
                           col_names = TRUE,
                           skip = 4)
data_list[[6]] <- read_csv(file = 'data/archive/graduate_prospects_2017.csv',
                           col_names = TRUE,
                           skip = 4)