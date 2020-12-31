# ------------- #
## Exploration ##
# ------------- #

# ----------------------------------------------------------------
# Desc: Descriptive statistics of our variables.
# Naming convention: N/A
# Credit: None
# Script Dependencies:
  # 1. '0.1 - functions.R'
  # 2. '1.1 - dataLoad.R'
  # 3. '1.2 - dataWrangle.R'
  # 4. '1.3 - dataConsolidation.R'
# Packages Used: tidyverse, fitdistrplus, plotly
# Notes: None
# ----------------------------------------------------------------

# Count total number of NAs in dataframe
temp_countNA <- apply(X = data_master, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# Get proportion of NAs in dataframe
temp_propNA <- sum(temp_countNA) / (nrow(data_master) * ncol(data_master))
# Text for report
# Note ifelse() is a vectorised function, so cannot return a vector here. 
if (temp_propNA < 0.05) { 
  txt_missingData <- c("very little", "do not")
} else {
  if (temp_propNA < 0.4) {
    txt_missingData <- c("some", "do")
  } else 
      txt_missingData <- c("a lot of", "do")
}

# CORRELATIONS
 # vector of correlations
name_indVar <- c("PG FT student count", "Male Female ratio", "EU to non-EU ratio", "Pass Fail ratio", 
                 "STEM non-STEM ratio", "State Private ratio", "Research Quality","Student-Staff Ratio", 
                 "Graduate Prospects 2018")
 # DV to IV correlation
mat_corrDV <- data_master[, name_indVar] %>% 
              cor(use = "pairwise.complete.obs") %>% 
              round(digits = 2)
mat_corrDV <- mat_corrDV[1:nrow(mat_corrDV) - 1,"Graduate Prospects 2018"]
 # IV correlation table
name_indVar <- name_indVar[!name_indVar %in% c("Graduate Prospects 2018")]
mat_corrIV <- data_master[, name_indVar] %>% 
  cor(use = "pairwise.complete.obs") %>%
  round(digits = 2) %>% 
  # Remove upper-left
  func_upperNA() %>% 
  reshape2::melt(na.rm = TRUE)

 # drop highly correlated variables
name_indVar <- name_indVar[!name_indVar %in% c("PG FT student count", "Male Female ratio", "State Private ratio")]
name_indVar <- append(x = name_indVar, values = "Graduate Prospects 2018")

# TRANSFORM FOR REGRESSION
mat_reg <- data_master[, name_indVar] %>% 
            # normalise values
            sapply(FUN = function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) %>% 
            # replace NA values with their median - bad practice!
            apply(MARGIN = 2, FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) 


