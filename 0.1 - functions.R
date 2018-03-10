# ----------- #
## Functions ##
# ----------- #

# ----------------------------------------------------------------
# Desc: Houses all user-created functions.
# Naming convention: User-created functions prefixed with 'func_'.
# Credit: None
# Script Dependencies: None
# Packages Used: tidyverse, rvest, tm, SnowballC, caret, runjags
# Notes: None
# ----------------------------------------------------------------

# --------------------
# Webscrape HTML table
#---------------------
# DESC: Pulls data from HTML tables on websites via webscraping.
# CREDIT: None
# ARGUMENTS:
  # 1. 'url' | the web-address you're pulling HTML table from.
func_readHTMLTable <- function(url) {
  html <- read_html(x = url)
  x <- html %>% 
        html_nodes(css = "table") %>% 
        html_table(header = TRUE)
  x <- x[[1]]
  return(x)
}

# ----------------
# Clean HTML table
#-----------------
# DESC: Cleans HTML tables into readable format.
# CREDIT: None
# ARGUMENTS:
  # 1. 'x' (dataframe) | HTML table to clean.
  # 2. 'colNames' (vector) | Names to replace existing columns.
func_cleanHTMLTable <- function(x, colNames) {
  # Rename columns
  colnames(x) <- colNames
  # Remove rows with entries starting with 'googletag' in Rank column
  # This is predicated on renaming a column, 'Name'
  x <- x[!grepl(pattern = "^googletag", x = x$Name), ]
  # Take every odd row to remove nonsense entries
  # More sustainable solution is to use x[!grepl(pattern = "^League Table Ranking", x = x$Name)]
  # But retained this for learning purposes
  x <- x[c(TRUE, FALSE), ]
}

# -----------------
# Clean Admin table
# -----------------
# DESC: Re-formats and applies various filters on Admin table
#       to obtain right information in right format.
#       For data_list[[1]], enrolment, only interested in the following:
#         - Level of study == 'First degree'
#         - Mode of study == 'Full time'
#       Table is in very long format, meaning we also need to filter on the following fields:
#         - Country of HE provider
#         - Region of HE provider
#         - Sex/Domicile marker == 'Totals'
#       Where for universities not in England, their region is only 'All',
#       but for univeristies in England, their regions is 'All' and somewhere else,
#       meaning need to separate univeristies in England from those not in England,
#       and append.
# CREDIT: None
# ARGUMENTS:
  # 1. 'x' (dataframe) | Dataframe to clean.
  # 2. 'filter_lvlstudy (vector) | Filter to apply to `Level of study` column.
  # 3. 'filter_sexdomicile (vector) | Filter to apply to `Sex/Domicile marker` column.
func_cleanAdminTable <- function(x, 
                                 filter_lvlstudy = c("First degree", "First degree"), 
                                 filter_sexdomicile = "Totals") {
  # Non-English universities
  x1 <- x %>% 
          filter(`HE provider` != "Total" & `Mode of study` == "Full-time" & 
                  `Country of HE provider` != "All" & `Country of HE provider` != "England" &
                  `First year marker` == "All" & `Sex/Domicile marker` == filter_sexdomicile &
                  (`Level of study` == filter_lvlstudy[1] | `Level of study` == filter_lvlstudy[2]))
  # English universties
  x2 <- x %>% 
          filter(`HE provider` != "Total" & `Mode of study` == "Full-time" &
                  `Country of HE provider` == "England" & `Region of HE provider` != "All" &
                  `First year marker` == "All" & `Sex/Domicile marker` == filter_sexdomicile &
                  (`Level of study` == filter_lvlstudy[1] | `Level of study` == filter_lvlstudy[2]))
  # Union two dataframe together
  x <- x1 %>% 
        rbind(x = x2)
  return(x)
}

# ----------
# Text Clean
# ----------
# DESC: Cleans text data by following the steps:
#         1. Convert to lower-case
#         2. Remove numbers
#         3. Remove English common stopwords
#         4. Remove punctuation
#         5. Eliminate extra white spaces
#         6. Stems words
# CREDIT: None
# ARGUMENTS:
  # 1. 'col' (vector) | Dataframe column of text data type to clean
func_textClean <- function(col){
  # Convert column into a Corpus object to apply tm functions on there
  x <- Corpus(VectorSource(x = col)) %>% 
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removeNumbers) %>% 
        tm_map(removeWords, stopwords("english")) %>% 
        tm_map(removePunctuation) %>% 
        tm_map(stripWhitespace) %>% 
        tm_map(stemDocument) %>% 
        # Remove additional stopwords of "The University of" to get 
        # HESA SFR uni names in line with Complete University's Guide
        tm_map(removeWords, c("The University of"))
  # Convert Corpus to dataframe
  # Note: https://stackoverflow.com/questions/33193152/unable-to-convert-a-corpus-to-data-frame-in-r/33193705
  x <- data.frame(Name = sapply(X = x, FUN = as.character), stringsAsFactors = FALSE)
  return(x)  
}

# -------
# NA Plot
# -------
# DESC: Visualises missing data within a dataframe.
# CREDIT: https://www.r-bloggers.com/ggplot-your-missing-data-2/
# ARGUMENTS:
  # 1. 'x' (dataframe) | Dataframe to visualise missing values from.
func_plotNAs <- function(x) {
  x %>% 
    is.na() %>% 
    reshape2::melt() %>% 
    ggplot(data = ., mapping = aes(x = `Var2`, y = `Var1`)) +
      geom_raster(mapping = aes(fill = value)) +
      scale_fill_grey(name = "", labels = c("Present", "Missing")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5), plot.title = element_text(hjust = 0.5)) +
      labs(title = "Missing Values in Dataset", x = "Variables in dataset", y = "Observation/row index")
}

# ------------------
# Correlation Matrix
# ------------------
# DESC: Helper functions to remove repeated, redundant information in lower-left
#       or upper-right part of correlation matrix.
# CREDIT: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# ARGUMENTS:
  # 1. 'cormat' (matrix) | Correlation matrix to visualise.
# Get lower triangle of the correlation matrix
func_lowerNA<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
func_upperNA <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# --------------------
# Goodness-of-fit Plot
# --------------------
# DESC: Plots the following so we can test which distibution best fits DV.
#       Bundles together the densplotcomp, qqcomp, cdfcomp, and ppcomp functions
#       from 'fitdistrplus' package.
# CREDIT: None.
# ARGUMENTS:
  # 1. 'dist' (list) | Distribution estimates to plot with.
  # 2. 'titles' (vector) | Legend label to describe distrubition being plotted.
func_plotGoF <- function(dist, titles) {
  denscomp(ft = dist, legendtext = titles)
  qqcomp(ft = dist, legendtext = titles)
  cdfcomp(ft = dist, legendtext = titles)
  ppcomp(ft = dist, legendtext = titles)
}

# -----------------------
# Positive Transformation
# -----------------------
# DESC: Turns all values in column positive for Bayesian regression.
# CREDIT: None.
# ARGUMENTS:
# 1. 'x' (vector) | Dataframe column to convert values to positive.
func_transPos <- function(x) {
  if (min(x) < 0) {
    x <- x - min(x) + 0.00000001
  } else {
    x <- x + 0.00000001
  }
  return(x)
}

# --------------------
# Bayes Predict Output
# --------------------
# DESC: Performs k-fold cross-validation Bayesian 
# CREDIT: None.
# NOTE: Function used within 'func_kFoldCV'.
# ARGUMENTS:
# 1. 'x' (mcmc) | Matrix of independent variables.
# 2. 'y' (vector) | Column of dependent variable.
func_predictBayes <- function(mcmcobject, x) {
  # Get coefficient outputs and store in a dataframe
  #coeff <- summary(mcmcobject)$statistics[,1] doesn't work because this is not a recursive(list) object
  #so can't use $ operator
  coeff <- summary(mcmcobject)[[1]][,1]
  coeff <- as.data.frame(coeff)
  # Do matrix multiplication to get outputs
  y_pred <- coeff[1,] + x %*% coeff[2:nrow(coeff), ]
  return(y_pred)
}

# -----------------------------
# Calculate RMSE
# -----------------------------
# DESC: Calculates the root-mean-squared-error
# CREDIT: None.
# NOTE: Function used within 'func_kFoldCV'.
# ARGUMENTS:
# 1. 'y' (vector) | Column of DV values.
# 2. 'y_pred' (vector) | Column of predicted DV values.
func_calcRMSE <- function(y, y_pred) {
  sqrt(mean( (y - y_pred)^2 ))
}

# -----------------------------
# Bayes k-Fold Cross-Validation
# -----------------------------
# DESC: Performs k-fold cross-validation Bayesian 
# CREDIT: None.
# NOTE: When doing matrix multiplication, are arbitrarily taking
#       first element of the mcmc list that will be passed into 'func_predictBayes'
# ARGUMENTS:
  # 1. 'x' (matrix) | Matrix of independent variables.
  # 2. 'y' (vector) | Column of dependent variable.
  # 3. 'partition' (value) | Proportion of data to split for training set.
  # 4. 'folds' (value) | Number of folds to perform.
  # 5. 'parameters' (vector) | Regression parameters for regression.

func_kFoldCV <- function(x, y, partition, folds, parameters, burnin, nChains) {
  # Create list to store MCMC outputs in.
  list_mcmc <- list()
  length(list_mcmc) <- folds
  # Create list for predicted values. 
  list_predict <- list()
  length(list_predict) <- folds
  # Create list test sets.
  test_sets <- list()
  length(test_sets) <- folds
  test_predictions <- list()
  length(test_predictions) <- folds
  # Create vector for RMSEs.
  vec_RMSE <- c()
  length(vec_RMSE) <- folds
  
  # Workhorse of the function
  for (i in 1:folds) {
    # Partiton data into train and test sets.
    train_set_indices <- createDataPartition(y, p = partition, list = FALSE)
    train_predictors <- y[train_set_indices]
    train_set <- x[train_set_indices, ]
    test_predictions[[i]] <- y[-train_set_indices]
    test_sets[[i]] <- x[-train_set_indices, ]
    
    # Set-up.
    noRowsIV <- nrow(train_set)
    noRowsDV <- length(train_predictors)
    data_reg <- list(x = train_set[1:noRowsIV, ], y = train_predictors[1:noRowsDV])
    
    # Run MCMC simulations using parallel computing methods.
    temp_reg <- run.jags(method = "parallel", model = "regModel.txt", monitor = parameters,
                         data = data_reg, n.chains = nChains, burnin = burnin, 
                         summarise = FALSE, plots = FALSE)
    
    # Store the MCMC objects.
    list_mcmc[[i]] <- as.mcmc.list(temp_reg)
    # Store predicted values from Bayes output.
    list_predict[[i]] <- func_predictBayes(list_mcmc[[i]][[1]], test_sets[[i]])
    # Store RMSEs
    vec_RMSE[i] <- func_calcRMSE(test_predictions[[i]], list_predict[[i]])
  }
  
  # Store mcmc objects, regression prediction, and RMSE values in a list to return.
  output <- list(list_mcmc, list_predict, vec_RMSE)
  return(output)
}