# ----------- #
## Functions ##
# ----------- #

# ----------------------------------------------------------------
# Desc: Houses all user-created functions.
# Naming convention: User-created functions prefixed with 'func_'.
# Credit: None
# Script Dependencies: None
# Packages Used: tidyverse, rvest, tm, SnowballC
# Notes: None
# ----------------------------------------------------------------

# --------------------
# Webscrape HTML table
#---------------------
# DESC: Pulls data from HTML tables on websites via webscraping.
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
# DESC: Cleans HTML tables into readable format
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