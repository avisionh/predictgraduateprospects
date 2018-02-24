#-------------#
## Functions ##
#-------------#

# ----------------------------------------
# Desc: Houses all user-created functions.
# Naming convention: Prefixed with 'func_'.
# Credit: None
# Script Dependencies: None
# Packages Used: rvest
# Notes: None
# ----------------------------------------

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