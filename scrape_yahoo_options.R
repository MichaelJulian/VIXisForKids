
library(quantmod)
library(RCurl)
library(XML)
library(data.table)

# Declare variables
SPXURL <- "http://finance.yahoo.com/q/op?s=^SPX"

# Scrape Risk Free Rate
getRiskFreeRate <- function(){  
  treasury <- xmlParse('http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=month(NEW_DATE)%20eq%201%20and%20year(NEW_DATE)%20eq%202016')
  treasury_list <- xmlToList(treasury)
  treasury_n <- length(treas_list)
  R <- as.numeric(treasury_list[treasury_n-1][[1]]$content$properties$BC_1MONTH$text)
  R <- R/100
  R
}

# Scrape Yahoo ^SPX Expiries
getExpiries <- function(SPXURL){
  webpage <- getURL(SPXURL)
  htmlpage <- htmlParse(webpage, asText=T)
  pageoptions <- xpathSApply(htmlpage, "//option", function(u) xmlAttrs(u)["value"])
  expiries_UNIX <- as.numeric(as.vector(pageoptions))
  ExpiryInfo <- data.table(expiries_UNIX,
                           SecondsTo30Day = abs(expiries_UNIX-as.numeric(Sys.time() + 30*24*60*60)))
  TrueExpiries <- ExpiryInfo[order(SecondsTo30Day)][1:2]
  TrueExpiries
}

## PRE:
# chain: data.table of option chain
## POST: returns data.table with col names, correct datatypes
clean <- function(chain){
  chain <- sapply(chain, as.character) # factor -> character
  chain <- sub("%", "", chain)
  chain <- sub("SPXW", "", chain)
  chain <- as.data.frame(chain)
  chain$V1 <- sub(",", "", chain$V1)
  names(chain) <- c('Strike','Symbol','Last', 'Bid', 'Ask', 'Change',
                'PctChange', 'Volume', 'OI', 'IV')
  for(i in c(1,3:10)) {
    chain[,i] <- as.numeric(as.character(chain[,i]))    
  }
  chain <- as.data.table(chain)

}
