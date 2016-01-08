
library(quantmod)
library(RCurl)
library(XML)
library(data.table)

# Declare Variables
SPXURL <- "http://finance.yahoo.com/q/op?s=^SPX"
SPX_LAST <- getQuote('^SPX')$Last

# Scrape Yahoo ^SPX Expiries
webpage <- getURL(SPXURL)
htmlpage <- htmlParse(webpage, asText=T)
pageoptions <- xpathSApply(htmlpage, "//option", function(u) xmlAttrs(u)["value"])
expiries_UNIX <- as.numeric(as.vector(pageoptions))

ExpiryInfo <- data.table(expiries_UNIX,
           SecondsTo30Day = abs(expiries_UNIX-as.numeric(Sys.time() + 30*24*60*60)))
  
VIX_Expiries <- ExpiryInfo[order(SecondsTo30Day)][1:2]

# Scrape First Expiry Options
tables <- readHTMLTable(paste0(SPXURL,"&date=", VIX_Expiries$expiries_UNIX[1]))
calls_SPX1 <- clean(as.data.table(tables[[2]]))
puts_SPX1 <- clean(as.data.table(tables[[3]]))

# Scrape Second Expiry Options
tables <- readHTMLTable(paste0(SPXURL,"&date=", VIX_Expiries$expiries_UNIX[2]))
calls_SPX2 <- clean(as.data.table(tables[[2]]))
puts_SPX2 <- clean(as.data.table(tables[[3]]))
rm(tables)

## PRE:
# chain: data.table of option chain
## POST: returns data.table with col names, correct datatypes
clean <- function(chain){
  chain <- sapply(chain, as.character) # factor -> character
  chain <- sub("%", "", chain)
  chain <- as.data.frame(chain)
  chain$V1 <- sub(",", "", chain$V1)
  names(chain) <- c('Strike','Symbol','Last', 'Bid', 'Ask', 'Change',
                'PctChange', 'Volume', 'OI', 'IV')
  for(i in c(1,3:10)) {
    chain[,i] <- as.numeric(as.character(chain[,i]))    
  }
  chain <- as.data.table(chain)
}
