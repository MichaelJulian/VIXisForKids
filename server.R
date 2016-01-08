
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source('scrape_yahoo_options.R')
source('vixify.R')
library(shiny)

shinyServer(function(input, output) {
  
  R <- getRiskFreeRate()
  VIX_Expiries <- getExpiries(SPXURL)
  #   output$expiry1 <- reactive({ format(as.POSIXct(VIX_Expiries$expiries_UNIX[1],origin='1970-01-01'), format="%b %d, %Y" ) })
  #   output$expiry2 <- reactive({ format(as.POSIXct(VIX_Expiries$expiries_UNIX[2],origin='1970-01-01'), format="%b %d, %Y" ) })
  #   
  TTE1 <- (VIX_Expiries$expiries_UNIX[1] + (6+12+3)*60*60 - as.numeric(Sys.time()))/(365*24*60*60)
  TTE2 <- (VIX_Expiries$expiries_UNIX[2] + (6+12+3)*60*60 - as.numeric(Sys.time()))/(365*24*60*60)
  print((VIX_Expiries$expiries_UNIX[1] + (6+12+3)*60*60 - as.numeric(Sys.time()))/(365*24*60*60))
  #format(as.POSIXct(VIX_Expiries$expiries_UNIX[1]+(6+12+3)*60*60,origin="1970-01-01"))
  # time until expiry in N seconds
  NT1 <- (VIX_Expiries$expiries_UNIX[1] - as.numeric(Sys.time()))
  NT2 <- (VIX_Expiries$expiries_UNIX[2] - as.numeric(Sys.time()))
  
  N30 <- 30*24*60*60
  N365 <- 365*24*60*60
  
  # Scrape First Expiry Options
  tables <- readHTMLTable(paste0(SPXURL,"&date=", VIX_Expiries$expiries_UNIX[1]))
  calls_SPX1 <- clean(as.data.table(tables[[2]]))
  puts_SPX1 <- clean(as.data.table(tables[[3]]))
  
  # Scrape Second Expiry Options
  tables <- readHTMLTable(paste0(SPXURL,"&date=", VIX_Expiries$expiries_UNIX[2]))
  calls_SPX2 <- clean(as.data.table(tables[[2]]))
  puts_SPX2 <- clean(as.data.table(tables[[3]]))
  rm(tables)
  
  if(exists('TTE2') & exists('TTE1')){
    
    F1 <- returnForwardLevel(calls_SPX1, puts_SPX1, R, TTE1)
    F2 <- returnForwardLevel(calls_SPX2, puts_SPX2, R, TTE2)
    
    w1 <- (NT2-N30)/(NT2-NT1)
    w2 <- (N30-NT1)/(NT2-NT1)
    
    c1 <- returnEligibleOptions(calls_SPX1,F1,R,TTE1)
    p1 <- returnEligibleOptions(puts_SPX1,F1,R,TTE1)
    
    
    c2 <- returnEligibleOptions(calls_SPX2,F2,R,TTE2)
    p2 <- returnEligibleOptions(puts_SPX2,F2,R,TTE2)
    
    output$final1 <- variance(c1,p1,R,TTE1,F1) 
    var1 <- final1$var
    chain1 <- final1[[2]]
    chain1
    
    output$final2 <- variance(c2,p2,R,TTE2,F2) 
    var2 <- final2$var
    chain2 <- final2[[2]]
    chain2
    output$VIX <-  100 * sqrt((TTE1*var1*w1 + TTE2*var2*w2)*(N365/N30))
    
    
    output$contributionPlot1 <- renderPlot({ 
      plot(chain1$contribution,x=sqrt(chain1$K2))
    })
    
    output$contributionPlot2 <- renderPlot({ 
      plot(chain2$contribution,x=sqrt(chain2$K2))
    })
    
    output$chain1 <- renderDataTable({
      chain1[,.(Strike=sqrt(K2),Bid,Mid,Ask,contributionx1000=round(contribution*1000,digits=4))]
    })
    
    output$chain2 <- renderDataTable({
      chain2[,.(Strike=sqrt(K2),Bid,Mid,Ask,contributionx1000=round(contribution*1000,digits=4))]
    })
    
    
    
  }
  
  
  
  
})
