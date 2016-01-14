source('scrape_yahoo_options.R')
source('vixify.R')

R <- getRiskFreeRate()

VIX_Expiries <- getExpiries(SPXURL)

TTE1 <- (VIX_Expiries$expiries_UNIX[1] - as.numeric(Sys.time()))/(365*24*60*60)
TTE2 <- (VIX_Expiries$expiries_UNIX[2] - as.numeric(Sys.time()))/(365*24*60*60)

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

F1 <- returnForwardLevel(calls_SPX1, puts_SPX1, R, TTE1)
F2 <- returnForwardLevel(calls_SPX2, puts_SPX2, R, TTE2)

# puts_SPX1[1:4,Mid := Mid + .025]
# puts_SPX1[1:4,eligible := T]

c1 <- returnEligibleOptions(calls_SPX1,F1,R,TTE1)
p1 <- returnEligibleOptions(puts_SPX1,F1,R,TTE1)

c2 <- returnEligibleOptions(calls_SPX2,F2,R,TTE2)
p2 <- returnEligibleOptions(puts_SPX2,F2,R,TTE2)

final1 <- variance(c1,p1,R,TTE1,F1)
final2 <- variance(c2,p2,R,TTE2,F2)
final1
var1 <- final1$var# + puts_SPX1[1:4,sum(contribution)]*(2/TTE1)
var2 <- final2$var 


chain1 <- final1[[2]]
chain2 <- final2[[2]]

w1 <- (NT2-N30)/(NT2-NT1)
w2 <- (N30-NT1)/(NT2-NT1)

#VIX <- 100 * sqrt((TTE1*var1*w1 + TTE2*var2*w2)*(N365/N30))
VIX2 <- 100 * sqrt((TTE1*var1*w1 + TTE2*var2*w2)*(N365/N30))
VIX2
chain1
chain2
plot(y=chain1$contribution,x=sqrt(chain1$K2),xlab="Strike",ylab='VIX Calculation Contribution',main="Strike Contributions to VIX Calculation")
points(y=chain1$contribution[1:3],x=sqrt(chain1$K2)[1:3], col='red')

plot(y=puts_SPX1$contribution,x=puts_SPX1$Strike,xlab="Strike",ylab='VIX Calculation Contribution',main="Strike Contributions to VIX Calculation")
VIX2

plot(y=cumsum(chain2$contribution),x=sqrt(chain2$K2))
puts_SPX1[,.(Strike,Symbol,Bid,Ask,contribution,eligible)]
