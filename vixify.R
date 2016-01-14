# TTE = Time to Expiration
# F1, F2 = Forward Index Level Desired from index option prices
# K0 = First Strike below forward index level F
# Ki = Strike price of the i'th OTM option, (Ki<0 if put)
# deltaKi = Interval between strike prices minus
  # half the difference between the strike on either side of Ki
    # Note: deltaKi for lowest strike isjust 
    # diff between lowest and next higher, same for highest strike
# R = risk-free intersst rate to expiration
# Q(Ki) = midpoint of bid/ask spread for each option with strike Ki

# C1

returnForwardLevel <- function(call, put, R, TTE){
  call[,Call_Mid := (Bid+Ask)/2]
  put[,Put_Mid := (Bid+Ask)/2]
  
  m <- merge(call,put,by='Strike', all.y=T)
  m <- as.data.table(m[complete.cases(m)])
  
  FStrike <- m[,.(diff=abs(Put_Mid-Call_Mid), 
       Strike, Call_Mid, Put_Mid)][order(diff)][1]
  
  # F = Strike Price + eRT x (Call Price - Put Price)
  
  Forward <- 
    FStrike$Strike + 
    exp(R*TTE)*(FStrike$Call_Mid - FStrike$Put_Mid)
  
  QK0 <- (FStrike$Call_Mid + FStrike$Put_Mid)/2
  
  data.table(Forward,Strike=FStrike$Strike,QK0)
}


returnEligibleOptions <- function(chain,fwd,R,TTE){
  ERT <- exp(R*TTE)
  isCALL <- grepl('C',as.character(chain[,Symbol][1]))
  N <- dim(chain)[1]
  chain[,prevStrike := c(2*Strike[1]-Strike[2],Strike[1:(N-1)])]
  chain[,nextStrike := c(Strike[2:N], 2*Strike[N]-Strike[(N-1)])]
  chain[,deltaK := (nextStrike-prevStrike)/2]
  
  K0 <- fwd$Strike
  
  if(isCALL){
    chain[,eligible := T]
    chain[,nextBid := c(Bid[2:(N)],0)]
    i <- chain[,.(Bid,nextBid,.I)][nextBid == 0 & Bid == 0][1,I]
    chain[i:N, eligible := FALSE]
    chain[Strike <= K0, eligible := FALSE]
  
  } else {
    chain[, eligible := T]
    chain[,prevBid := c(0,Bid[1:(N-1)])]
    i <- chain[,.(Bid,prevBid,.I)][prevBid == 0 & Bid == 0][.N,I]
    chain[1:i, eligible := FALSE]
    chain[Strike >= K0, eligible := FALSE]
  }
  chain[, K2 := (Strike^2)]
  chain[, contribution := (deltaK/K2)*ERT*Mid]
  chain[chain$eligible]
}

variance <- function(calls, puts,R,TTE,fwd) {
  ERT <- exp(R*TTE)
  QK0 <- fwd$QK0
  atm <- data.table(deltaK=5,
                    K2=fwd$Strike^2,
                    Bid=QK0,
                    Mid=QK0,
                    Ask=QK0,
                    contribution=(5/(fwd$Strike^2))*ERT*QK0)
  chain <- rbind(puts[,.(deltaK,K2,Bid,Mid,Ask,contribution)],
                 atm,
                 calls[,.(deltaK,K2,Bid,Mid,Ask,contribution)])
  
  carry <- (1/TTE1)*(fwd$Forward/fwd$Strike -1)^2
  list(var=(2/TTE1)*sum(chain[,contribution]) - carry,
       chain)
}

puts_SPX1[,.(deltaK,Mid,Strike)]
rawvar <- function(chain, R, TTE){
  
}  
  