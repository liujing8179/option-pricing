priceOption <- function(expT=6,s0=30.00,strike=30,rfr=0.1,vol=0.2,type='eu',method='crr',steps=8,DY=0.0263)
{
  #declare variables
  stockTree <- matrix(0,steps+1,steps+1)
  callTree <- matrix(0,steps+1,steps+1)
  putTree <- matrix(0,steps+1,steps+1)
  
  # define size of movements and probabilities
  stepSize <- expT/(12*steps)
  if(method=='crr')
  {
    sizeUp <- exp(vol*sqrt(stepSize))
    sizeDown <- 1/sizeUp
    pU <- (exp((rfr-DY)*stepSize)-sizeDown)/(sizeUp-sizeDown)
    pD <- 1-pU
  }
  else if(method=='jr')
  {
    sizeUp <- exp((rfr-0.5*(vol^2))*stepSize+vol*sqrt(stepSize))
    sizeDown <- exp((rfr-0.5*(vol^2))*stepSize-vol*sqrt(stepSize))
    pU <- 0.5
    pD <- 0.5
  }
  
  # create tree of stock prices
  stockTree[1,1] <- s0  
  bot <- 2
  for(n in 2:(steps+1))
  {
    stockTree[bot,n] <- stockTree[bot-1,n-1]*sizeDown
    for(k in (bot-1):1)
    {
      stockTree[k,n] <- stockTree[k,n-1]*sizeUp
    }
    bot <- bot + 1
  }
  
  # create tree of option prices
  for(i in 1:(steps+1))
  {
    callTree[i,steps+1] <- max(0,stockTree[i,steps+1]-strike)
    putTree[i,steps+1] <- max(0,strike-stockTree[i,steps+1])    
  }
  discount <- exp(-1*rfr*stepSize)
  bot <- steps
  for(n in steps:1)
  {
    for(k in bot:1)
    {
      if(type=='eu')
      {
        callTree[k,n] <- (callTree[k,n+1]*pU+callTree[k+1,n+1]*pD)*discount
        putTree[k,n] <- (putTree[k,n+1]*pU+putTree[k+1,n+1]*pD)*discount
      }
      else if(type=='am')
      {
        callTree[k,n] <- max((callTree[k,n+1]*pU+callTree[k+1,n+1]*pD)*discount,stockTree[k,n]-strike)
        putTree[k,n] <- max((putTree[k,n+1]*pU+putTree[k+1,n+1]*pD)*discount,strike-stockTree[k,n])
      }
      
    }
    bot <- bot - 1
  }
  
  
  tiy <- expT/12
  #BlackSholes 
  d1 <- (log(s0/strike)+(rfr+0.5*(vol*vol))*tiy)/(vol*sqrt(tiy))
  d2 <- d1-(vol*sqrt(tiy))
  Call <-s0*pnorm(d1)-strike*exp(-rfr*tiy)*pnorm(d2)
  Put <- -1*s0*pnorm(-d1)+strike*exp(-rfr*tiy)*pnorm(-d2)
 
  #Merton 
  d1M <- (log(s0/strike)+(rfr-DY+0.5*(vol*vol))*tiy)/(vol*sqrt(tiy))
  d2M <- d1M-(vol*sqrt(tiy))
  CallM <- exp(-DY*tiy)*s0*pnorm(d1M)-strike*exp(-rfr*tiy)*pnorm(d2M)
  PutM <- -exp(-DY*tiy)*s0*pnorm(-d1M)+strike*exp(-rfr*tiy)*pnorm(-d2M)
  
  # output
  list(callPrice = signif(callTree[1,1],4),putPrice = signif(putTree[1,1],4),
       bsCall=signif(Call,4),bsPut=signif(Put,4),bsmCall=signif(CallM,4),bsmPut=signif(PutM,4),
       callTree = round(callTree,3),putTree=round(putTree,3))
}