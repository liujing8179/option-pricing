priceOption <- function(expT=6,s0=30.00,strike=30,rfr=0.1,vol=0.2,type='eu',steps=8,DY=0.0263)
{
  stockTree <- matrix(0,steps+1,steps+1)
  callTree <- matrix(0,steps+1,steps+1)
  putTree <- matrix(0,steps+1,steps+1)
  
  stepSize <- expT/(12*steps)
  sizeUp <- exp(vol*sqrt(stepSize))
  sizeDown <- 1/sizeUp
  pU <- (exp((rfr-DY)*stepSize)-sizeDown)/(sizeUp-sizeDown)
  pD <- 1-pU
  
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
  
  
  
  list(callPrice = callTree[1,1],putPrice = putTree[1,1],callTree = callTree,putTree=putTree)
}