#' CDF from Table Function
#'
#' A function building and plotting CDF, with fitted lines for each branch, from range and frequency of bins given.
#' input
#'  T$min= left boundary, T$max=right boundary, T$frequency= bin frequency.
#' @param values values=vector of values, on which histogram will be built
#' @param T T= a data frame with three columns describing a set of histograms.
#' @keywords cats
#' @export
#' @examples
#' cdffromtable()




cdffromtable=function(values,T){
  
  T=T[,c(3,1,2)]
  T=T[order(T$min),]
  y = c(0,T$frequency)
  p = y/sum(T$frequency)
  P = matrix(0,length(p),1)
  for(i in 1:length(P)){
    P[i,1] = sum(p[1:i])
  }
  
  COL = "red"
  CDF = ecdf(values)
  plot(CDF,col="grey",main="empirical CDF")
  for(i in 1:(length(P)-1)){
    if(T[i,1]>1){
      lines(T[i,2:3],P[i:(i+1)],col=COL,lwd=2)
      points(T[i,2:3],P[i:(i+1)],col=COL,pch=19)
      #if(i < (length(P)-1)){
      # lines(c(T[i,3],T[(i+1),2]),rep(P[i+1],2),col=COL)
      #}
    }
    if(T[i,1]==1){
      lines(c(T[i,3],T[(i+1),2]),rep(P[i+1],2),col=COL)
      points(T[i,3],P[i+1],pch=19,col=COL)
    }
  }
}
