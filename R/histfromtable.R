#' Histogram from Table Function
#'
#' This function builds a histogram from range and frequency of bins given.
#' @param T T= a data frame with three columns describing a set of histograms. T$min= left boundary, T$max=right boundary, T$frequency= bin freuency.
#' @keywords cats
#' @export
#' @examples
#' histfromtable()

histfromtable=function(T){
  T=T[order(T$min),]
  r = range(c(T$max,T$min))
  plot(NA,ylim = c(0,round(max(T$frequency),2)+0.01),xlim = c(r[1]-0.05,r[2]+0.05),ylab="frequency",xlab="data")
  abline(h=0,col="grey")
  
  for(i in 1:nrow(T)){
    if(T[i,1]==1){
      points(T[i,2],T$prob[i],pch=19)
    }else{
      lines(rep(T$min[i],2),c(0,T$frequency[i]))
      lines(rep(T$max[i],2),c(0,T$frequency[i]))
      lines(c(T$min[i],T$max[i]),rep(T$frequency[i],2))
    }
  }
}