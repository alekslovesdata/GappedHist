#' Plot DESS Function
#'
#' This function makes a plot using M, the output from the histbyDESS() function.
#' $all.dess= A list of all DESS values calculated from the bins
#'  $all.threshold = A list of (b_j-a_j)^2/3 values from the bins
#' @param values
#' @param M M=the output from the histbyDESS() function
#' @keywords plot
#' @export
#' @examples
#' plotDESS()

plotDESS=function(values,M){
  all.dess=rep(0,length(M$trees))
  all.threshold=rep(0,length(M$trees))
  for(j in 1:length(all.dess)){
    all.dess[j]=DESS(M$trees[[j]],values=values,kk=1,graph = FALSE)$DESS
    all.threshold[j]=(M$table$max[j]-M$table$min[j])^2/3
  }
  plot(all.dess,col=1,type="b",ylim=c(min(all.dess,all.threshold)-0.5,max(all.dess,all.threshold)+0.5))
  lines(all.threshold,col=2)
  return(list(all.dess=all.dess,all.threshold=all.threshold))
}
