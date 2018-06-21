#' DESS Function
#'
#' This function returns DESS values from all branches if tree is cut a fixed height.
#' @param tree tree= an hclust object created on "values"
#' @param values vector of values, on which histogram will be built
#' @param kk kk=Number of clusters at the tree-height the tree will be cut.
#' @param graph graph=Logical. Whether the histogram will be plotted or not.
#' @keywords DESS 
#' @export 
#' @examples
#' DESS()

DESS=function(tree,values,kk,graph){
  if(kk>1){  
    cut=cutree(tree,kk)
    ht=heights_per_k.dendrogram(as.dendrogram(tree))
    tree1.cut=cut(as.dendrogram(tree),min(ht[which(as.numeric(names(ht))==kk)])) #tree1.cut$lower has kk parts
    h=matrix(0,kk,3)
    colnames(h)=c("min","max","frequency")
    for(i in 1:kk){
      h[i,]=c(range(values[labels(tree1.cut$lower[[i]])]),length(labels(tree1.cut$lower[[i]])))
    }
    h=as.data.frame(h)
    diff_sum=rep(0,kk)
    threshold=(h$max-h$min)^2/3
    repeated=rep(0,1000)
    for(i in 1:kk){
      for(r in 1:1000){
        repeated[r]=sum((sort(runif(h$frequency[i],h$min[i],h$max[i]))-sort(values[values<=h$max[i]&values>=h$min[i]]))^2)
      }
      diff_sum[i]=mean(repeated)
    }
  }  
  if(kk==1){
    repeated=rep(0,1000)
    for(r in 1:1000)
      repeated[r]=sum((sort(values[labels(tree)])-sort(runif(length(values[labels(tree)]),min(values[labels(tree)]),max(values[labels(tree)]))))^2)
    diff_sum=mean(repeated)
    threshold=(max(values[labels(tree)])-min(values[labels(tree)]))^2/3
    graph=FALSE
    h=as.matrix(c(range(values[labels(tree)]),attr(tree,"members")),1,3)
    if(nrow(h)!=1)h=t(h)
    tree1.cut=tree
  }
  if(graph){
    plot(diff_sum,col=1,type="b",ylim=c(min(diff_sum,threshold)-0.5,max(diff_sum,threshold)+0.5))
    lines(threshold,col=2)
  }
  return(list("DESS"=diff_sum,"threshold"=threshold,"table"=h,"pruned.tree"=tree1.cut))
}
