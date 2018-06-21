#' Hist by DESS Function
#'
#' This function dynamically cuts a tree (not at fixed height) according to DESS threshold. (application of algorithm1)
#' inputs: 
#' values=vector of values, on which histogram will be built
#' epsilon: from the DESS criterion DESS < epsilon*(b_j-a_j)^2/3. Default value is 1 if missing
#' L0= minimum DESS values which is tolerated in the bins. Default is 0.1* tree height if missing.
#' Graph=Logical. Whether the histogram will be plotted or not. Default is FALSE.
#' Output:
#'  $table= a table containing maximum, minimum and frequency of each bin.
#'  $trees - a list all pruned branches from the main tree that constructed the histogram
#'  @param values
#'  @param epsilon
#'  @param L0
#'  @param graph
#' @keywords Hist
#' @import dendextend 
#' @export
#' @examples
#' histbyDESS()

histbyDESS=function(values,epsilon,L0,graph){ 
  if(missing(epsilon))epsilon=1
  if(missing(graph))graph=FALSE
  tree=hclust(dist(values))
  if(missing(L0))L0=0.1*attr(as.dendrogram(tree),"height")
  dess_list=c()
  threshold_list=c()
  table_list=c()
  tree_list=c()
  all.possible.trees=list()
  i=1
  sel=0
  all.possible.trees
  ht=heights_per_k.dendrogram(as.dendrogram(tree))
  ce_input=DESS(tree,values = values,kk=2,graph = FALSE)
  ce_input$DESS<L0 #if TRUE then break
  ce_input$DESS<2*ce_input$threshold #if TRUE then break
  new_trees=ce_input$pruned.tree$lower
  
  if(heights_per_k.dendrogram(new_trees[[1]])[1]>heights_per_k.dendrogram(new_trees[[2]])[1]){selected=1;not.selected=2}
  if(heights_per_k.dendrogram(new_trees[[1]])[1]<heights_per_k.dendrogram(new_trees[[2]])[1]){selected=2;not.selected=1}
  all.possible.trees[[i]]=new_trees[[not.selected]]
  i=i+1
  new_kk=2
  selected #this determines which branch of the pruned tree will be selected as the higest available one.
  height.selected=heights_per_k.dendrogram(new_trees[[selected]])[1]
  tree.selected=new_trees[[selected]]
  DESS.selected=ce_input$DESS[selected]
  table.selected=ce_input$table[selected,]
  threshold.selected=ce_input$threshold[selected]
  repeat{
    if(DESS.selected<max(L0,epsilon*threshold.selected)){ #if true put a stop sign on that node, collect the h table row
      sel=sel+1
      dess_list=c(dess_list,DESS.selected)
      threshold_list=c(threshold_list,threshold.selected)
      table_list=rbind(table_list,c(table.selected))
      tree_list[[sel]]= tree.selected
      #i=i-1
    }
    if(length(unlist(sapply(tree_list,labels)))==length(values))break
    #if false then go to the following step
    #find the second highest node
    if(DESS.selected>max(L0,epsilon*threshold.selected)){
      all.possible.trees[[i]]=cut(as.dendrogram(tree.selected),min(ht[which(names(ht)==new_kk)]))$lower[[1]]
      i=i+1
      all.possible.trees[[i]]=cut(as.dendrogram(tree.selected),min(ht[which(names(ht)==new_kk)]))$lower[[2]]
      i=i+1
    }
    
    heights=rep(0,length(all.possible.trees))
    for(j in 1: length(all.possible.trees)){
      heights[j]=attr(all.possible.trees[[j]],"height")
    }
    to.remove=which(heights==0)
    if(length(to.remove)>0){
      heights=heights[-to.remove]
      for(k in 1:length(to.remove)){
        sel=sel+1
        tree_list[sel]= all.possible.trees[to.remove[k]]
        all.possible.trees=all.possible.trees[-to.remove[k]]
        i=i-1
      }
      
    }
    
    selected=min(which(heights==max(heights)))
    not.selected=which(heights!=max(heights))
    new_trees=all.possible.trees
    new_kk=min(as.numeric(names(which(round(ht,5)==round(max(heights),5)))))
    ce_input=DESS(new_trees[[selected]],values=values,kk=1,graph = FALSE)
    DESS.selected=ce_input$DESS
    height.selected=max(heights)
    threshold.selected=ce_input$threshold
    table.selected=ce_input$table
    tree.selected=new_trees[[selected]]
    all.possible.trees=all.possible.trees[-selected]
    i=i-1
  }
  all.labels=sapply(tree_list,labels)
  all_heights=rep(0,length(tree_list))
  for(j in 1: length(tree_list)){
    all_heights[j]=attr(tree_list[[j]],"height")
  }
  full.table=matrix(0,length(all.labels),3)
  colnames(full.table)=c("min","max","frequency")
  for(i in 1:length(all.labels)){
    all.values=values[all.labels[[i]]]
    full.table[i,]=c(range(all.values),length(all.values))
  }
  full.table=as.data.frame(full.table)
  full.table=full.table[order(full.table$min),]
  if(graph==TRUE){
    par(mfrow=c(1,2))
    cdffromtable(values,full.table)
    histfromtable(full.table)
  }
  return(list("table"=full.table,"trees"=tree_list[as.numeric(rownames(full.table))]))
}
