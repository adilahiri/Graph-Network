 calc_auc<-function(tpr,fpr){
  height = (tpr[-1]+tpr[-length(tpr)])/2
  width = -diff(fpr) 
  return(sum(height*width))
  
}
