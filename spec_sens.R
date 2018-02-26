spec_sens <-function(predicted,truth){
  pos_a_1= which(predicted==1)
  pos_b_1= which(truth==1)
  count_TP= length(intersect(pos_a_1,pos_b_1))
  pos_a_0= which(predicted==0)
  pos_b_0= which(truth==0)
  count_TN= length(intersect(pos_a_0,pos_b_0))
  count_FP= length(intersect(pos_a_1,pos_b_0))
  count_FN= length(intersect(pos_a_0,pos_b_1))
  arr=c(count_TP,count_TN,count_FP,count_FN)
  return(arr)
}