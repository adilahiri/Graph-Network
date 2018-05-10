# Data Load and Preprocess for the real world data
setwd("C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET")
require(xlsx)
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/minmax_normalize.R')
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/assign_value.R')
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/calc_shape_param.R')
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/binarize_mean_median.R')

library(Binarize)
set.seed(101)
data_1<- as.matrix(read.xlsx("GSE46365.xlsx",1))
data_2<- as.matrix(read.xlsx("GSE65046.xlsx",1))
data_3<- as.matrix(read.xlsx("GSE76827.xlsx",1))

data_1_norm<- apply(data_1,2,minmax_normalize)
data_2_norm<- apply(data_2,2,minmax_normalize)
data_3_norm<- apply(data_3,2,minmax_normalize)

#data_1_bin <- as.matrix(binarizeMatrix(data_1_norm,method="kMean"))[,1:4]
#data_2_bin <- as.matrix(binarizeMatrix(data_2_norm,method="kMeans"))[,1:4]
#data_3_bin <- as.matrix(binarizeMatrix(data_3_norm,method="kMeans"))[,1:4]

data_1_bin <- binarize_mean_median(data_1_norm,method="median")
data_2_bin <-binarize_mean_median(data_2_norm,method="median")
data_3_bin <- binarize_mean_median(data_3_norm,method="median")

data_set <- rbind(data_1_bin,data_2_bin,data_3_bin)


# Generate Synthetic data 
WRKY_18_40<-vector()
WRKY_18_18 <-vector()
WRKY_40_40<-vector()
WRKY_60_60<-vector()
for (iter in 1:nrow(data_set)){
  if(sum(data_set[iter,3],data_set[iter,2],data_set[iter,1])==3)
    WRKY_18_40[iter]<-1
  else if (sum(data_set[iter,3],data_set[iter,2],data_set[iter,1])==0)
    WRKY_18_40[iter]<-0
  else if (sum(data_set[iter,3],data_set[iter,2],data_set[iter,1])==2)
    WRKY_18_40[iter]<-assign_value(1)
  else if (sum(data_set[iter,3],data_set[iter,2],data_set[iter,1])==1)
    WRKY_18_40[iter]<-assign_value(0)
  
  # WRKY18-18
  if(sum(data_set[iter,2],data_set[iter,4])==0)
    WRKY_18_18[iter]<-0
  else if (sum(data_set[iter,2],data_set[iter,4]==2))
    WRKY_18_18[iter]<-1
  else if (data_set[iter,2]==0 & data_set[iter,4]==1)
    WRKY_18_18[iter] <- assign_value(0)
  else if (data_set[iter,2]==1 & data_set[iter,4]==0)
    WRKY_18_18[iter] <- assign_value(1)
  
  
  
  #WRKY 40-40
  if(data_set[iter,1] ==0 & data_set[iter,4]==0)
    WRKY_40_40[iter]<-assign_value(0)
  
  else if(data_set[iter,1]==1 & data_set[iter,4]==1)
    WRKY_40_40[iter]<-assign_value(1)
  
  else if(data_set[iter,1]==1 & data_set[iter,4]==0)
    WRKY_40_40[iter]<-1
  else if(data_set[iter,1]==0 & data_set[iter,4]==1)
    WRKY_40_40[iter]<-0
  
  ## WRKY 60-60
  if(sum(data_set[iter,3],data_set[iter,4])==0)
    WRKY_60_60[iter]<-0
  else if(sum(data_set[iter,3],data_set[iter,4])==2)
    WRKY_60_60[iter]<-1
  else if(data_set[iter,3]==1 & data_set[iter,4]==0)
    WRKY_60_60[iter]<-assign_value(1)
  else if(data_set[iter,3]==0 & data_set[iter,4]==1)
    WRKY_60_60[iter]<-assign_value(0)
  
}
WRKY_40_40<-cbind(WRKY_40_40)
WRKY_18_40<-cbind(WRKY_18_40)
WRKY_18_18<-cbind(WRKY_18_18)

Synth_Data <- cbind(data_set[,2],data_set[,1],WRKY_18_40,WRKY_18_18,WRKY_40_40,data_set[,3],WRKY_60_60,data_set[,4])
rownames(Synth_Data)<-NULL
colnames(Synth_Data)<-c("WRKY18","WRKY40","WRKY_18_40","WRKY_18_18","WRKY_40_40","WRKY60","WRKY_60_60","RD29A")
write.csv(Synth_Data,file="Synthetic_Data.csv")

# Plot for activation and inhibition
count1<-list()
for(iter in 1:8){
  count1[[iter]]<- table(Synth_Data[,iter])
}
c1<-unlist(count1)
c2<-matrix(c1,ncol=2,byrow=TRUE)
colnames(c2)<-c("Inhibited","Activated")
rownames(c2)<-c("WRKY18","WRKY40","WRKY18-40","WRKY18-18","WRKY40-40","WRKY60","WRKY60-60","RD29A")


barplot(t(c2),ylim =c(0,200),main="Activation vs Inhibition",ylab="count",col=c("black","red"))
legend("topleft",c("Inhibited","Activated"),fill=c("black","red"))



### Estimate the Shape parameters and the expected values for each nodes

Nodes <- 8
obs<- nrow(Synth_Data)
 
alpha_init <- 1
beta_init <- 1

shape_param <- list()
Parents<-matrix()
shape_mat<- matrix(ncol=2)
for (iter_node in 1:Nodes){
  
  if (iter_node ==1 || iter_node ==2)
    Parents <- NULL
  else if( iter_node ==3)
    Parents<- cbind(Synth_Data[,1],Synth_Data[,2])
  else if(iter_node==4)
    Parents<-cbind(Synth_Data[,1])
  else if(iter_node==5)
    Parents<- cbind(Synth_Data[,2])
  else if(iter_node==6)
    Parents<- cbind(Synth_Data[,3])
  else if(iter_node==7)
    Parents<- cbind(Synth_Data[,6])
  else if(iter_node==8)
    Parents<- cbind(Synth_Data[,4],Synth_Data[,5],Synth_Data[,7])
  
  shape_param [[iter_node]] <- calc_shape_param(alpha_init,beta_init,Synth_Data[,iter_node],Parents)
  shape_mat <- rbind(shape_mat,shape_param[[iter_node]])
}

shape_mat <-shape_mat[-1,]

Expected_Values <- signif(cbind(shape_mat[,1]/(shape_mat[,1]+shape_mat[,2])),digits = 3)

shape_mat2<-cbind(shape_mat,100*Expected_Values,100*(1-Expected_Values))





