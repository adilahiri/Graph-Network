# Data Load and Preprocess for the real world data
setwd("C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET")
require(xlsx)
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/minmax_normalize.R')
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/assign_value.R')
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/calc_shape_param.R')
library(Binarize)
set.seed(101)
data_1<- as.matrix(read.xlsx("GSE46365.xlsx",1))
data_2<- as.matrix(read.xlsx("GSE65046.xlsx",1))
data_3<- as.matrix(read.xlsx("GSE76827.xlsx",1))

data_1_norm<- apply(data_1,2,minmax_normalize)
data_2_norm<- apply(data_2,2,minmax_normalize)
data_3_norm<- apply(data_3,2,minmax_normalize)



data_1_bin <- as.matrix(binarizeMatrix(data_1_norm,method="BASCA"))[,1:4]
data_2_bin <- as.matrix(binarizeMatrix(data_2_norm,method="BASCA"))[,1:4]
data_3_bin <- as.matrix(binarizeMatrix(data_3_norm,method="BASCA"))[,1:4]

data_set <- rbind(data_1_bin,data_2_bin,data_3_bin)


# Generate Synthetic data 
WRKY_18_40<-vector()
WRKY_18_18 <-vector()
WRKY_40_40<-vector()
WRKY_60_60<-vector()
value<-9
for (iter in 1:nrow(data_set)){
  if(data_set[iter,2]==1 & data_set[iter,1] ==1)
    WRKY_18_40[iter]<-assign_value(1)
  else
    WRKY_18_40[iter]<-assign_value(0)
  
  if(data_set[iter,2]==1)
    WRKY_18_18[iter]<-assign_value(1)
  else
    WRKY_18_18[iter]<-assign_value(0)
  
  if(data_set[iter,1]==1)
    WRKY_40_40[iter]<-assign_value(1)
  else
    WRKY_40_40[iter]<-assign_value(0)
  
  
  if(data_set[iter,3]==1)
    WRKY_60_60[iter]<-assign_value(1)
  else
    WRKY_60_60[iter]<-assign_value(0)
  
}
WRKY_40_40<-cbind(WRKY_40_40)
WRKY_18_40<-cbind(WRKY_18_40)
WRKY_18_18<-cbind(WRKY_18_18)

Synth_Data <- cbind(data_set[,2],data_set[,1],WRKY_18_40,WRKY_18_18,WRKY_40_40,data_set[,3],WRKY_60_60,data_set[,4])
rownames(Synth_Data)<-NULL
colnames(Synth_Data)<-c("WRKY18","WRKY40","WRKY_18_40","WRKY_18_18","WRKY_40_40","WRKY60","WRKY_60_60","RD29A")
write.csv(Synth_Data,file="Synthetic_Data.csv")


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








