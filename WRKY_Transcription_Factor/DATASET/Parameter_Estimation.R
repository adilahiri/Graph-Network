# Data Load and Preprocess for the real world data
require(xlsx)
source('C:/Users/adi44/Desktop/CodeHouse/Graph-Network/WRKY_Transcription_Factor/DATASET/minmax_normalize.R')
#library(bootnet)
library(Binarize)
data_1<- as.matrix(read.xlsx("GSE46365.xlsx",1))
data_2<- as.matrix(read.xlsx("GSE65046.xlsx",1))
data_3<- as.matrix(read.xlsx("data.xlsx",1))

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

for (iter in 1:nrow(data_set)){
  if(data_set[iter,3]==1)
    WRKY_18_40[iter]<-1
  else
    WRKY_18_40[iter]<-0
  
  if(data_set[iter,2]==1)
    WRKY_18_18[iter]<-1
  else
    WRKY_18_18[iter]<-0
  
  if(data_set[iter,1]==1)
    WRKY_40_40[iter]<-1
  else
    WRKY_40_40[iter]<-0
  
  
  if(data_set[iter,3]==1)
    WRKY_60_60[iter]<-1
  else
    WRKY_60_60[iter]<-0
  
}
WRKY_40_40<-cbind(WRKY_40_40)
WRKY_18_40<-cbind(WRKY_18_40)
WRKY_18_18<-cbind(WRKY_18_18)

Synth_Data <- cbind(data_set[,2],data_set[,1],WRKY_18_40,WRKY_18_18,WRKY_40_40,data_set[,3],WRKY_60_60,data_set[,4])


