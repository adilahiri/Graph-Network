# Data Load and Preprocess 
require(xlsx)
library(bootnet)
data_1<- as.matrix(read.xlsx("Data Matrix_17464.xlsx",1))
data_2<- as.matrix(read.xlsx("Data Matrix_18978.xlsx",1))
data_3<- as.matrix(read.xlsx("Data Matrix_19109.xlsx",1))

data_1 <- binarize(data_1)
data_2 <- binarize(data_2)
data_3 <- binarize(data_3)

data_set <- rbind(data_1,data_2,data_3)

# Make Complete Graph 
Complete_Graph <- matrix(1, nrow=9,ncol=9)
diag(Complete_Graph)<-0 # Node does not interact with itself or have a "self loop"

# Computer Marginal Independence 

