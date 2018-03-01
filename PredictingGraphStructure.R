library(MASS)
library(ppcor)
library(infotheo)
library(pROC)
library(gdata)
library(igraph)
library(bnlearn)
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/calc_auc.R')
source('C:/Users/adi44/Desktop/CodeHouse/Statistical_Analysis/ROC_AUC.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/spec_sens.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/get_skeleton.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/single_witness.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/doublewitness.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/condinformation_2.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/find_neighbors.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/find_immoralities.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/evaluate_immorality.R')
source('C:/Users/adi44/Desktop/CodeHouse/R_Programming/convert_number_edge.R')
###########################################
A1=0.9; A0=0.1
B1=0.15;B0=0.85

C1A0B0=0.1;C1A1B0=0.9;C1A0B1=0.8;C1A1B1=0.2
C0A0B0=0.9;C0A1B0=0.1;C0A0B1=0.2;C0A1B1=0.8

D1C0=0.1;D1C1=0.85
D0C0=0.9;D0C1=0.15

E1C0=0.74;E1C1=0.25
E0C0=0.26;E0C1=0.75

F1D0G0=0.1;F1D1G0=0.9;F1D0G1=0.8;F1D1G1=0.1
F0D0G0=0.9;F0D1G0=0.1;F0D0G1=0.2;F0D1G1=0.9


G1D0E0=0.25;G1D1E0=0.85;G1D0E1=0.75;G1D1E1=0.9
G0D0E0=0.75;G0D1E0=0.15;G0D0E1=0.25;G0D1E1=0.1

n<-500
set.seed(2341)
data_A <-rbinom(n,size=1,prob=A1)
data_B <-rbinom(n,size=1,prob=B1)
data_C <- array(0,dim=c(0,n))
for(iter in 1:n){
  if(data_A[iter]==1 & data_B[iter]==1){
    data_C[iter]=rbinom(1,size=1,prob=C1A1B1)
  }
  else if(data_A[iter]==1 & data_B[iter]==0){
      data_C[iter]=rbinom(1,size=1,prob=C1A1B0)
  }
  else if(data_A[iter]==0 & data_B[iter]==0){
    data_C[iter]=rbinom(1,size=1,prob=C1A0B0)
  }
  else if(data_A[iter]==0 & data_B[iter]==1){
    data_C[iter]=rbinom(1,size=1,prob=C1A0B1)
  }

} 
data_D <- array(0,dim=c(0,n))
for (iter in 1:n){
  if (data_C[iter]==0){
    data_D[iter]=rbinom(1,size=1,prob=D1C0) 
  }
  else if(data_C[iter]==1)
  {
    data_D[iter]=rbinom(1,size=1,prob=D1C1)
  }
}

data_E <- array(0,dim=c(0,n))
for (iter in 1:n){
  if (data_C[iter]==0){
    data_E[iter]=rbinom(1,size=1,prob=E1C0) 
  }
  else if(data_C[iter]==1)
  {
    data_E[iter]=rbinom(1,size=1,prob=E1C1)
  }
}



data_G <- array(0,dim=c(0,n))
for(iter in 1:n){
  if(data_D[iter]==1 & data_E[iter]==1){
    data_G[iter]=rbinom(1,size=1,prob=G1D1E1)
  }
  else if(data_D[iter]==1 & data_E[iter]==0){
    data_G[iter]=rbinom(1,size=1,prob=G1D1E0)
  }
  else if(data_D[iter]==0 & data_E[iter]==0){
    data_G[iter]=rbinom(1,size=1,prob=G1D0E0)
  }
  else if(data_D[iter]==0 & data_E[iter]==1){
    data_G[iter]=rbinom(1,size=1,prob=G1D0E1)
  }
  
} 

data_F <- array(0,dim=c(0,n))
for(iter in 1:n){
  if(data_D[iter]==1 & data_G[iter]==1){
    data_F[iter]=rbinom(1,size=1,prob=F1D1G1)
  }
  else if(data_D[iter]==1 & data_G[iter]==0){
    data_F[iter]=rbinom(1,size=1,prob=F1D1G0)
  }
  else if(data_D[iter]==0 & data_G[iter]==0){
    data_F[iter]=rbinom(1,size=1,prob=F1D0G0)
  }
  else if(data_D[iter]==0 & data_G[iter]==1){
    data_F[iter]=rbinom(1,size=1,prob=F1D0G1)
  }
  
} 
Data_Table <- data.frame(data_A,data_B,data_C,data_D,data_E,data_F,data_G)
rm(data_A,data_B,data_C,data_D,data_E,data_F,data_G)
rm(A1, A0,B1,B0,C1A0B0,C1A1B0,C1A0B1,C1A1B1,C0A0B0,C0A1B0,C0A0B1,C0A1B1,D1C0,D1C1,D0C0,
   D0C1,E1C0,E1C1,E0C0,E0C1, F1D0G0,F1D1G0,
   F1D0G1,F1D1G1,F0D0G0,F0D1G0,F0D0G1,F0D1G1,G1D0E0,
   G1D1E0,G1D0E1,G1D1E1,G0D0E0,G0D1E0,G0D0E1,G0D1E1)


#### Compute correlation ####
Edge_Correlation <- cor(Data_Table,method="pearson")
#print(Edge_Correlation)
### PARTIAL CORRELATIONS
Edge_Correlation_Partial <- pcor(Data_Table,method="pearson")
#print(Edge_Correlation_Partial$estimate)
## MUTUAL INFORMATION 
Edge_Mutual_Info <- (mutinformation(Data_Table,method = "emp"))
#print(Edge_Mutual_Info)

### THRESHOLD
corr_thresh =seq(0.0,1,by=0.001)
#Edge_Prediction_Correlation <- (abs(Edge_Correlation)>0.5)
Edge_Correlation_Data <- as.vector(upperTriangle(Edge_Correlation,diag=FALSE,byrow = TRUE))
#Edge_Prediction_Correlation[Edge_Prediction_Correlation]=1
#precited_edges_Correlation <- as.vector(upperTriangle(Edge_Prediction_Correlation,diag=FALSE,byrow = TRUE))
Edge_Part_Correlation_Data <- as.vector(upperTriangle(Edge_Correlation_Partial$estimate,diag=FALSE,byrow = TRUE))
Edge_Mutual_Data <- as.vector(upperTriangle(Edge_Mutual_Info,diag=FALSE,byrow = TRUE))
actual_edges <-rbind(c(0,0,1,0,0,0,0),c(0,0,1,0,0,0,0),c(1,1,0,1,1,0,0),c(0,0,1,0,0,1,1),
                     c(0,0,1,0,0,0,1),c(0,0,0,1,0,0,1),c(0,0,0,1,1,0,0))
rownames(actual_edges)<-c("data_A","data_B","data_C","data_D","data_E","data_F","data_G")
colnames(actual_edges)<-c("data_A","data_B","data_C","data_D","data_E","data_F","data_G")
actual_edges<-as.data.frame(actual_edges)
actual_edges_row <- as.vector(upperTriangle(actual_edges,diag=FALSE,byrow = TRUE))
ROC_Coord_Corr <- ROC_AUC(corr_thresh,Edge_Correlation_Data,actual_edges_row)
ROC_Coord_Pat_Corr <- ROC_AUC(corr_thresh,Edge_Part_Correlation_Data,actual_edges_row)
ROC_Mutual <-ROC_AUC(corr_thresh, Edge_Mutual_Data,actual_edges_row)

plot(ROC_Coord_Corr[,2], ROC_Coord_Corr[,1], col='blue',type="l", xlim=c(0,1), ylim=c(0,1), lwd=2,main="correlation",xlab="FPR",ylab="TPR",pch=1)
grid()
plot(ROC_Coord_Pat_Corr[,2], ROC_Coord_Pat_Corr[,1], col='red',type="b", xlim=c(0,1), ylim=c(0,1), lwd=2,main="partial correlation",xlab="FPR",ylab="TPR",pch=2)
grid()
plot(ROC_Mutual[,2], ROC_Mutual[,1], col='green',type="b", xlim=c(0,1), ylim=c(0,1), lwd=2,main="Mutual Info",xlab="FPR",ylab="TPR",pch=3)
grid()

AUC_CORR <-calc_auc(ROC_Coord_Corr[,1],ROC_Coord_Corr[,2])
AUC_PAR_CORR<-calc_auc(ROC_Coord_Pat_Corr[,1],ROC_Coord_Pat_Corr[,2])
AUC_Mutual<-calc_auc(ROC_Mutual[,1],ROC_Mutual[,2])
print(AUC_CORR)
print(AUC_PAR_CORR)
print(AUC_Mutual)
AUC_Vector<-c(AUC_CORR,AUC_PAR_CORR,AUC_Mutual)
std_dev <-sd(AUC_Vector)
mids=barplot(AUC_Vector,main="AUC VALUES FOR EACH APPROACH",
             xlab="Correlation, Partial Correlation, Mutual Information",
             ylab="AUC VALUE",
             col=c("blue","green","red"),ylim=c(0,1.0))
arrows(mids,AUC_Vector-std_dev,mids,std_dev+AUC_Vector,code=3,angle = 90)

###### PART  C ###############################################################################

complete_graph <-as.matrix(actual_edges)
pos_zero = which(complete_graph==0)
complete_graph[pos_zero]=1

Mutual_Threshold <- 0.0042
# Compute Pairwise mutual info and remove edges if the mutual information between two node
# is less than 0.4
PairWise_Mutual_Information <-Edge_Mutual_Info
##Marginally indenpendent nodes annotated by row and col number of matrix 
Pos_Mutual_Info <- which(PairWise_Mutual_Information<Mutual_Threshold,arr.ind = TRUE)
Graph_Construct <- complete_graph
diag(Graph_Construct) <-0
Graph_Construct[Pos_Mutual_Info]<-0
#### COMPUTE CONDITIONAL MULTUAL INFORMATION NOW TO DETECT INDEPENDENCIES############
Upper_Complete <- upper.tri(complete_graph, diag = FALSE)
POS_IND<-cbind(row = row(complete_graph)[Upper_Complete], col = col(complete_graph)[Upper_Complete])

diag(Graph_Construct)<-1
Pos_Marginal_Ind <-matrix(nrow=dim(POS_IND)[1],ncol=2)
count<-1
for (iter in 1: dim(POS_IND)[1]){
  if(Graph_Construct[POS_IND[iter,1],POS_IND[iter,2]]==0){
    Pos_Marginal_Ind[count,]<-POS_IND[iter,]
    count=count+1
  }
}

Pos_Marginal_Ind<- Pos_Marginal_Ind[-c(count:dim(POS_IND)[1]),]
#Pos_Marginal_Ind <- which(Graph_Construct==0,arr.ind = TRUE)
list_indep <-as.list(cbind(rep(0,dim(POS_IND)[1])))


store_pos <- vector()
for (iter in 1: dim(Pos_Marginal_Ind)[1]){
  
  store_pos<-which(apply(POS_IND, 1, function(x) all(x == Pos_Marginal_Ind[iter,]))==TRUE)
  list_indep[[store_pos]]<-NA
}

##############################################################################################
Skeleton <- get_skeleton(Data_Table,POS_IND,Graph_Construct,list_indep,Mutual_Threshold)

Skeleton_Graph <- Skeleton[[1]]
Witness_Indep<-Skeleton[[2]] ## NA -> Marginally Indep; 0 -> dependent ,1-7 cond indep

List_Adj <- find_neighbors(Skeleton_Graph)

Immoral_Nodes<- find_immoralities(List_Adj)



Immorality_Mat<-matrix(ncol =3)
Immorality_List<-list()

if(length(Immoral_Nodes)>0){
  for (iter in 1:length(Immoral_Nodes)){
    Immorality_Mat<-rbind(Immorality_Mat,Immoral_Nodes[[iter]])}
  
  
  Immorality_Mat<-Immorality_Mat[2:dim(Immorality_Mat)[1],]
  Immorality_List <- evaluate_immorality(Immorality_Mat,POS_IND,Witness_Indep)
}
Immorality_Info<-data.frame()
if(length(Immorality_List)>0){
  pos_na <- which(is.na(Immorality_List))
  Immorality_List[pos_na]<- -1
  Immorality_Result <-rep("NO",length(Immorality_List))
  for (iter in 1: length(Immorality_List))
  {
    if (!(0 %in% Immorality_List[iter])){#!=0
      if(!(Immorality_Mat[iter,2] %in% Immorality_List[[iter]]))
      {
        Immorality_Result[iter]="YES"
      }
    }
  }
  Immorality_Info<-as.data.frame(cbind(Immorality_Mat,Immorality_List,cbind(Immorality_Result)))
  
}
########### RULE PROPAGATION #########################################################
diag(Skeleton_Graph)<-0
print(graph_from_adjacency_matrix(Skeleton_Graph,mode=c("undirected")))
pos_immo<-which(Immorality_Info$Immorality_Result=="YES")
pos_ud<-which(Skeleton_Graph==1,arr.ind = TRUE)
e = empty.graph(LETTERS[1:7])
Edges_ud<-convert_numbered_edge(pos_ud)










