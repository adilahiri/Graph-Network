condinformation_2<-function(X,Y,S=NULL, method="emp")
{
  if(is.null(S))
    Ires<-mutinformation(X,Y, method)
  else
  {
    U<-data.frame(S,X,Y)
    Hysx<-entropy(U,method)
    Hsx<-entropy(U[,c(1,2,3)],method)
    Hys<-entropy(U[,c(1,2,4)],method)
    Hs<-entropy(S,method)
    Ires<- Hys - Hs - Hysx + Hsx
  }
  Ires
}