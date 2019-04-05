#Create DBN space
library(acl)
library(abind)

rm(list=ls())
flat3<-expand.grid(data.frame(matrix(0:3, 4, 3)))
DBN3<-array(data = 0, dim=c(3,3,nrow(flat3)))

for (i in 1:nrow(flat3))
{
  DBN3[,,i][upper.tri(DBN3[,,i])]<-unlist(flat3[i,])
  
  #Reverse only
  DBN3[,,i][t(DBN3[,,i])==2]<-1
  DBN3[,,i][DBN3[,,i]==2]<-0
  
  #Both directions
  DBN3[,,i][t(DBN3[,,i])==3]<-1
  DBN3[,,i][DBN3[,,i]==3]<-1
}

#Make the first 25 of these indexed the same as the acylic graphs
acyclic<-c()
for (i in 1:dim(DAGs_3)[3])
{
  acyclic<-c(acyclic, which(apply(sweep(DBN3, c(1,2), DAGs_3[,,i], '=='), 3, all)))
}

DBN3<-abind(DAGs_3, DBN3[,,!1:64%in%acyclic])


#4 variable!
flat4<-expand.grid(data.frame(matrix(0:3, 4, 6)))
DBN4<-array(data = 0, dim=c(4,4,nrow(flat4)))

for (i in 1:nrow(flat4))
{
  DBN4[,,i][upper.tri(DBN4[,,i])]<-unlist(flat4[i,])
  
  #Reverse only
  DBN4[,,i][t(DBN4[,,i])==2]<-1
  DBN4[,,i][DBN4[,,i]==2]<-0
  
  #Both directions
  DBN4[,,i][t(DBN4[,,i])==3]<-1
  DBN4[,,i][DBN4[,,i]==3]<-1
}

#Make the first 25 of these indexed the same as the acylic graphs
acyclic<-c()
for (i in 1:dim(DAGs_4)[3])
{
  acyclic<-c(acyclic, which(apply(sweep(DBN4, c(1,2), DAGs_4[,,i], '=='), 3, all)))
}

DBN4<-abind(DAGs_4, DBN4[,,!1:4096%in%acyclic])



get_ix<-function(graph)
{
  if (dim(graph)[1]==3)
  {
    out<-which(apply(sweep(DBN3, c(1,2), graph, '=='), 3, all))
  } else if (dim(graph)[1]==4){
    out<-which(apply(sweep(DBN4, c(1,2), graph, '=='), 3, all))
  } else
  {
    out = NA
  }

  out
}

save(file='..data/dbn.rdata', DBN3, DBN4, get_ix)