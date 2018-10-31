rm(list=ls())
setwd("C:/Users/schwa/Desktop/bitcoin_dataset/2017-12")
tx=read.table("tx.csv",header = FALSE,sep = ",")
tx_input=read.table("txin.csv",header = FALSE,sep = ",")
tx_output=read.table("txout.csv",header = FALSE,sep = ",")
bh=read.table("bh.csv",header = FALSE,sep = ",")
colnames(bh)<-c("blockID","blockHash","block_timestamp","n_txs")
colnames(tx)<-c("txID","blockID","n_inputs","n_outputs")
colnames(tx_input)<-c("txID","input_seq","prev_txID","prev_output_seq","addrID","sum")
colnames(tx_output)<-c("txID","output_seq","addrID","sum")



# install.packages('plyr')
library(dplyr)
# install.packages("C:/Users/schwa/Desktop/clValid-master.tar.gz", repos = NULL, type="source")
# install.packages('factoextra')
library(factoextra)
# install.packages('tidyverse')
library(tidyverse)
library(gridExtra)
library (cluster)
# library(clValid)
library(caret)
library(GA)
library(clusterSim)
#주소별 거래량
countTx=count(tx_input,c(addrID))
colnames(countTx)<-c("addrID","freq")
countTx=arrange(countTx,desc(freq))
tmp_input=aggregate(sum~addrID, tx_input,sum)
countTx <- left_join(countTx, tmp_input, by = "addrID")

countTxOut=count(tx_output,c(addrID))
colnames(countTxOut)<-c("addrID","freq")
countTxOut=arrange(countTxOut,desc(freq))
tmp_input=aggregate(sum~addrID, tx_output,sum)
countTxOut <- left_join(countTxOut, tmp_input, by = "addrID")
colnames(countTxOut)<- c("addrID","output_freq","output_sum")
tmp_input=merge(countTx,countTxOut,by="addrID")

addressCount<-tmp_input
rm(tmp_input)
colnames(addressCount)=c("addrID","input_freq","input_sum","output_freq","output_sum")
addressCount<-cbind(addressCount,total_freq=addressCount$input_freq+addressCount$output_freq)
addressCount<-cbind(addressCount,total_sum=addressCount$input_sum+addressCount$output_sum)
addressCount=arrange(addressCount,desc(total_freq))
#address별 크기 비교 테이블 완성
target<-filter(addressCount,addrID==338838107|addrID==333286776|addrID==1776092|addrID==462866|addrID==17642733)

#sample address extraction
sampleDataset1=addressCount[sample(which(addressCount$total_freq>=100&addressCount$addrID!=target$addrID),100),]
sampleDataset2=addressCount[sample(which(addressCount$total_freq<100&addressCount$addrID!=target$addrID),1000),]
sampleDataset<- rbind(sampleDataset1,sampleDataset2)
sampleDataset<- rbind(sampleDataset,target)
# #sample transaction extraction
# tx_sum=cbind(tx,total_tx=tx$n_inputs+tx$n_outputs)
# sampleDataset1=tx_sum[sample(which((tx_sum$total_tx)>=100),100),]
# sampleDataset2=tx_sum[sample(which((tx_sum$total_tx)<100),900),]
# sampleDataset<- rbind(sampleDataset1,sampleDataset2)
dataset=data.frame()
for (r in 1:nrow(sampleDataset)) {
  tmpDataset=data.frame()
  #input transaction adder
  tmpInput=filter(tx_input, addrID == sampleDataset[r,1])
  colnames(tmpInput)[2]<-"seq"
  tmpInput$sum<-tmpInput$sum*-1
  tmpInput=tmpInput[,-c(3,4)]
  tmpTarget=merge(x = tmpInput, y = tx[ , c("txID", "blockID")], by = "txID", all.x=TRUE)
  t=merge(x=tmpTarget,y=bh[,c("blockID","block_timestamp")],by="blockID",all.x = TRUE)
  tmpDataset=rbind(tmpDataset,t)
  #output transaction adder
  tmpoutput=filter(tx_output, addrID == sampleDataset[r,1])
  colnames(tmpoutput)[2]<-"seq"
  tmpTarget=merge(x = tmpoutput, y = tx[ , c("txID", "blockID")], by = "txID", all.x=TRUE)
  t=merge(x=tmpTarget,y=bh[,c("blockID","block_timestamp")],by="blockID",all.x = TRUE)
  tmpDataset=rbind(tmpDataset,t)
  
  tmpDataset=arrange(tmpDataset,block_timestamp)
  
  balance=tmpDataset[1,5]
  bal<-data.frame(balance)
  for(i in 2:nrow(tmpDataset)){
    bal<-rbind(bal,tmpDataset[i,5]+bal[i-1,1])  
  }
  tmpDataset<-cbind(tmpDataset,bal)
  rm(bal)
  dataset=rbind(dataset,tmpDataset)
  cat(r,'/',nrow(sampleDataset),' transaction was created...\n')
}
write.table(dataset, "C:/Users/schwa/Desktop/bitcoin_dataset/2017-12/final_db.csv", sep=",")


dataset$block_timestamp=as.POSIXct(dataset$block_timestamp, origin="1970-01-01", tz="GMT")
dataset$block_timestamp=as.numeric(as.POSIXct(dataset$block_timestamp,"YYYY-MM-dd HH:mm:ss", tz = "GMT", origin="1970-01-01"))

dataset<-scale(dataset)


#--start GA
dataset=read.table("final_db.csv",header = TRUE,sep = ",")
# dataset=read.table("data300_2.csv",header = TRUE,sep = ",")
dataset<-dataset[,-1]

# library(lubridate)
# dataset$block_timestamp=ymd_hms(dataset$block_timestamp)
# dataset$block_timestamp=as.numeric(as.POSIXct(dataset$block_timestamp,"YYYY-MM-dd HH:mm:ss", tz = "GMT", origin="1970-01-01"))

dataset <- left_join(dataset, tx, by = "txID")
dataset=merge(x=dataset,y=bh[,c("blockID","n_txs")],by="blockID",all.x = TRUE)

dataset<-scale(dataset)
dataset<-dataset[,c(4,5,6,7)]
tmp<-dataset[,-c(1)]
tmp<-tmp[sample(tmp,100),]

tmpSample=tmp[sample(which(tmp$sum<100),100),]



write.table(dataset, "C:/Users/schwa/Desktop/bitcoin_dataset/2017-12/final_db_scale.csv", sep=",")


intern <- clValid(dataset, 2:3, clMethods = c("hierarchical","kmeans", "diana", "fanny", "pam","clara","model"), validation = "internal")
stab <- clValid(dataset, 2:3, clMethods = c("hierarchical","kmeans", "diana", "fanny", "pam", "clara","model"), validation = "stability")





# starting<-rfGA$initial(vars=8,popSize = 15)
DBI_con <- function(x) {
  v <- clValid(dataset[,x==1],2, clMethods = "kmeans", validation = "internal")
  score<--optimalScores(v)[1,1]
  return(score)
}
DBI_dun <- function(x) {
  if(sum(x)<=1){
    score<-0
  }else{
    clmethods<-c("hierarchical","kmeans","pam")
    v <- clValid(dataset[,x==1],nClust = 2:6, clMethods = clmethods, validation = "internal")
    score<-optimalScores(v)[2,1]  
  }
  return(score)
}
DBI_sil <- function(x) {
  v <- clValid(dataset[,x==1],2, clMethods = "kmeans", validation = "internal")
  score<-optimalScores(v)[3,1]
  return(score)
}
g1 <- ga(type = "binary",fitness = DBI_dun,popSize = 10,nBits = ncol(dataset),maxiter = 10)
g2 <- ga(type = "binary",fitness = DBI_dun,popSize = 20,nBits = ncol(dataset),maxiter = 30)
g3 <- ga(type = "binary",fitness = DBI_dun,popSize = 30,nBits = ncol(dataset),maxiter = 30)


#another method

dataset200=read.table("data200.csv",header = TRUE,sep = ",")
# dataset=read.table("dataset.csv",header = TRUE,sep = ",")

dataset200=scale(dataset200)

k2<-kmeans(dataset200,centers = 2,nstart = 25)
k3<-kmeans(dataset200,centers = 3,nstart = 25)
k4<-kmeans(dataset200,centers = 4,nstart = 25)
k5<-kmeans(dataset200,centers = 5,nstart = 25)
p1 <- fviz_cluster(k2, geom = "point", data = dataset200) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = dataset200) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = dataset200) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = dataset200) + ggtitle("k = 5")
grid.arrange(p1,p2,p3,p4,nrow=2)
a=index.DB(dataset200,k2$cluster)


# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  # km.res <- kmeans(dataset200, centers = k, nstart = 25)
  # ss <- silhouette(km.res$cluster, dist(dataset200))
  
  p2<-pam(dataset200,k)
  # ss <- silhouette(p2$cluster, dist(dataset200))
  # 
  # cl2<-clara(dataset200,k)
  # ss <- silhouette(cl2$cluster, dist(dataset200))
  # 
  # 
  # mean(ss[, 3])
  a=index.DB(dataset200,p2$cluster)
  a$DB
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#kmean GA
2
#kmean GA----
sol1<-g_kmean@solution
sol2<-g_clara@solution
sol3<-g_pam@solution
tmp1=data.frame(dataset200)
cluster_sol1<-kmeans(tmp1[,sol1[1,]==1],centers = 2,nstart = 25)
p1 <- fviz_cluster(cluster_sol1, geom = "point", data = dataset200) + ggtitle("kmean")
cluster_sol2<-clara(tmp1[,sol2[2,]==1],2)
p2 <- fviz_cluster(cluster_sol2, geom = "point", data = dataset200) + ggtitle("clara")
cluster_sol3<-pam(tmp1[,sol3==1],2)
p3 <- fviz_cluster(cluster_sol3, geom = "point", data = dataset200) + ggtitle("pam")
grid.arrange(p1,p2,p3,nrow=3)

cluster_sol1<-kmeans(tmp1[,sol1[1,]==1],centers = 2,nstart = 25)
p1 <- fviz_cluster(cluster_sol1, geom = "point",ellipse = FALSE ,data = dataset200) + ggtitle("kmean")
cluster_sol2<-kmeans(tmp1[,sol1[2,]==1],centers = 2,nstart = 25)
p2 <- fviz_cluster(cluster_sol2, geom = "point",ellipse = FALSE,  data = dataset200) + ggtitle("kmean")
cluster_sol3<-kmeans(tmp1[,sol1[3,]==1],centers = 2,nstart = 25)
p3 <- fviz_cluster(cluster_sol3, geom = "point",ellipse = FALSE, data = dataset200) + ggtitle("kmean")
cluster_sol4<-kmeans(tmp1[,sol1[4,]==1],centers = 2,nstart = 25)
p4 <- fviz_cluster(cluster_sol4, geom = "point",ellipse = FALSE, data = dataset200) + ggtitle("kmean")
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

#create new dataset by labeling previous dataset
labeled_dataset<-cbind(tmp1,cluster_sol4$cluster)
big_labeled_dataset<-cbind(testTable,o_cluster_sol1$cluster)


testTable=read.table("final_db_scale.csv",header = TRUE,sep = ",")


o_cluster_sol1<-kmeans(testTable[,sol1[4,]==1],centers = 2,nstart = 25)
o_p1 <- fviz_cluster(o_cluster_sol1, geom = "point",data = testTable) + ggtitle("kmean-GA")
o_cluster_sol2<-kmeans(testTable,centers = 2,nstart = 25)
o_p2 <- fviz_cluster(o_cluster_sol2, geom = "point",data = testTable) + ggtitle("kmean-Origin")
grid.arrange(o_p1,o_p2,ncol=2)


o_cluster_sol3<-kmeans(testTable[,sol2[1,]==1],centers = 2,nstart = 25)
o_p3 <- fviz_cluster(o_cluster_sol3, geom = "point",data = testTable) + ggtitle("clara-GA")
o_cluster_sol4<-kmeans(testTable,centers = 2,nstart = 25)
o_p4 <- fviz_cluster(o_cluster_sol4, geom = "point",data = testTable) + ggtitle("clara-Origin")
grid.arrange(o_p3,o_p4,ncol=2)

o_cluster_sol5<-kmeans(testTable[,sol3[1,]==1],centers = 2,nstart = 25)
o_p5 <- fviz_cluster(o_cluster_sol5, geom = "point",data = testTable) + ggtitle("PAM-GA")
o_cluster_sol6<-kmeans(testTable,centers = 2,nstart = 25)
o_p6 <- fviz_cluster(o_cluster_sol6, geom = "point",data = testTable) + ggtitle("PAM-Origin")
grid.arrange(o_p5,o_p6,ncol=2)


# ss <- silhouette(cluster_sol1$cluster, dist(dataset200))
# sil<-mean(ss[, 3])
# dbi=index.DB(dataset200,cluster_sol1$cluster)
# cat('\t','silhouette : ',sil,'\t, DB index : ',dbi$DB,'\n')
# ss <- silhouette(cluster_sol2$cluster, dist(dataset200))
# sil<-mean(ss[, 3])
# dbi=index.DB(dataset200,cluster_sol2$cluster)
# cat('\t','silhouette : ',sil,'\t, DB index : ',dbi$DB,'\n')
# 
# 
# h2<-hclust(dist(dataset200),method = "ave")
# plot(h2, hang=-1)
# rect.hclust(h2, k=3)
# groups <- cutree(h2, k=3) 
# 
# p2<-pam(dataset200,3)
# fviz_cluster(k2, data = dataset200)
# cl2<-clara(dataset200,3)
# fviz_cluster(cl2, data = dataset200)
# ss <- silhouette(cl2$cluster, dist(dataset200))
# mean(ss[,3])

#classification...


write.table(labeled_dataset, "D:/dataset/bitcoin_dataset/2017-12/labeled_dataset.csv", sep=",")



write.table(labeled_dataset, "C:/Users/schwa/Desktop/bitcoin_dataset/2017-12/final_labeled_dataset.csv", sep=",")

write.table(big_labeled_dataset, "C:/Users/schwa/Desktop/bitcoin_dataset/2017-12/final_big_labeled_dataset.csv", sep=",")