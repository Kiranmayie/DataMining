##  Execute different feature selection algorithms using the function fss_Algorithms
fss_Algorithms <- function(data,Fss_Alg=c("CFS","ReliefF","FAST"),corr.threshold=0.6)
  {
  library(roahd)
  library(SDMTools)
  library(R.matlab)
  library(stats)
  library(ica)
  library(caret)
  library(kernlab)
  library(Biocomb)
  library(forecast)
  library(heuristica)
  #data<-read.csv("C:/Users/Kiranmayie/Desktop/Data mining project/Data1-Diabetes.csv")
  x<-data[,-ncol(data)]
  y<-data$Y
  attrs.nominal=ncol(data)
  data[,ncol(data)]<-as.factor(data[,ncol(data)])
  
  
   if(Fss_Alg == "CFS")
    {
    # CFS
    set.seed(3233)
    start.pt<-proc.time()
    start_time<-Sys.time()
      fss.cfs<-select.forward.Corr(data,disc.method="MDL",attrs.nominal=numeric()) 
    cfs.data<-data[,c(fss.cfs)]
    View(cfs.data)
    train <- createDataPartition(y,p = 0.7, list = FALSE)
    training.data <- cfs.data[train,]
    training.class<-factor(data$Y[train])
    testing.data <- cfs.data[-train,]
    testing.class <- data$Y[-train]
    actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                             trControl=trctrl,
                             tuneLength = 10)
    rsvm.predicted <- predict(rsvm.train.model,testing.data)
    rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    cfs.rsvm.acc<-accuracy(actual,rsvm)
    end_time<-Sys.time()
    configuration_time<-end_time-start_time
    print(configuration_time)
    return(cfs.rsvm.acc)
  }
  else if(Fss_Alg == "ReliefF")
    {
    if(ncol(data)>7000){
      data1<-data[,1:2500]
      fssRelief<-select.relief(data1)
      data2<-data[,2501:ncol(data)]
      fssRelief1<-select.relief(data2)
    }else{
      fssRelief<-select.relief(data)
    }
     
     selected.features<-fssRelief[,3]
     Relief.rsvm.acc<-data.frame()
     for(i in 1:7){
       Relief.rsvm.acc[1,i]<-NA
     }
     Relief.rsvm.acc[]
    for(i in 2:length(selected.features)){
      Relief.data<-data[,c(selected.features[1:i])]
      train <- createDataPartition(y,p = 0.7, list = FALSE)
      training.data <- Relief.data[train,]
      training.class<-factor(data$Y[train])
      testing.data <- Relief.data[-train,]
      testing.class <- data$Y[-train]
      actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                               trControl=trctrl,
                               tuneLength = 10)
      rsvm.predicted <- predict(rsvm.train.model,testing.data)
      rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
      Relief.rsvm.acc<-accuracy(actual,rsvm)
    }
      #acc.max<-which(Relief.rsvm.acc[,5]==max(Relief.rsvm.acc[,5]))
     # index<-seq(2:acc.max[1]+10)
     # plot(index,Relief.rsvm.acc[(1:length(index)),5],main="Relief Feature Selection",xlab="No of Features",ylab="Accuracy(0-1)scale",lwd=3,type = "l")
      #return(list(Relief.rsvm.acc,acc.max))
       return(Relief.rsvm.acc)
  
   }
  
  else if(Fss_Alg == "FAST")
 {
         ## initialization
    x<-Dataset[,-ncol(Dataset)]
     y<-Dataset$Y
     attrs.nominal=ncol(Dataset)
     Dataset[,ncol(Dataset)]<-as.factor(Dataset[,ncol(Dataset)])
    target<-y
    fs<-Dataset
    is.normalized=TRUE
    fss <- vector()
    start.index = 1
      ## Extract a small grid from dataset
    input.row.count = nrow(fs)
    input.column.count = ncol(fs)
    temp <- input.column.count
    if (temp < floor(input.row.count/2)) {
        Grid.size <- temp
      }else {
        Grid.size <- floor(input.row.count/2)
        temp <- temp - Grid.size
      }
      stop.index <- start.index + Grid.size - 1
      
      while (stop.index <= input.column.count)
      {
        if(stop.index > start.index)
        {
          Grid.data <- fs[, start.index:stop.index] 
          ## Remove NA values and replace with mean column value
          if(anyNA(Grid.data))
          {
            data.withoutNA <- remove.NA(Grid.data)}else{
              data.withoutNA <- Grid.data
            }
          ##Function to normalize data in 0-1 range
          if(is.normalized){data.normalized<-data.withoutNA}else{
            data.normalized <- (normlize.data(data.withoutNA))}
          #CostMatrix <- abs(cor(as.numeric(data.normalized)))
          CostMatrix <- abs(cor(data.normalized[sapply(data.normalized,function(x)!is.factor(x))]))
          CostMatrix.row <- nrow(CostMatrix)
          Graph.arcs <- matrix(ncol = 3)[-1, ]
          for (i in 1:CostMatrix.row) {
            for (j in 1:i) {
              if (CostMatrix[i,j] != 0 & CostMatrix[i,j] != Inf & !is.na(CostMatrix[i,j])) {
                Graph.arcs <- rbind(Graph.arcs,cbind(i+start.index-1,j+start.index-1,CostMatrix[i,j]))
              }
            }
          }
          nodes <- start.index:stop.index
          ## Construct a Maximum Spanning Tree
          tree.arcs<-msTreePrim(nodes,Graph.arcs,start.index)
          ## removhe the arc with minimum cost
          tree.arcs.remove <- which(tree.arcs[,3]<0.5)
          if(length(tree.arcs.remove) != 0){
            tree.arcs <- tree.arcs[-c(tree.arcs.remove), ]
            ## Cluster the data 
            Clusters <- Clustering(tree.arcs)}else{
              Clusters <- list()
              Clusters[[1]] <- start.index:stop.index
            }
          ## Evaluate Weight Score for input subset
          cluster.count <- length(Clusters)
          if(cluster.count>0){
            for (i in 1:cluster.count) {
              irrdata <- data.normalized[,sapply(c(Clusters[[i]]),FUN=function(x){x<-x-start.index+1;return(x)})]
              Target <- target[sapply(c(Clusters[[i]]),FUN=function(x){x<-x-start.index+1;return(x)})]
              Weight_score <- round(Weight(data.normalized,Target),1)
              feature.selected <- which(Weight_score==Weight_score)
              #fss <- cbind(fss,Clusters[[i]][feature.selected])
              fss<-append(fss,feature.selected, after = length(fss))
            }
          }
          
          start.index <- stop.index + 1
          if (temp < floor(input.row.count/2)){
            Grid.size <- temp}else {
              Grid.size <- floor(input.row.count/2)
              temp <- temp - Grid.size
            }
          stop.index <- start.index + Grid.size - 1  
        }
        else{
          stop.index <- input.column.count+1}
      }
     FAST.rsvm.acc<-data.frame()
        for(i in 1:7){
          FAST.rsvm.acc[1,i]<-NA
        }
     for(i in 1:length(feature.selected)){
       FAST.data<-Dataset[,c(feature.selected[1:i])]
     }
    train <- createDataPartition(y,p = 0.7, list = FALSE)
    training.data <- FAST.data[train,]
    training.class<-factor(Dataset$Y[train])
    testing.data <- FAST.data[-train,]
    testing.class <- factor(Dataset$Y[-train])
    actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                               trControl=trctrl,
                               tuneLength = 10)
    rsvm.predicted <- predict(rsvm.train.model,testing.data)
    rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    FAST.rsvm.cm<-confusion.matrix(actual,rsvm)
    FAST.rsvm.acc<-accuracy(actual,rsvm)
    #return(list(FAST.rsvm.acc,fss.FAST,FAST.data))
    return(FAST.rsvm.acc)
        }
  }







