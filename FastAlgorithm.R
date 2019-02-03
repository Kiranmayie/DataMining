library(Biocomb)

x<-data[,-ncol(data)]
y<-data$Y
attrs.nominal=ncol(data)
data[,ncol(data)]<-as.factor(data[,ncol(data)])

Weight <- function(Z,Target)
  {
  F <- matrix()
  
  nc <- ncol(Z)
  for (c in 1:nc) {
    n0 <- 0
    n1 <- 0
    mea1 <- 0
    mea0 <- 0
    #calculate nk
    for (i in 1:length(target)) {
      if (target[i] == 1) {
        mea1 <- mea1 + Z[i,c]
        n1 <- n1 + 1
      } else {
        n0 <- n0 + 1
        mea0 <- mea0 + Z[i,c]
      }
    }
    # if class is 1 then nk=n1 otherwise nk=n0
    fsilon <- mean(Z[,c])
    mea <- (mea0 + mea1)/2
    X0 <- round(mea - mea0,4)
    Y0 <- round(t(X0),4)
    X1 <- round(mea - mea1,4)
    Y1 <- round(t(X1),4)
    Sb <- round(sum((n0*X0*Y0),(n1*X1*Y1)),4)
    eX0 <- round(fsilon - mea0,4)
    eY0 <- round(t(eX0),4)
    eX1 <- round(fsilon - mea1,4)
    eY1 <- round(t(eX1),4)
    St <- round(sum(eX0*eY0,eX1*eY1),4)
    gamma <- 1
    #nr<-nrow(St)
    #nc<-ncol(St)
    #I<-diag(nr )
    #F<-Sb/(St+gamma*I)
    F[c] <- signif(Sb/St,3)
  }
  return(F)
}


remove.NA <- function(x) {
  na.index <- which(is.na(x),arr.ind = TRUE)
  na.row<-nrow(na.index)
  if(length(na.index)){
    for(i in 1:na.row) {
      x[na.index[i,1],na.index[i,2]] <- mean(x[, na.index[i,2]], na.rm = TRUE)
    }
  }
  return(x)
}

normlize.data <- function(x) {
  output <- matrix(data = NA ,
                   nrow = nrow(x),
                   ncol = ncol(x))
  for (j in 1:ncol(x)) {
    for (i in 1:nrow(x)) {
      output[i, j] <-
        (x[i, j] - min(x[, j], na.rm = TRUE)) / (max(x[, j], na.rm = TRUE) -
                                                   min(x[, j], na.rm = TRUE))
    }
  }
  return(output)
}


msTreePrim <- function(nodes, arcs, start.node) {
  arcs <- rbind(arcs, matrix(c(arcs[, 2], arcs[, 1], arcs[,3]), ncol = 3))
  tree.arcs <- matrix(ncol = 3)[-1, ]
  tree.nodes <- nodes[nodes == start.node]
  flag <- TRUE
  while (length(tree.nodes) < length(nodes) & flag) {
    k <- which(arcs[, 1] %in% tree.nodes & arcs[, 2] %in% 
                 nodes[-which(nodes %in% tree.nodes)])
    validArcs <- matrix(arcs[k, ], ncol = 3)
    l <- which(validArcs[, 3] == suppressWarnings(max(validArcs[, 3])))
    max.arc <- matrix(validArcs[l, ], ncol = 3) 
    if (length(max.arc) > 0) {
      tree.arcs <- rbind(tree.arcs, max.arc) 
      tree.nodes <- c(tree.nodes, max.arc[1, 2])
    } else {
      flag <- nrow(max.arc)
    }
  }
  return(tree.arcs)
}


Enqueue <- function(Q, element) {
  Qindex <- length(Q)
  if (Qindex == 0)
  {
    Qindex <- 1
    Q[Qindex] <- element
  }else 
  {
    Qindex <- Qindex + 1
    Q[Qindex] <- element
  }
  return(Q)
}


Dequeue <- function(Q) {
  Qindex <- length(Q)
  if (Qindex == 0)
  {
    Q <- vector()
  }else if (Qindex == 1)
  {
    Qindex <- Qindex - 1
    Q <- vector()
  }else
  {
    Qindex <- Qindex - 1
    Q <- Q[2:length(Q)]
  }
  return(Q)
}


Clustering <- function(Tree) {
  forest <- list()
  Q <- vector()
  x <- 1
  j <- 1
  if(is.null(nrow(Tree))){
    nrow.Tree <- 0 
  } else {
    nrow.Tree <- nrow(Tree)
  }
  while (nrow.Tree > 0) {
    temp1 <- vector()
    node <- Tree[1,1]                               
    temp1[j] <- node
    while (!is.na(node)) {
      index <- vector()
      index <- which(Tree[,1] == node, arr.ind = T)
      #i <- 1
      while (length(index) != 0) {
        if(length(which(Q == Tree[index[1],2])) == 0){
          Q  <- Enqueue(Q,Tree[index[1],2])}    
        Tree[index[1],] <- NA
        index  <-  Dequeue(index)
      }
      node <- Q[1]
      j <- j + 1
      if(length(which(temp1 == node)) == 0){
        temp1[j] <- node}
      Q  <-  Dequeue(Q)                                                
      ##flag <- (length(Q)!=0)
    }
    Tree <- Tree[complete.cases(Tree), ]
    temp1 <- temp1[complete.cases(temp1)]
    forest[[x]] <- temp1 
    x <- x + 1
    if(is.null(nrow(Tree))){
      nrow.Tree <- 0 
    } else {
      nrow.Tree <- nrow(Tree)
    }
  }
  return(forest)
}




fs_rredt <- function(data,is.normalized=TRUE,corr.threshold=0.7)
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





