LoadData <- function(dataset=c("D1","D2")){
  
  if(dataset == "D1")
    {
    Dataset <- read.csv("C:/Users/Kiranmayie/Desktop/Data mining project/Data1-Diabetes.csv",row.names = NULL)
  }
  else if(dataset == "D2")
    {
    Dataset <- read.csv("C:/Users/Kiranmayie/Desktop/Data mining project/Data2-Diabetes.csv",row.names = NULL)  
  }

  return(Dataset)
}