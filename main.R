## Clear workspace
#rm(list = ls())
## Include library
  # file.exists("C:/Users/Kiranmayie/Desktop/Data mining project/.Rprofile")

source('C:/Users/Kiranmayie/Desktop/Data mining project/SourceFunctions.R',encoding= "UTF-8")
library(SDMTools)
library(caret)
library(kernlab)
library(Biocomb)
library(forecast)
library(heuristica)
source('C:/Users/Kiranmayie/Desktop/Data mining project/LoadData.R',encoding = "UTF-8")
source('C:/Users/Kiranmayie/Desktop/Data mining project/fss_Algorithms.R',encoding= "UTF-8")

Dataset<-LoadData("D1")

D1.CFS.result<-fss_Algorithms(Dataset,Fss_Alg="CFS")
D1.Relief.result<-fss_Algorithms(Dataset,Fss_Alg="ReliefF")
D1.Fast.result<-fss_Algorithms(Dataset,Fss_Alg="FAST",corr.threshold=0.6)

Dataset<-LoadData("D2")

D2.CFS.result<-fss_Algorithms(Dataset,Fss_Alg="CFS")
D2.Relief.result<-fss_Algorithms(Dataset,Fss_Alg="ReliefF")
D2.Fast.result<-fss_Algorithms(Dataset,Fss_Alg="FAST",corr.threshold=0.9)


