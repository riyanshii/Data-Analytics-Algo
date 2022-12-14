install.packages("ggfortify")
install.packages("ggplot2")
iris <- read.csv("iris.csv",head=TRUE,sep=",");
head(iris,20)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
mydata=select(iris,c(1,2,3,4))
KM=kmeans(mydata,2)
autoplot(KM,mydata,frame=TRUE)
KM$centers