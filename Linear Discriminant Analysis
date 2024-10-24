library(MASS)
library(matlib)
library(tidyverse)
library(klaR)
library(psych)
library(ggord)
library(devtools)

x1 = as.matrix()
x2 = as.matrix(c)
s = matrix(c, ncol = 2)


xbar1=c(3,6)
xbar2=c(5,8)
Spinv=matrix(c(1,1,1,2),2,2)
colnames(Spinv)=c("Log10(activity)", "Log10(antigen)")
#the equal cost equal prior rule has coefficients
a=(xbar1-xbar2)%*%Spinv
a
ybar1=a%*%xbar1
ybar2=a%*%xbar2
m=(ybar1+ybar2)/2
m

ybar1
ybar2

x0=c(2,7)
a%*%x0
c(m,a%*%x0)

mower = read.csv("C:\\Users\\DavetteUser\\Desktop\\KUMC Data Science\\STAT 850 Multivariate Statistics\\Week 11\\Chapter11_datafiles\\table11_1.csv", header =TRUE)
mower

mower$Ownership = factor(mower$Ownership)

lda(Ownership ~ ., data=mower)
partimat(Ownership~., data = mower, method="lda")
