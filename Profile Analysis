library(stats)
Dat1=read.table("C:\\Users\\DavetteUser\\Desktop\\KUMC Data Science\\STAT 850 Multivariate Statistics\\Week 9\\GuineaPigs-1-3\\GUINEAPIGS.dat")

head(Dat1)

Husb <- as.matrix(Dat1[ which(Dat1$V1=='Control'),2:8])
head(Husb)

Wife <-  as.matrix(Dat1[ which(Dat1$V1=='High'),2:8])


n1=nrow(Husb)
n2=nrow(Wife)
p=ncol(Husb)
n1
n2
p

xbar1=as.matrix(t(colMeans(Husb)))
xbar1
xbar2=as.matrix(t(colMeans(Wife)))
xbar2

S1 = cov(Husb)
S1
S2 = cov(Wife)
S2

SSCP1=as.matrix((n1-1)*S1)

SSCP2=as.matrix((n1-1)*S2)

Sp=as.matrix((SSCP1+SSCP2)/(n1+n2-2))
Sp


C=matrix(c(-1,0,0,1,-1,0,0,1,-1,0,0,1),3,4)
C


C=matrix(c(-1,0,0,0,0,0,
           1,-1,0,0,0,0,
           0,1,-1,0,0,0,
           0,0,1,-1,0,0,
           0,0,0,1,-1,0,
           0,0,0,0,1,-1,
           0,0,0,0,0,1),p-1,p)
C
Cmean=C%*%t(xbar1-xbar2)
Cmean
Ccov=C%*%Sp%*%t(C)
Ccov
#T2 calculation
T2= t(Cmean)%*%solve((1/n1+1/n2)*Ccov)%*%Cmean
T2
Tcon=((n1+n2-2)*(p-1)/(n1+n2-p))*qf(.95,p-1,n1+n2-p) #since tsq is larger, we reject null hypothesis
Tcon



tsq=(sum(xbar1-xbar2)/sqrt((1/n1+1/n2)*sum(Sp)))^2
tcon=qf(.95,1,n1+n2-2) 
c(tsq,tcon) 

xbar=as.matrix((n1/(n1+n2))*xbar1+(n2/(n1+n2))*xbar2)
T2_level= (n1+n2)*(xbar%*%t(C))%*%solve(Ccov)%*%(C%*%t(xbar))
tconlev=((n1+n2-1)*(p-1)/(n1+n2-p+1))*qf(0.95,p-1,n1+n2-p+1)
c(T2_level,tconlev)


varib=seq(1:4)
varib
plot(varib,xbar1, xlab="variable",ylab="sample mean",type="b",bty="n",lwd=2,col="blue",ylim=c(2,8)) 
lines(varib,xbar2,col="red",type="b",lwd=2)



Dat1=read.table("C:\\Users\\DavetteUser\\Desktop\\KUMC Data Science\\STAT 850 Multivariate Statistics\\Week 9\\T6-14.dat")

head(Dat1)

Husb <- as.matrix(Dat1[ which(Dat1$V5=='Husband'),1:4])
head(Husb)

Wife <-  as.matrix(Dat1[ which(Dat1$V5=='Wife'),1:4])

n1=nrow(Husb)
n2=nrow(Wife)
p=ncol(Husb)
n1
n2
p

xbar1=as.matrix(t(colMeans(Husb)))
xbar1
xbar2=as.matrix(t(colMeans(Wife)))
xbar2

S1 = cov(Husb)
S1
S2 = cov(Wife)
S2

SSCP1=as.matrix((n1-1)*S1)

SSCP2=as.matrix((n1-1)*S2)

Sp=as.matrix((SSCP1+SSCP2)/(n1+n2-2))
Sp


C=matrix(c(-1,0,0,1,-1,0,0,1,-1,0,0,1),3,4)
C
Cmean=C%*%t(xbar1-xbar2)
Cmean
Ccov=C%*%Sp%*%t(C)
Ccov
T2= t(Cmean)%*%solve((1/n1+1/n2)*Ccov)%*%Cmean
T2
Tcon=((n1+n2-2)*(p-1)/(n1+n2-p))*qf(.95,p-1,n1+n2-p) #since tsq is larger, we reject null hypothesis
Tcon



tsq=(sum(xbar1-xbar2)/sqrt((1/n1+1/n2)*sum(Sp)))^2
tcon=qf(.95,1,n1+n2-2) 
c(tsq,tcon) 

xbar=as.matrix((n1/(n1+n2))*xbar1+(n2/(n1+n2))*xbar2)
T2_level= (n1+n2)*(xbar%*%t(C))%*%solve(Ccov)%*%(C%*%t(xbar))
tconlev=((n1+n2-1)*(p-1)/(n1+n2-p+1))*qf(0.95,p-1,n1+n2-p+1)
c(T2_level,tconlev)


varib=seq(1:4)
varib
plot(varib,xbar1, xlab="variable",ylab="sample mean",type="b",bty="n",lwd=2,col="blue",ylim=c(2,8)) 
lines(varib,xbar2,col="red",type="b",lwd=2)

