#Analysis of n-of-1 data in Araujo, Julious and Senn
#Set working directory to that containing the data
setwd("C:\\Users\\Stephen\\Documents\\R\\Programs\\R Files\\Research\\n of 1")


#Read data
FEV1.frame<-read.table("Example_1.txt",header=T)

#Define contrasts
options(contrasts=c(factor="contr.treatment", ordered="contr.poly"))


#_______________________________________________________________________________#
#Set and define variables and factors
FEV1<-FEV1.frame$Y
Cycle.F<-as.factor(FEV1.frame$Cycle)
Patient.F<-as.factor(FEV1.frame$Patient)
Period.F<-as.factor(FEV1.frame$Period)
Treat.F<-as.factor(FEV1.frame$Treat)

#_______________________________________________________________________________#
#Split FEV1
FEV1.A<-FEV1[Treat.F=='A']
FEV1.B<-FEV1[Treat.F=='B']
Patient.A<-Patient.F[Treat.F=='A']


#_______________________________________________________________________________#
#Plot data
library(lattice)
xyplot(FEV1.B~FEV1.A|Patient.A,xlab="Treatment A",ylab="Treatment B",pch=19,
   cex=1.5,
   panel=function(x,y,...){
   panel.xyplot(x,y,...)
   panel.abline(a=0,b=1)
   })

#_______________________________________________________________________________#
#Linear model analysis one
fit1<-lm(FEV1~Treat.F+Patient.F/Cycle.F)
#extract terms of interest
estimates1<-coefficients(fit1)
est1.treat<-estimates1[2]
SE1.treat<-sqrt(diag(vcov(fit1)))[2]
t1.treat<-est1.treat/SE1.treat
fit1.frame<-data.frame(est1.treat,SE1.treat,t1.treat)
fit1.frame

#_______________________________________________________________________________#

#Paired t-test analysis
#Calculate differences per cycle
FEV1diff<-FEV1.B-FEV1.A
#Perform one-sample t-test
fit2<-t.test(FEV1diff)
fit2

#_______________________________________________________________________________#
#Linear model anyalsis two
#removing interaction

fit3<-lm(FEV1~Treat.F+Patient.F/Cycle.F+Treat.F:Patient.F)
#extract terms of interest
estimates3<-coefficients(fit3)
est3.treat<-estimates3[2]
SE3.treat<-sqrt(diag(vcov(fit3)))[2]
t3.treat<-est3.treat/SE3.treat
fit3.frame<-data.frame(est3.treat,SE3.treat,t3.treat)
fit3.frame

#_______________________________________________________________________________#
#Mixed models using original data
#load library lme4
library(lme4)
fit4<-lmer(FEV1~Treat.F+Patient.F/Cycle.F+(1|Treat.F:Patient.F))
summary4<-summary(fit4)
summary4$coefficients[2,]

#_______________________________________________________________________________#
#Summary measures approach
#Calculate mean differences B-A per patient using the previously calculated
#differences per cycle
FEV1.Patient<-tapply(FEV1diff,Patient.A,mean)#note use of tapply function
#Perform one-sample t-test
fit5<-t.test(FEV1.Patient)
fit5 #Compare to mixed model

#_______________________________________________________________________________#
#Mixed models using differences
fit6<-lmer(FEV1diff~(1|Patient.A))
summary6<-summary(fit6)
summary6$coefficients[1,] #compare to summary measures and fit4

