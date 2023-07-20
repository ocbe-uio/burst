#PURPOSE
#Illustrate analyses of n-of-1 trials as covered by
#Araujo A, Julious S, Senn S. Understanding Variation in Sets of N-of-1 Trials. 
#PloS one. 2016;11(12):e0167167.

#**********************************************************************************************

#FUNDING#
#This work was partly supported by  the European Union’s 7th Framework Programme 
#for research, technological development and demonstration under grant agreement no. 602552, IDEAL
#**********************************************************************************************

#AUTHOR
#Stephen Senn
#**********************************************************************************************

#EXPLANATION

#Details are given in the paper by Araujo, Julious and Senn



#This version can also create an ubalanced data set


#Set working directory to that containing the data
#setwd("C:\\Users\\Stephen\\Documents\\R\\Programs\\R Files\\Research\\n of 1")
setwd("C:\\Users\\steph\\OneDrive\\Documents\\Local Files\\Documents\\R\\Programs\\R Files\\Research\\n of 1")
#Remove the hash sign at the beginning of this if you wish to save figures
#pdf('analysis_plots.pdf')#assigns all plots to a common pdf file

#Read data
FEV1.frame<-read.table("Example_1.txt",header=T)

#Define contrasts
options(contrasts=c(factor="contr.treatment", ordered="contr.poly"))

FEV1.frame

#_______________________________________________________________________________#
#Remove missing rows if so desired
# NB the first of these statements will remove missing rows. The second won't. 
#Choose one by removing/adding hash sign
Reduced<-subset(FEV1.frame, Missing<1)
#Reduced<-subset(FEV1.frame, Missing<2)



#Set and define variables and factors
FEV1<-Reduced$Y
Cycle.F<-as.factor(Reduced$Cycle)
Patient.F<-as.factor(Reduced$Patient)
Period.F<-as.factor(Reduced$Period)
Treat.F<-as.factor(Reduced$Treat)




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
anova(fit1)
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
#Linear model analysis two
#removing interaction

fit3<-lm(FEV1~Treat.F+Patient.F/Cycle.F+Treat.F:Patient.F)
anova(fit3)
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
anova(fit4)
summary4$coefficients[2,]

#_______________________________________________________________________________#
#Calculate some simple statistics for use in subsequent analyses
#Calculate mean differences B-A per patient using the previously calculated
#differences per cycle
FEV1.Patient<-tapply(FEV1diff,Patient.A,mean)#note use of tapply function
#Calculate standard errors per patient to use for further calculations
Var.Patient<-tapply(FEV1diff,Patient.A,var)#note use of tapply function
n.Patient<-tapply(FEV1diff,Patient.A,length)#note use of tapply function
Var.Patient[is.na(Var.Patient)] <- 0 #replacing NA by 0
Pooled<-sum(Var.Patient*(n.Patient-1))/sum(n.Patient-1)#Pooled per cycle variance estimate
SE.Patient<-sqrt(Pooled/n.Patient)#Standard error per patients
Patient<-(subset(Reduced, (Cycle.F==1)&(Treat.F=='A')))$Patient
Meta.frame<-data.frame(Patient,FEV1.Patient,SE.Patient)#Data frame for meta-analysis
Meta.frame
#_______________________________________________________________________________#

#Summary measures approach
#Perform one-sample t-test
fit5<-t.test(FEV1.Patient)
fit5 #Compare to mixed model

#_______________________________________________________________________________#
#Mixed models using differences
fit6<-lmer(FEV1diff~(1|Patient.A))
summary6<-summary(fit6)
summary6$coefficients[1,] #compare to summary measures and fit4
#_______________________________________________________________________________#

#Meta-analysis of n of 1 trials
#remember to load meta, metafor and rmeta
library(meta)
library(metafor)
library(rmeta)


#Set meta-analysis method for metafor
MAMethod<-"REML" #REML
#MAMethod<-"DL"  #DerSimoniamn and Laird
#MAMethod<-"ML" #Maximum likelihood
#MAMethod<-"EB"  #Empirical Bayes

#Coerce estimates and SEs to have appropriate form
Estimates2<-as.vector(FEV1.Patient)
SE2<-as.vector(SE.Patient)
#Round values to two decimal places
SE2<-floor(SE2*100+0.5)/100

#Carry out meta-analysis using the meta package
Results_u1<-metagen(TE=Estimates2,seTE=SE2,studlab=Patient)
Results_u1
forest(Results_u1)


#Carry out meta-analysis using the metafor package
Results_u2<-rma(yi=Estimates2,sei=SE2,method=MAMethod)
Results_u2



#Calculate shrunk estimates and standard errors
ShrunkEst2<-round(blup(Results_u2)$pred,digits=1)
ShrunkSE2<-round(blup(Results_u2)$se,digits=1)

Shrunk.frame<-data.frame(Meta.frame,ShrunkEst2,ShrunkSE2)
Shrunk.frame

#Plot results
lower<-0.9*min(Estimates2)
upper<-1.1*max(Estimates2)
par(mfrow = c(1, 1),pty="s")#make dimensions of plot equal
plot(y=ShrunkEst2,x=Estimates2,pch=19,xlab='Naive estimate',
ylab='Shrunk estimate',xlim=c(lower,upper),ylim=c(lower,upper),
col='blue')
abline(a=0,b=1,col='red')
lines(x=rep(Results_u2$beta,2),y=c(lower,upper),lty='dashed')
lines(y=rep(Results_u2$beta,2),x=c(lower,upper),lty='dashed')
#annotate plot
title(main="Shrunk versus naive estimates for the unbalanced case")
text(Estimates2[11],ShrunkEst2[11]+10,labels="11")
text(Estimates2[12],ShrunkEst2[12]-10,labels="12")
#Carry out meta-analysis using the rmeta package
Results_u3<-meta.summaries(d=Estimates2,se=SE2,names=Patient,method="random")
summary(Results_u3)



