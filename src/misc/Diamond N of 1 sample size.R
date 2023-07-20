#PURPOSE
#Sample size calculation for n of 1 trials
#**********************************************************************************************

#FUNDING#
#This work was partly supported by  the European Union’s 7th Framework Programme 
#for research, technological development and demonstration under grant agreement no. 602552, IDEAL
#**********************************************************************************************

#AUTHOR
#Stephen Senn
#**********************************************************************************************

#EXPLANATION


#The purpose of the program is to provide sample size calculation for trials with two treatments
#(say A and B) randomised in k cycles. In each cycle it is assumed that each patient will
#receive one treatment with A and one with B in random order.

#Outcome measures are asumed to be (approximately) Normally distributed

#Two fundamental tasks are addressed. 
#I) Testing the overall effect of treatment across patients
#II) Providing estimates of the treatment effect in a given patient

#For the first task, two basic types of calculation are provided.
#1) Fixed effects calculation (calculation I.1)
#2) Random effects calculation (calculation I.2)

#For both these cases the sample size is rounded up to the nearest integer
#providing the requested power. The associated power for this sample size
#is also provided

#For the second task, two approaches are covered.
#1) Naive calculation using just the results for that patient (calculation II.1)
#2) Shrunk estimation using a combination of inidividual results for (calculation II.2)
#that patient and overal results

#For the second task and the second approach (II.2) it is assumed that the various components of variance
#and the overall mean have been estimated with high precision and that uncertainy as regards their
#values is unimportant

#For these four cases, solutions are provided for various values of k

#The user must put in the maximum number of cycles for which power and sample size will be determined
#This and other parameters must be set in the section headed INPUT

#A two-sided test is assumed throughout
#**********************************************************************************************

#INPUT
#Input parameter values

setwd("C://Users//Stephen//Documents//R/Programs//R Files//Research//n of 1//") #set folder to store graphs
getwd()

CycleMax<-15 #Maximum number of cycles per patient
Alpha<-0.05  #Type I error rate
Beta<-0.2 #Type II error rate'
Delta<-1 #Clinically relevant difference
Psi<-1 #SD of treatment by patient interaction
Sigma<-2 #Within cycle SD
Sides<-2 #Sides for test
MinN<-20 #Minimum number of patients for SE of ratio of weights
MaxN<-100 #Maximum number of patients for SE of ratio of weights
kExamp<-3 #Number of yccles for calculating ratio of variances

Parameters<-data.frame(CycleMax,Alpha,Beta,Delta,Psi,Sigma, MinN,MaxN,kExamp)#Dataframe of parameters
Parameters

#**********************************************************************************************
#CALCULATION
#Begin calculation

#Calculate target power
Power<-100*(1-Beta)
#Assign vector to hold list of possible numbers of cycles
k<-c(2:CycleMax)
#Calculate fixed effects variance vector
V.fixed<-(2*(Sigma**2)/k)
#Calculate random effects variance vector
V.random<-((Psi**2)+2*(Sigma**2)/k)

V.fr<-data.frame(k,V.fixed,V.random)
#Initialise sample sizes for fixed and random effects 
n.fixed<-2:CycleMax #Fixed effects
n.random<-2:CycleMax #Random effects


#Loop to change the number of cycles, k, and calculate the number of patients
#required for each value

#set approximate non-centrality target using alpha and beta
Z.target<-qnorm(Alpha/Sides, lower.tail = FALSE)+qnorm(Beta, lower.tail = FALSE)
#calculate approximate sample size by setting it just below what would apply for
#Normal distribution
n.fix.temp<-floor((Z.target**2)*(V.fixed)/(Delta**2))
#initialise values for power.fixed and power.random
power.fixed=rep(Power,CycleMax-1)
power.random<-power.fixed


#fixed effects
#begin for loop for number of cycles
for (i in 1:(CycleMax-1)){ 
                      n.fixed[i]<-n.fix.temp[i]
                      DF<-n.fixed[i]*(k[i]-1)
                      NCP=Delta/(2*(Sigma**2)/(k[i]*n.fixed[i]))**0.5
                      t.crit<-qt(Alpha/Sides,df=DF,lower.tail=FALSE)
                      power.temp<-pt(t.crit,df=DF,ncp=NCP,lower.tail=FALSE)

                      #increase sample size until target power is reached
                      #begin while loop  
                      while (power.temp<(Power/100)) {
                         n.fixed[i]<-n.fixed[i]+1
                         DF<-n.fixed[i]*(k[i]-1)
                         NCP=Delta/(2*(Sigma**2)/(k[i]*n.fixed[i]))**0.5
                         t.crit<-qt(Alpha/Sides,df=DF,lower.tail=FALSE)
                         power.temp<-pt(t.crit,df=DF,ncp=NCP,lower.tail=FALSE)
                         power.fixed[i]<-power.temp*100
                      } #end of while loop
                                            
                      }#end of for loop

#random effects
#begin for loop for number of cycles
for (i in 1:(CycleMax-1)){ 
                      #calculate sample size
                      dummy.r<-power.t.test(type="one.sample",sig.level=Alpha, 
                      alternative="two.sided", power=Power/100, 
                      sd=V.random[i]**0.5,delta=Delta,n=NULL)
                      n.random[i]<-ceiling(dummy.r$n)
                      #calculate power
                      dummy.power<-power.t.test(type="one.sample",sig.level=Alpha, 
                      alternative="two.sided", power=NULL, 
                      sd=V.random[i]**0.5,delta=Delta,n=n.random[i])
                      power.random[i]<-dummy.power$power
                      }


TotalCycles.f<-k*n.fixed
TotalCycles.r<-k*n.random

samplesize.f<-data.frame(k,V.fixed,n.fixed,TotalCycles.f,power.fixed) #dataframe for fixed effects
samplesize.r<-data.frame(k,V.random,n.random,TotalCycles.r,power.random)#dataframe for random effects



#Now calculate variance of naive and shrunk estimates

Naive<-(2*(Sigma**2)/k)**0.5 #Task II.1
Shrunk<-((2*(Sigma**2)*(Psi**2))/(k*(Psi**2)+2*(Sigma**2)))**0.5 #Task II.2
Estimates.SE<-data.frame(k,Naive,Shrunk)

#Calculate standard errors of ratios of weights
n.r<-c(MinN:MaxN) #Sample sizes
k.E<-kExamp
term1<-(((k.E*(Psi**2)/(2*(Sigma**2)))+1)**2)
term2<-(2*(n.r*(k.E-1))**2)*(n.r*k.E-3)
term3<-((n.r-1)*((n.r*(k.E-1)-2)**2)*(n.r*(k.E-1)-4))
var.ratio<-term1*term2/term3
SE.ratio<-var.ratio**0.5
ratio.frame<-data.frame(n.r,var.ratio,SE.ratio)


#**********************************************************************************************
#OUTPUT
#fixed effects results
samplesize.f
#random effects results
samplesize.r

#Standard errors of individual predictions for patients
Estimates.SE
#Standard errors of ratio of weights
ratio.frame
#**********************************************************************************************
#PLOTS
pdf('plots.pdf')#assigns all plots to a common pdf file
#calculate xaxis limits for most plots
lower.x<-0
upper.x<-CycleMax+1


#----------------------------------------------------------------------------------------------

#plot number of patients
#calculate yaxis limits for this plot
upper.y<-n.random[1]+5
lower.y<-n.fixed[CycleMax-1]-5
#construct text to label graph with targetted power
power.label<-paste(c('Target power is ',Power,'%'),collapse = " ")


plot(n.random~k,type='b',ylab='Number of Patients',xlab='Number of cycles',
main='Number of patients as a function of number of cycles',pch=1,ylim=c(lower.y,upper.y),
xlim=c(lower.x,upper.x),col=2)
lines(n.fixed~k,col=4)
points(n.fixed~k,pch=5,col=4)
text(upper.x/2,lower.y+(upper.y-lower.y)/2,power.label)
legend(CycleMax-4,upper.y-5,c('random effects','fixed effects'),col=c(2,4),pch=c(1,5),lty=c(1,1))

#----------------------------------------------------------------------------------------------
#plot number of cycles
#calculate yaxis limits for this plot
upper.y<-TotalCycles.r[CycleMax-1]+10
lower.y<-TotalCycles.f[1]-10

#plot
plot(TotalCycles.r~k,type='b',ylab='Total Number of Cycles',xlab='Number of cycles per patient',
main='Totals as a function of number of cycles per patient',pch=1,ylim=c(lower.y,upper.y),
xlim=c(lower.x,upper.x),col=2)
lines(TotalCycles.f~k,col=4)
points(TotalCycles.f~k,pch=5,col=4)
text(upper.x/2,lower.y+2*(upper.y-lower.y)/3,power.label)
legend(1,upper.y-5,c('random effects','fixed effects'),col=c(2,4),pch=c(1,5),lty=c(1,1))



#----------------------------------------------------------------------------------------------
#plot standard errors for predictions for individual patients
#calculate yaxis limits for this plot
upper.y<-Naive[1]*1.1
lower.y<-Shrunk[CycleMax-1]*0.9
#calculate y values for reference line
y.ref<-c(Psi,Psi)
#calculate x values for reference line
x.ref<-c(lower.x,upper.x)

#plot
plot(Naive~k,type='b',ylab='Standard errors',xlab='Number of cycles per patient',
main='Standard errors as a function of number of cycles per patient',pch=1,
ylim=c(lower.y,upper.y),xlim=c(lower.x,upper.x),col=2)
lines(Shrunk~k,col=4)
points(Shrunk~k,pch=5,col=4)
lines (y.ref~x.ref,col=1,lty=2)
text(2*upper.x/3,1.1*Psi,'SE using values from other patients only')
legend(CycleMax-5,upper.y*0.9,c('Shrunk','Naive'),col=c(2,4),pch=c(1,5),lty=c(1,1))


#----------------------------------------------------------------------------------------------
#plot standard errors of ratios of weights
#calculate yaxis limits for this plot
upper.y<-SE.ratio[1]*1.1
lower.y<-0
#calculate y values for reference line
w.r<-(Psi**2)*k.E/(2*(Sigma**2)) #Ratio of weights
y.ref<-c(w.r,w.r)
#calculate x values for reference line
x.ref<-c(MinN,MaxN)

#construct text to label graph



#plot
plot(SE.ratio~n.r,type='b',ylab='Standard errors',xlab='Number patients',
main='Standard errors for ratios of weights',pch=1,
ylim=c(lower.y,upper.y),xlim=c(MinN,MaxN),col=2)
lines (y.ref~x.ref,col=1,lty=2)
text ((MinN+MaxN)/2,1.05*y.ref[1],'Ratio of weights')

dev.off()

