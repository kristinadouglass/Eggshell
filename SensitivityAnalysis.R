#Fitting ordinal Logistic regression model. Response: Early, Middle, Late
#Response is discritized: 

#Use two eggs from a day as training set and 1 as testing set

#Read original profileometer data
library("readxl")
#install.packages("ggplot2")
library(ggplot2)
library(MASS) #polr function
require(reshape2)
require(tidyr)
library(corrplot) #cor plot
library(dplyr)

egg1data=read_excel("/Users/priyangi/Box Sync/PSU/EggShell Project/Original Data/Roughness.xlsx",sheet=1)
egg2data=read_excel("/Users/priyangi/Box Sync/PSU/EggShell Project/Original Data/Roughness.xlsx",sheet=2)
egg3data=read_excel("/Users/priyangi/Box Sync/PSU/EggShell Project/Original Data/Roughness.xlsx",sheet=3)
egg1data = as.data.frame(egg1data)
egg2data = as.data.frame(egg2data)
egg3data = as.data.frame(egg3data)

#Cerate a variable named egg to show which dataset(eggidata) it came from 
egg1data$egg=1
egg2data$egg=2
egg3data$egg=3

#Sort the dataset by the day egg was broken(Variable Sample is the day)
alleggdata=rbind(egg1data,egg2data,egg3data)
alleggdata=alleggdata[order(alleggdata$Sample),]
colnames(alleggdata) = c("day","Sk","Spk", "Svk","Sa","Sku","Sq","Ssk","Sz","Sdq","Sdr","egg")

#Discritize: Day 0-21 is Early, Day 22-35 is Middle, Day 36-42 Late
alleggdata$stage=cut(alleggdata$day,breaks = c(0,21,35,42),labels=c("Early","Middle","Late"))
#Discritize: Day 0-21 is Early (1), Day 22-35 is Middle(2), Day 36-42 Late(3)
alleggdata$stageNum=cut(alleggdata$day,breaks = c(0,21,35,42),labels=c(1,2,3))

#Keep data on one egg (egg3 since there is no particular order) from each day as s testing set
testingeggs=alleggdata[alleggdata$egg==3,]
trainingeggs=alleggdata[alleggdata$egg!=3,]


#Ordinal Logistic Regression (Only Sa)
OnePredFit=polr(stageNum ~ Sa, data = trainingeggs, Hess=TRUE)
summary(OnePredFit)

#Calculate P values manually
OnePred_ctable=coef(summary(OnePredFit))
OnePred_pvalue=pnorm(abs(OnePred_ctable[, "t value"]), lower.tail = FALSE) * 2
OnePred_ctable=cbind(OnePred_ctable, "p value" = OnePred_pvalue)

#Check the sensitivity of the developed model.
#(1). Add a ramdom error with a variability specified by certain SNR:0.5,0.5,1,2
#(1). Get the marginal probabilities for each egg
#(2). Predicted day of the egg would be the day that has the highest marginal probability
#(3). Then compare it with the actual hatched day

set.seed(7182)
#A function to add a random noise to data for a specified SNR
#SNR=xbar/sigma_e
#xbar is the average of Sa values for a given day.
#sigma_e is the erros variance defined by xbar/SNR

addnoise=function(SNR,data){
  mean_sa=aggregate(data,list(data$day),mean)
  mean_sa=mean_sa[,c("day","Sa")]
  sigma_e=mean_sa$Sa/SNR
  mean_sa=cbind(mean_sa,sigma_e)
  allerror=NULL
  for(i in 1:42){
    temp=cbind(i,rnorm(3,mean=0,sd=sigma_e[i]))
    allerror=rbind(allerror,temp)
  }
  colnames(allerror)=c("day","randerror")
  noisyalleggdata=cbind(data,allerror)
  noisyalleggdata$noisySa=noisyalleggdata$Sa+noisyalleggdata$randerror
  noisyalleggdata=noisyalleggdata[,c("day","egg","stage","stageNum","noisySa")]
  #Rename noisy Sa colum as Sa
  colnames(noisyalleggdata)=c("day","egg","stage","stageNum","Sa")
  return(noisyalleggdata)
  }

#Function eggPredictStage() gives the predicted stage using the highest marginal probability from the model fit
eggPredictStage=function(day,egg,fitModel,dataset){
  newdata=subset(dataset, day==i&egg==j)
  if (is.na(newdata$Sa)){
    predictStage=NA
  }else{
    pred.prob=predict(fitModel, newdata=newdata, type="probs")
    predictStage=which(pred.prob==max(pred.prob))
  }
  return(predictStage=predictStage)}

#SENSITIVITY ANALYSIS WHEN SNR=0.5
#Comparison of observed and predicted stage when SNR=0.5
Predict.5SNR=array(0,c(42,3,500)) #Repeat 500 times
for (k in 1:500){
  allegg.5SNR=addnoise(SNR=0.5,data=alleggdata) #add noise to Sa
  allegg.5SNR$Sa[allegg.5SNR$Sa<0]=NA #If Sa value is negative, set it to NA
  for (i in 1:42){
    for(j in 1:3){
      predictStage=eggPredictStage(day=i,egg=j,fitModel=OnePredFit,dataset=allegg.5SNR)
      Predict.5SNR[i,j,k]=predictStage
    }
  }
  if(k==1){
    allegg.5SNR_ObsPred=cbind(rep(1:42,each=3),as.vector(t(Predict.5SNR[,,k])))
  }else{
    allegg.5SNR_ObsPred = cbind(allegg.5SNR_ObsPred, as.vector(t(Predict.5SNR[,,k])))
  }
}
colnames(allegg.5SNR_ObsPred)=c("day",paste("iter",1:500,sep=""))
allegg.5SNR_ObsPred=data.frame(allegg.5SNR_ObsPred)
allegg.5SNR_ObsPred$egg=rep(1:3,times=42)
allegg.5SNR_ObsPred$stageNum=cut(allegg.5SNR_ObsPred$day,breaks = c(0,21,35,42),labels=c(1,2,3))


#SENSITIVITY ANALYSIS WHEN SNR=1
#Comparison of observed and predicted stage when SNR=1
Predict1SNR=array(0,c(42,3,500)) #Repeat 500 times
for (k in 1:500){
  allegg1SNR=addnoise(SNR=1,data=alleggdata)#add noise to Sa
  allegg1SNR$Sa[allegg1SNR$Sa<0]=NA #If Sa value is negative, set it to NA
  for (i in 1:42){
    for(j in 1:3){
      predictStage=eggPredictStage(day=i,egg=j,fitModel=OnePredFit,dataset=allegg1SNR)
      Predict1SNR[i,j,k]=predictStage
    }
  }
  if(k==1){
    allegg1SNR_ObsPred=cbind(rep(1:42,each=3),as.vector(t(Predict1SNR[,,k])))
  }else{
    allegg1SNR_ObsPred = cbind(allegg1SNR_ObsPred, as.vector(t(Predict1SNR[,,k])))
  }
}
colnames(allegg1SNR_ObsPred)=c("day",paste("iter",1:500,sep=""))
allegg1SNR_ObsPred=data.frame(allegg1SNR_ObsPred)
allegg1SNR_ObsPred$egg=rep(1:3,times=42)
allegg1SNR_ObsPred$stageNum=cut(allegg1SNR_ObsPred$day,breaks = c(0,21,35,42),labels=c(1,2,3))


#SENSITIVITY ANALYSIS WHEN SNR=2
#Comparison of observed and predicted stage when SNR=2
Predict2SNR=array(0,c(42,3,500)) #Repeat 500 times
for (k in 1:500){
  allegg2SNR=addnoise(SNR=2,data=alleggdata) #add noise to Sa
  allegg2SNR$Sa[allegg2SNR$Sa<0]=NA #If Sa value is negative, set it to NA
  for (i in 1:42){
    for(j in 1:3){
      predictStage=eggPredictStage(day=i,egg=j,fitModel=OnePredFit,dataset=allegg2SNR)
      Predict2SNR[i,j,k]=predictStage
    }
  }
  if(k==1){
    allegg2SNR_ObsPred=cbind(rep(1:42,each=3),as.vector(t(Predict2SNR[,,k])))
  }else{
    allegg2SNR_ObsPred = cbind(allegg2SNR_ObsPred, as.vector(t(Predict2SNR[,,k])))
  }
}
colnames(allegg2SNR_ObsPred)=c("day",paste("iter",1:500,sep=""))
allegg2SNR_ObsPred=data.frame(allegg2SNR_ObsPred)
allegg2SNR_ObsPred$egg=rep(1:3,times=42)
allegg2SNR_ObsPred$stageNum=cut(allegg2SNR_ObsPred$day,breaks = c(0,21,35,42),labels=c(1,2,3))

#SENSITIVITY ANALYSIS WHEN SNR=3
#Comparison of observed and predicted stage when SNR=3
Predict3SNR=array(0,c(42,3,500)) #Repeat 500 times
for (k in 1:500){
  allegg3SNR=addnoise(SNR=3,data=alleggdata) #add noise to Sa
  allegg3SNR$Sa[allegg3SNR$Sa<0]=NA #If Sa value is negative, set it to NA
  for (i in 1:42){
    for(j in 1:3){
      predictStage=eggPredictStage(day=i,egg=j,fitModel=OnePredFit,dataset=allegg3SNR)
      Predict3SNR[i,j,k]=predictStage
    }
  }
  if(k==1){
    allegg3SNR_ObsPred=cbind(rep(1:42,each=3),as.vector(t(Predict3SNR[,,k])))
  }else{
    allegg3SNR_ObsPred = cbind(allegg3SNR_ObsPred, as.vector(t(Predict3SNR[,,k])))
  }
}
colnames(allegg3SNR_ObsPred)=c("day",paste("iter",1:500,sep=""))
allegg3SNR_ObsPred=data.frame(allegg3SNR_ObsPred)
allegg3SNR_ObsPred$egg=rep(1:3,times=42)
allegg3SNR_ObsPred$stageNum=cut(allegg3SNR_ObsPred$day,breaks = c(0,21,35,42),labels=c(1,2,3))


###Success rate by day for each SNR
#SNR=0.5
SNR.5srate=NULL
for (i in 1:3){
  temp_stage=allegg.5SNR_ObsPred[allegg.5SNR_ObsPred$stageNum==i,grep("iter", colnames(allegg.5SNR_ObsPred))] #Get the predictions from 500 trials for a given day
  temp_nonna=sum(is.na(temp_stage)==F) #No.of non-nas
  temp_srate=(length(which(temp_stage==i))/temp_nonna)*100 #Success rate for a gievn stage 
  tempall=c(i,temp_nonna,temp_srate)
  SNR.5srate=rbind(SNR.5srate,tempall)
}
colnames(SNR.5srate)=c("Stage","Feasible","SRate")

#SNR=1
SNR1srate=NULL
for (i in 1:3){
  temp_stage=allegg1SNR_ObsPred[allegg1SNR_ObsPred$stageNum==i,grep("iter", colnames(allegg1SNR_ObsPred))] #Get the predictions from 500 trials for a given day
  temp_nonna=sum(is.na(temp_stage)==F) #No.of non-nas
  temp_srate=(length(which(temp_stage==i))/temp_nonna)*100 #Success rate for a gievn stage 
  tempall=c(i,temp_nonna,temp_srate)
  SNR1srate=rbind(SNR1srate,tempall)
}
colnames(SNR1srate)=c("Stage","Feasible","SRate")


#SNR=2
SNR2srate=NULL
for (i in 1:3){
  temp_stage=allegg2SNR_ObsPred[allegg2SNR_ObsPred$stageNum==i,grep("iter", colnames(allegg2SNR_ObsPred))] #Get the predictions from 500 trials for a given day
  temp_nonna=sum(is.na(temp_stage)==F) #No.of non-nas
  temp_srate=(length(which(temp_stage==i))/temp_nonna)*100 #Success rate for a gievn stage 
  tempall=c(i,temp_nonna,temp_srate)
  SNR2srate=rbind(SNR2srate,tempall)
}
colnames(SNR2srate)=c("Stage","Feasible","SRate")

#SNR=3
SNR3srate=NULL
for (i in 1:3){
  temp_stage=allegg3SNR_ObsPred[allegg3SNR_ObsPred$stageNum==i,grep("iter", colnames(allegg3SNR_ObsPred))] #Get the predictions from 500 trials for a given day
  temp_nonna=sum(is.na(temp_stage)==F) #No.of non-nas
  temp_srate=(length(which(temp_stage==i))/temp_nonna)*100 #Success rate for a gievn stage 
  tempall=c(i,temp_nonna,temp_srate)
  SNR3srate=rbind(SNR3srate,tempall)
}
colnames(SNR3srate)=c("Stage","Feasible","SRate")

plot(SNR.5srate[,3],type="o",lty=1,pch=15, xlab="True Hatched Stage",ylab="Success Rate",ylim=c(0,100))
lines(SNR1srate[,3],type="o",lty=2,pch=16,col=2)
lines(SNR2srate[,3],type="o",lty=3,pch=17,col=3)
lines(SNR3srate[,3],type="o",lty=4,pch=18,col=4)
legend("bottomleft",legend=c("SNR=0.5","SNR=1","SNR=2","SNR=3"),col=c(1,2,3,4),lty=c(1,2,3,4),pch=c(15,16,17,18))










