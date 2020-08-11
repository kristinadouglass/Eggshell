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


#Plot the data
#Make side-by-side boxplots of Sa for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Sa))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Skfor Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Sk))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Spk for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Spk))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Spk for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Sku))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Spk for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Sq))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Spk for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Ssk))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Spk for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Sz))+geom_boxplot(size = .75)

#Make side-by-side boxplots of Spk for Early, Middle and Late stages
ggplot(alleggdata, aes(x = stage, y = Sdr))+geom_boxplot(size = .75)

#Get the correlation matrix 
alleggdata.tmp = alleggdata[,-c(12:14)] #Only keep the 10 scanner measurments
alleggdata.tmp=aggregate(.~day, data=alleggdata.tmp, mean) #Avg of 3 eggs 
corrplot(cor(alleggdata.tmp[,-1]),type="upper")
corrplot(cor(alleggdata.tmp[,-1]),type="upper",method="number")

#Measurements (from 10) availabe in Kristina's scanner: Sa,Sz,Sq,Ssk,Sku
#cor(Sa, Sz)=0.8
#cor(Sa, Sq)=1
#cor(Sa,Ssk)=0.87
#cor(Sa,Sku)=-0.85

#cor(Sz,Sq)=0.82
#cor(Sz,Ssk)=0.65
#cor(Sz,Sku)=-0.54

#cor(Sq,Ssk)=0.86
#cor(Sq,Sku)=-0.85

#cor(Ssk,Sku)=-0.9

#Ordinal Logistic Regression with four predictors (with Sa and other)
FourPredFit1=polr(stageNum ~Sa+Sku+Ssk+Sz, data = trainingeggs, Hess=TRUE)
summary(FourPredFit1)

#Calculate P values manually (FourPredFit1)
FourPred_ctable1=coef(summary(FourPredFit1))
FourPred_pvalue1=pnorm(abs(FourPred_ctable1[, "t value"]), lower.tail = FALSE) * 2
FourPred_ctable1=cbind(FourPred_ctable1, "p value" = FourPred_pvalue1)

#FourPredFit1:Sa+Sku+Ssk+Sz
#Obtain predicted probabilities for the eggs in testing set (FourPredFit1)
pred.ProbFour1=data.frame(predict(FourPredFit1, testingeggs, type = "probs"))
names(pred.ProbFour1)=c("Early","Middle","Late")

#Plot the predicted probabilities
pred.ProbFour1_2=data.frame(Probability = as.vector(t(pred.ProbFour1)), Stage = rep(c("Early","Middle","Late"),42), Day = rep(1:42,each=3))
ggplot(data=pred.ProbFour1_2,aes(x=Day, y=Probability, group=Stage, color=Stage)) +
  geom_line()+geom_vline(xintercept = c(21,35), linetype="dashed")

#Compare Observed and predicted stage from FourPredFit1:Sa+Sku+Ssk+Sz
FourFit1_ObsPred=data.frame(cbind(PredictedStage=apply(pred.ProbFour1,1,which.max),TrueStage=testingeggs$stage))
table(FourFit1_ObsPred)


#Ordinal Logistic Regression with four predictors (with Sq and other)
FourPredFit2=polr(stageNum ~Sq+Sku+Ssk+Sz, data = trainingeggs, Hess=TRUE)
summary(FourPredFit2)

#Calculate P values manually (FourPredFit2)
FourPred_ctable2=coef(summary(FourPredFit2))
FourPred_pvalue2=pnorm(abs(FourPred_ctable2[, "t value"]), lower.tail = FALSE) * 2
FourPred_ctable2=cbind(FourPred_ctable2, "p value" = FourPred_pvalue2)

#FourPredFit2:Sq+Sku+Ssk+Sz
#Obtain predicted probabilities for the eggs in testing set (FourPredFit1)
pred.ProbFour2=data.frame(predict(FourPredFit2, testingeggs, type = "probs"))
names(pred.ProbFour2)=c("Early","Middle","Late")

#Plot the predicted probabilities
pred.ProbFour2_2=data.frame(Probability = as.vector(t(pred.ProbFour2)), Stage = rep(c("Early","Middle","Late"),42), Day = rep(1:42,each=3))
ggplot(data=pred.ProbFour2_2,aes(x=Day, y=Probability, group=Stage, color=Stage)) +
  geom_line()+geom_vline(xintercept = c(21,35), linetype="dashed")

#Compare Observed and predicted stage from FourPredFit1:Sa+Sku+Ssk+Sz
FourFit2_ObsPred=data.frame(cbind(PredictedStage=apply(pred.ProbFour2,1,which.max),TrueStage=testingeggs$stage))
table(FourFit2_ObsPred)


#Ordinal Logistic Regression (Only Sa)
OnePredFit=polr(stageNum ~ Sa, data = trainingeggs, Hess=TRUE)
summary(OnePredFit)

#Calculate P values manually
OnePred_ctable=coef(summary(OnePredFit))
OnePred_pvalue=pnorm(abs(OnePred_ctable[, "t value"]), lower.tail = FALSE) * 2
OnePred_ctable=cbind(OnePred_ctable, "p value" = OnePred_pvalue)

#Obtain predicted probabilities for the eggs in testing set
pred.Prob=data.frame(predict(OnePredFit, testingeggs, type = "probs"))
names(pred.Prob)=c("Early","Middle","Late")
#Plot the predicted probabilities
pred.Prob2=data.frame(Probability = as.vector(t(pred.Prob)), Stage = rep(c("Early","Middle","Late"),42), Day = rep(1:42,each=3))
ggplot(data=pred.Prob2,aes(x=Day, y=Probability, group=Stage, color=Stage)) +
  geom_line()+geom_vline(xintercept = c(21,35), linetype="dashed")

#Compare Observed and predicted stage from OnePredFit:Sa
OneFit_ObsPred=data.frame(cbind(PredictedStage=apply(pred.Prob,1,which.max),TrueStage=testingeggs$stage))
table(OneFit_ObsPred)

#In order to compare the outcome from 4 predictor model (Sa+Sku+Ssk+Sz) and one predictor model (Sa) 
FourFit1_ObsPred_temp=FourFit1_ObsPred
FourFit1_ObsPred_temp$PredPeriod=recode(FourFit1_ObsPred_temp$PredictedStage, '1' = "Early", '2' = "Middle", '3'="Late")
FourFit1_ObsPred_temp$TruePeriod=recode(FourFit1_ObsPred_temp$TrueStage, '1' = "Early", '2' = "Middle", '3'="Late")
write.csv(FourFit1_ObsPred_temp,"/Users/priyangi/Box Sync/PSU/EggShell Project/DiscreteTimeToEvent/NewAnalysis/OrdinalLogistic/FourFit1_ObsPred_temp.csv")
OneFit_ObsPred_temp=OneFit_ObsPred
OneFit_ObsPred_temp$PredPeriod=recode(OneFit_ObsPred_temp$PredictedStage, '1' = "Early", '2' = "Middle", '3'="Late")
write.csv(OneFit_ObsPred_temp,"/Users/priyangi/Box Sync/PSU/EggShell Project/DiscreteTimeToEvent/NewAnalysis/OrdinalLogistic/OneFit_ObsPred_temp.csv")


#Prediction for other species
otherspecies=data.frame(Sa=c(8.91,3.34,14.65,9.06,5.99))
other.pred.Prob=(predict(OnePredFit,otherspecies , type = "probs"))
other.pred.Prob=data.frame(other.pred.Prob,SampID=c("E1-1-(chicken)","E1-12-(chicken)","EUM-B04-0-034(emu)","EUM-0-20-50-Hatched(emu)","EUM-P10-45 Unhatched(emu)"))
names(other.pred.Prob)=c("Early","Middle","Late","SampID")
other.pred.Prob.long=gather(other.pred.Prob, Stage, Probability, Early:Late, factor_key=TRUE)
ggplot(other.pred.Prob.long, aes(fill=Stage, y=Probability, x=SampID),color=Stage) + 
  geom_bar(position="dodge", stat="identity")






