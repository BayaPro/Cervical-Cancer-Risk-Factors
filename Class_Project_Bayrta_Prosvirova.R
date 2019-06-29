install.packages("tidyverse")
install.packages("readxl")
library(tidyverse)
library(readxl)

View(risk_factors_cervical_cancer)
summary(risk_factors_cervical_cancer)
mydata<-data.frame(risk_factors_cervical_cancer$Age, 
                   as.numeric(risk_factors_cervical_cancer$`Number of sexual partners`), 
                   as.numeric(risk_factors_cervical_cancer$`Num of pregnancies`), 
                   as.numeric(risk_factors_cervical_cancer$`Hormonal Contraceptives (years)`), 
                   as.numeric(risk_factors_cervical_cancer$`STDs (number)`), 
                   risk_factors_cervical_cancer$New_Dx, 
                   risk_factors_cervical_cancer$`Dx:Cancer`,
                   as.factor(risk_factors_cervical_cancer$`Dx:HPV`),
                   as.factor(risk_factors_cervical_cancer$`Dx:CIN`))
mydata<-na_if(mydata, '?')

names(mydata)[1]<-"age"
names(mydata)[2]<-"N_Sexual_partners"
names(mydata)[3]<-"N_Pregnant"
names(mydata)[4]<-"Yrs_hormonal_contraception"
names(mydata)[5]<-"N_STDs"
names(mydata)[6]<-"Diagnostic_Gd"
names(mydata)[7]<-"Previous_Dx_Cancer"
names(mydata)[8]<-"HPV"
names(mydata)[9]<-"CIN"
mydata

hist(mydata$age)
hist(mydata$N_Sexual_partners)
hist(mydata$N_Pregnant)
hist(mydata$Yrs_hormonal_contraception)
hist(mydata$N_STDs)




median(mydata$N_Sexual_partners, na.rm = TRUE)
#2
mydata$N_Sexual_partners[which(is.na(mydata$N_Sexual_partners))]<-median(mydata$N_Sexual_partners, na.rm = TRUE)
mydata

mydata$N_Pregnant[which(is.na(mydata$N_Pregnant))]<-median(mydata$N_Pregnant, na.rm = TRUE)
median(mydata$N_Pregnant, na.rm = TRUE)
mydata

median(mydata$Yrs_hormonal_contraception, na.rm = TRUE)
#0.5
mydata$Yrs_hormonal_contraception[which(is.na(mydata$Yrs_hormonal_contraception))]<-median(mydata$Yrs_hormonal_contraception, na.rm = TRUE)
mydata

median(mydata$N_STDs, na.rm = TRUE)
#0
mydata$N_STDs[which(is.na(mydata$N_STDs))]<-median(mydata$N_STDs, na.rm = TRUE)
mydata


attach(mydata)
mydata$agecat[age<20]<- "<20"
mydata$agecat[age>=20 & age<35]<- "20-35"
mydata$agecat[age>=35 & age<45]<-"35-45"
mydata$agecat[age>=45 & age<60]<-"45-55"
mydata$agecat[age>=60 & age<70]<-"60-70"
mydata$agecat[age>=70]<-"70+"

library(dplyr)
mydata<-mutate(mydata, CancerPosNeg=as.factor(ifelse(Diagnostic_Gd>2, 
                                      "CancerPositive", "CancerNegative")))


mydata$agecat<-as.factor(mydata$agecat)
mydata

mydata<-select(mydata, -c(Diagnostic_Gd))
mydata



boxplot(age ~ CancerPosNeg, ylab="Age", xlab="Cancer",  data=mydata)
boxplot(N_Sexual_partners ~ CancerPosNeg, ylab="Number of Sexual Partners", 
        xlab="Cancer",  data=mydata)
boxplot(N_Pregnant ~ CancerPosNeg, ylab="Number of pregnancies", 
        xlab="Cancer",  data=mydata)
boxplot(Yrs_hormonal_contraception ~ CancerPosNeg, 
        ylab="Years of hormonal contraception", xlab="Cancer",  data=mydata)
boxplot(N_STDs ~ CancerPosNeg, ylab="Number of STDs", xlab="Cancer",  data=mydata)
spineplot(mydata$agecat ~ mydata$CancerPosNeg)


install.packages("randomForest")
library(randomForest)

n <- nrow(mydata)  
ntrain <- round(n*0.8)  
set.seed(100)    
tindex <- sample(n, ntrain)  
train_mydata <- mydata[tindex,] 
test_mydata <- mydata[-tindex,] 


library(randomForest)
rf <- randomForest(CancerPosNeg~ ., data=train_mydata, ntree=500, 
                   mtry=2, importance=TRUE)
rf

prediction <- predict(rf, newdata=test_mydata, type="class")

table(prediction, test_mydata$CancerPosNeg)

misclassification_error_rate <- sum(test_mydata$CancerPosNeg != prediction) / 
  nrow(test_mydata)*100
misclassification_error_rate 

varImpPlot(rf)




