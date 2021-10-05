#Credit Risk Data

#Target Variable: Good Credit
data <- read.csv(choose.files())

head(data)

#Checking for Categorical and Continuous columns
length(unique(data$checkingstatus)) #4 category
length(unique(data$duration)) #Continuous
length(unique(data$history)) #5 category
length(unique(data$purpose)) #10 category
length(unique(data$amount)) #Continuous
length(unique(data$savings)) #5 category
length(unique(data$employ)) #5 category
length(unique(data$installment)) #4 category
length(unique(data$status)) #4 category
length(unique(data$others)) #3 category
length(unique(data$residence)) #4 category
length(unique(data$property)) #4 category
length(unique(data$age)) #continuous
length(unique(data$otherplans)) #3 category
length(unique(data$housing)) #3 category
length(unique(data$cards)) #4 category
length(unique(data$job)) #4 category
length(unique(data$liable)) #2 category
length(unique(data$tele)) #2 category
length(unique(data$foreign)) #2 category

###########################################################################################

#Treatment of Missing Values

colSums(is.na(data))
#There is no missing data.

###########################################################################################

#Treating Outliers


boxplot(data$duration) #positive outliers found
summary(data$duration)
Q1<-12
Q3<-24
IQR<-Q3-Q1
pos_outlier <- Q3+1.5*IQR
pos_outlier
data$duration <- ifelse(data$duration>pos_outlier,41,data$duration)


boxplot(data$amount) #positive outliers found
summary(data$amount)
Q1 <- 1366
Q3 <- 3972
IQR <- Q3-Q1
IQR
pos_outlier <- Q3+1.5*IQR
pos_outlier
data$amount <- ifelse(data$amount>pos_outlier,7880,data$amount)


boxplot(data$age) #positive ouliers found
summary(data$age)
Q1 <- 27
Q3 <- 42
IQR <- Q3-Q1
pos_outlier <- Q3+1.5*IQR
pos_outlier
data$age <- ifelse(data$age>pos_outlier,64,data$age)

#################################################################################################


#Feature Selection
#Categorical vs Continuous -- Box Plot
#ANOVA Test
install.packages("dplyr")

# H0 = mu = mu01 = mu02(There is no difference
# between average displacement for different gear)
# H1 = Not all means are equal

boxplot(data$age~factor(data$GoodCredit),xlab = "GoodCredit", ylab = "age") 

age_aov<- aov(data$age~factor(data$GoodCredit))
summary(age_aov)

#p-value = 0.0036 < 0.05
#age is significant


boxplot(data$amount~factor(data$GoodCredit),xlab = "GoodCredit",ylab = "amount")
amount_aov <- aov(data$amount~factor(data$GoodCredit))
summary(amount_aov)
#amount is significant


boxplot(data$duration~factor(data$GoodCredit),xlab = "GoodCredit",ylab = "duration")
duration_aov <- aov(data$duration~factor(data$GoodCredit))
summary(duration_aov)
#duration is Significant


#Relationship exploration: Categorical Vs Categorical -- Chi Sq Test
#Assumption(H0): The two columns are NOT related to each other

chisq.test(data$GoodCredit,data$checkingstatus) #significant
chisq.test(data$GoodCredit,data$history) #significant
chisq.test(data$GoodCredit,data$installment) #not significant
chisq.test(data$GoodCredit,data$purpose) #significant
chisq.test(data$GoodCredit,data$savings) #significant
chisq.test(data$GoodCredit,data$employ) #significant
chisq.test(data$GoodCredit,data$status) #significant
chisq.test(data$GoodCredit,data$others) #significant
chisq.test(data$GoodCredit,data$residence) #not significant
chisq.test(data$GoodCredit,data$property) #significant
chisq.test(data$GoodCredit,data$otherplans) #significant
chisq.test(data$GoodCredit,data$housing) #significant
chisq.test(data$GoodCredit,data$cards) #not significant
chisq.test(data$GoodCredit,data$job) #not significant
chisq.test(data$GoodCredit,data$liable) #not significant
chisq.test(data$GoodCredit,data$tele) #not significant
chisq.test(data$GoodCredit,data$foreign) #significant

#Interpretation: We can remove the following columns:
#installment
#residence
#cards
#job
#liable
#tele

#Removing these columns
colnames(data)

data <- data[,-c(9,12,17,18,19,20)]
##########################################################################################

#Converting nominal variables to numeric

install.packages("fastDummies")
library(fastDummies)

data<-dummy_cols(data)

#Removing columns and also considering for Dummy Variable trap  
data <- data[,-c(2,4,5,7,8,9,10,11,13,14,15,16,20,25,35,40,45,49,52,56,59,62)]

colnames(data)

#################################################################################
#Handling Imbalance Dataset
library(ggplot2)
ggplot(data,aes(x = GoodCredit))+geom_bar()
table(data$GoodCredit)
#This looks balanced dataset.


#Splitting the data for training and testing
library(caTools)
set.seed(100)
split<-sample.split(Y = data$GoodCredit,SplitRatio = 0.7)
table(split)

training <- subset(data,split == T)
test <- subset(data,split == F)
nrow(training)
nrow(test)


#Logistic Regression

logit <-glm(GoodCredit~.,family = "binomial",data = training)
logit
#Null Deviance:	    855.2 
#Residual Deviance: 636.4 	AIC: 718.4

step(logit)

#This model gives us lower aic
logit <- glm(formula = GoodCredit ~ duration + age + checkingstatus_A13 + 
      checkingstatus_A14 + history_A32 + history_A33 + history_A34 + 
      purpose_A41 + purpose_A42 + purpose_A43 + purpose_A49 + purpose_A410 + 
      savings_A64 + savings_A65 + employ_A74 + others_A103 + otherplans_A143 + 
      housing_A152, family = "binomial", data = training)
logit
#Null Deviance:	    855.2 
#Residual Deviance: 650.2 	AIC: 688.2

# predict the model by using test dataset

logit_pred <- predict(logit,newdata = test,type = 'response')
head(logit_pred)

#Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults = confusionMatrix(logit_pred,test$GoodCredit,mode="prec_recall")
AccuracyResults

#threshold value - 50%
logit_pred_50 <- ifelse(logit_pred>=0.5,1,0)
cm <- table(test$GoodCredit, logit_pred_50)
cm
confusionMatrix(cm)
#Accuracy : 0.7433

#threshold value - 60%
logit_pred_60 <- ifelse(logit_pred>=0.6,1,0)
cm <- table(test$GoodCredit, logit_pred_60)
cm
confusionMatrix(cm)
#Accuracy : 0.7233

#threshold value - 70%
logit_pred_70 <- ifelse(logit_pred>=0.7,1,0)
cm <- table(test$GoodCredit, logit_pred_70)
cm
confusionMatrix(cm)
#Accuracy : 0.73

#threshold value - 80%
logit_pred_80 <- ifelse(logit_pred>=0.8,1,0)
cm <- table(test$GoodCredit, logit_pred_80)
cm
confusionMatrix(cm)
#Accuracy : 0.7167

#threshold value - 40%
logit_pred_40 <- ifelse(logit_pred>=0.4,1,0)
cm <- table(test$GoodCredit, logit_pred_40)
cm
confusionMatrix(cm)
#Accuracy : 0.75

###################################################################################################
#ROC -- AUC

install.packages("ROCR")
library(ROCR)

ROCRprediction <- prediction(test$GoodCredit,logit_pred_40)
ROCRprediction

ROCRperformance <- performance(ROCRprediction, 'tpr','fpr')

ROCRperformance 

plot(ROCRperformance)
plot(ROCRperformance, colorize = T, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
abline(a=0, b=1)


### Conclusion :
# 1) Accuracy - 75%
# 2) Data is linearly seperable
# 3) ROC curve above the AUC line
# 4) Cutoff value - 40~% (TPR - approx 60% & FPR - approx 20%)




























