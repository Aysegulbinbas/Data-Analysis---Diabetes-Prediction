
#Diabetes Prediction :

  
  Diabetes_data =read.csv("C:/Users/HP PROBOOK/Desktop/STAT412-PROJE/diabetes.csv",header = T,sep = ",")
head(Diabetes_data)
colnames(Diabetes_data)
summary(Diabetes_data)
str(Diabetes_data)
dim(Diabetes_data)

sd(Diabetes_data$Glucose,na.rm = T)
library(stringr) 
library(dplyr)


#Data Cleaning and Tidying


Diabetes_data$Gender  <- as.factor(Diabetes_data$Gender)
Diabetes_data$Outcome  <- as.factor(Diabetes_data$Outcome)
Diabetes_data$Exercise  <- as.factor(Diabetes_data$Exercise)
Diabetes_data$Pregnancies  <- as.factor(Diabetes_data$Pregnancies)  # ?t has small range,hence ?t took as factor.

str(Diabetes_data)

summary(Diabetes_data)

# According to summary of data set, 
#For categorical variables:
#The number of women is more than twice the number of men,hence it is not a balanced dataset for both gender.
#The outcome variable is also not balanced by whether or not they have diabetes since the number of people without diabetes is almost twice as high as those with diabetes.
#In this data set most of the people doing sport in the morning while very few people do sports both in the morning and in the evening.

#Continuous variables:

# we can say that he distribution of glucose,BloodPressure,BIM almost normal,while SkinThickness, Insulin and DiabetesPedigreeFunction show right skewed distibution.
#
# Age variable chance between 21 and 81.Mean and median of age almost equal.
#Also, there are some NA valus in the CalorieIntake variable.




#Missingless :
summary(Diabetes_data)
#there are some noisy data
# In summary, Glucose,BloodPressure,SkinThickness,Insulin,BMI contain 0 values,hwr it is impossible.

head(Diabetes_data)

Diabetes_data$SkinThickness[Diabetes_data$SkinThickness ==0] = "NA"
Diabetes_data$Glucose[Diabetes_data$Glucose ==0] = "NA"
Diabetes_data$BloodPressure[Diabetes_data$BloodPressure ==0] = "NA"
Diabetes_data$Insulin[Diabetes_data$Insulin ==0] = "NA"
Diabetes_data$BMI[Diabetes_data$BMI ==0] = "NA"
Diabetes_data$BMI[Diabetes_data$BMI ==0] = "NA"

#It was seen that the dataset contains NA values with '0' values. 

str(Diabetes_data)

Diabetes_data$Glucose  <- as.numeric(Diabetes_data$Glucose)
Diabetes_data$BloodPressure   <- as.numeric(Diabetes_data$BloodPressure )
Diabetes_data$SkinThickness  <- as.numeric(Diabetes_data$SkinThickness)
Diabetes_data$Insulin  <- as.numeric(Diabetes_data$Insulin)
Diabetes_data$BMI  <- as.numeric(Diabetes_data$BMI)
Diabetes_data$CalorieIntake  <- as.numeric(Diabetes_data$CalorieIntake)


colSums(is.na(Diabetes_data)) #there exist some na values in calorie intake.


#The subcutaneous tissue thickness range in males is from 1.60 mm to 25.45 mm, whereas it is from 3.40 mm to 25.20 mm in females.



library(ggplot2)

library(gridExtra)
#In order to look at the distributions of the variables :

options(repr.plot.width=6, repr.plot.height=6)
p1 <- ggplot(Diabetes_data, aes(Pregnancies)) + geom_bar()
p2 <- ggplot(Diabetes_data, aes(Gender)) + geom_bar()
p3 <- ggplot(Diabetes_data, aes(Glucose)) + geom_density()
p4 <- ggplot(Diabetes_data, aes(BloodPressure)) + geom_density()
p5 <- ggplot(Diabetes_data, aes(SkinThickness)) + geom_density()
p6 <- ggplot(Diabetes_data, aes(Insulin)) + geom_density()
p7 <- ggplot(Diabetes_data, aes(BMI)) + geom_density()
p8 <- ggplot(Diabetes_data, aes(DiabetesPedigreeFunction)) + geom_density()
p9 <- ggplot(Diabetes_data, aes(Age)) + geom_density()
p10 <- ggplot(Diabetes_data, aes(Outcome)) + geom_bar()
p11 <- ggplot(Diabetes_data, aes(CalorieIntake)) + geom_density()
p12 <- ggplot(Diabetes_data, aes(Exercise )) + geom_bar()
p13 <- ggplot(Diabetes_data, aes(SleepDuration)) + geom_density()
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,p9,p10,p11,p12,p13, ncol=4)

#In order to look at the relationships between the predictors and the response : 
sd(Age)
#corr:

contdata<-Diabetes_data[,-c(1,2,10,12)]

ggpairs(contdata,title="Correlogram")

library(corrplot)
library(RColorBrewer)
M <-cor(contdata)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


#Now that we have finished looking at the variables in a univariate aspect let's show some graphs based on 
#our response variable income stating our research questions.


#EDA :

#1-What is the average result of Glucose of Genders ?


tab1<-data.frame(table(Diabetes_data$Gender))
tab1

#In order to calculate the average wage of female :
mean(Diabetes_data[Diabetes_data$Gender=="M",]$Glucose)


#In order to calculate the average wage of male :
mean(Diabetes_data[Diabetes_data$Gender=="F",]$Glucose)

aggregate( Diabetes_data$Glucose ~ Gender, data = Diabetes_data, mean)



#Violin plot :

p1 <- ggplot(Diabetes_data, aes(x=Gender, y=Glucose)) + geom_violin(trim=FALSE)  + geom_boxplot(width=0.2,fill="beige") 
p1




#2-What is the average result of DiabetesPedigreeFunction of outcome ?



tab2<-data.frame(table(Diabetes_data$Outcome))
tab2

#In order to calculate the average wage of female :
mean(Diabetes_data[Diabetes_data$Outcome=="0",]$DiabetesPedigreeFunction)


#In order to calculate the average wage of male :
mean(Diabetes_data[Diabetes_data$Gender=="1",]$DiabetesPedigreeFunction)

aggregate( Diabetes_data$DiabetesPedigreeFunction ~ Outcome, data = Diabetes_data, mean)

#box plot:
p2 <- ggplot(Diabetes_data, aes(x=Outcome, y=DiabetesPedigreeFunction,color = Outcome)) + geom_boxplot()+scale_color_manual(values = c("#3a0ca3", "#c9184a"))
p2


#Violin plot :

p3 <- ggplot(Diabetes_data, aes(x=Outcome, y=DiabetesPedigreeFunction)) + geom_violin(trim=FALSE)  + geom_boxplot(width=0.2,fill="beige") 
p3





#What is the average result of DiabetesPedigreeFunction of male and female ?

tab<-data.frame(table(Diabetes_data$Gender))
tab

#In order to calculate the average wage of female :
mean(Diabetes_data[Diabetes_data$Gender=="F",]$DiabetesPedigreeFunction)


#In order to calculate the average wage of male :
mean(Diabetes_data[Diabetes_data$Gender=="M",]$DiabetesPedigreeFunction)

aggregate( Diabetes_data$DiabetesPedigreeFunction ~ Gender, data = Diabetes_data, mean)





#Violin plot :

p <- ggplot(Diabetes_data, aes(x=Gender, y=DiabetesPedigreeFunction)) + geom_violin(trim=FALSE)  + geom_boxplot(width=0.2,fill="beige") 
p


#Median of both gender almost equal to each other.


#3-What is the frequency distribution of exercise?



table_exercise<-table(Diabetes_data$Exercise)
table_exercise


# In order to show frequencies of each categories bar graph is used.
##for uniform color barplot(tab,col="green")
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
barplot(table_exercise,col=coul,main="Bar plot of exercise time of diabetes data"
        ,xlab="Exercise category",ylab="Frequency")

ggplot(Diabetes_data,aes(x=Exercise,y=BloodPressure))+geom_point(col="darkblue")+labs(title = "The relationship between blood pressure and exercise")+geom_smooth()


#While most of the observations belongs to the ?Morning category? ,the category with the least frequency is "Both"







# 4-What do you observe with respect to Diabetes Pedigree Function(rate of having diabetes ) of people by number of pregnancies?

summary(Diabetes_data$DiabetesPedigreeFunction)

boxplot(Diabetes_data$DiabetesPedigreeFunction~Diabetes_data$Pregnancies,
        main="The boxplot of rate of having diabetes by number of pregnancies",
        col=colors()[c(25,35,45,50,40)],xlab="Number of pregnancies",
        ylab="Rate of having diabetes")

# ?t can be seen that there exists many outlier observations. Also, it seems that number of pregnancies does not have an effect of rate of having diabetes.


#Q5. What is the association between BMI and calorie intake by outcome ?


ggplot(Diabetes_data,aes(x=CalorieIntake,y=BMI,col=Outcome))+geom_point()+labs(title="Association between BMI and calorie intake by outcome")


#There is no significant association between calorie intake and BMI.It can be seen that when the  calorie intake and BMI increase, Diabetes predigree function also increase.  



#ggplot(Diabetes_data,aes(x=CalorieIntake,y=DiabetesPedigreeFunction))+geom_point(col="darkred")+labs(title = "The relationship between rate of having diabetes and calorie intake")+geom_smooth()
#As it can be seen from the plot above,we cannot say that there is a clear relationship.


#RQ6:
#What is the association between rate of having diabetes and age by grouped by gender?


#ggplot(Diabetes_data,aes(x=Age,y=DiabetesPedigreeFunction))+geom_point(col="darkblue")+labs(title = "The relationship between rate of having diabetes and age  according to gender")+geom_smooth()+facet_wrap(.~Gender)

# there is no association among this variables.
#Also ,in both gender there are extreme values that is greater than 2.0 

ggplot(Diabetes_data,aes(x=Age,y=DiabetesPedigreeFunction,col=Gender))+geom_point()+labs(title="The relationship between rate of having diabetes and age  according to gender")


#After the data exploration using visualization techniques, let?s conclude this part with the appropriate 
#statistical tests.


#######################################333




# Handling with the Missing Values


colSums(is.na(Diabetes_data)) #there exist some na values in calorie intake.

library(VIM)

aggr_plot <- aggr(Diabetes_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Diabetes_data), ylab=c("Histogram of missing data","Pattern"))

#the table show that almost 0.49 % of Insulin and 0.30% of SkinThickness are misssing.


#In order to impute missing values mice package was used.
install.packages("mice")
library(mice)

init2 = mice(Diabetes_data, maxit=0) 
meth2 = init2$method
predM2 = init2$predictorMatrix

set.seed(103)
New_data= mice(Diabetes_data, method=meth2, predictorMatrix=predM2, m=5)


New_data <- complete(New_data)
colSums(is.na(New_data))






cormax=round(cor(New_data[,c(3:9,11,13)]),3) 
cormax

library(corrplot)
corrplot(cormax, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


summary(Diabetes_data)
summary(New_data)



###############################

#Eda RQ:
#?1-	What is the average amount of glucose of genders?
# 2-What is the average result of DiabetesPedigreeFunction of outcome ?
#?3-	What is the frequency distribution of exercise variable?
#?4-	What do we observe with respect to Diabetes Pedigree Function (rate of having diabetes ) of people by number of pregnancies?
#?5-	What is the association between BMI and calorie intake by outcome and rate of having diabetes?
#?6-	What is the relation between rate of having diabetes and age by grouped gender?


#Confirmatory data analysis :

#1-	What is the average amount of glucose of genders?

# Is the average glucose amount is equal to for both gender?  


library(dplyr)

library(dplyr)
group_by(New_data, Gender) %>%
  summarise(
    count = n(),
    mean = mean(Glucose, na.rm = TRUE),
    sd = sd(Glucose, na.rm = TRUE)
  )

# Plot weight by group and color by group
library("ggpubr")
ggboxplot(Diabetes_data, x = "Gender", y = "Glucose", 
          color = "Gender", palette = c("#00AFBB", "#E7B800"),
          ylab = "Glucose", xlab = "Gender")

#As can be seen from the graph that the level of glucose in blood differ on male and female.

#Assumption 1: Are the two samples independents?

#Yes, since the samples from men and women are not related.

#Assumtion 2: Are the data from each of the 2 groups follow a normal distribution?



# Shapiro-Wilk normality test for Men's weights
with(New_data, shapiro.test(Glucose[Gender == "F"]))
# Shapiro-Wilk normality test for Women's weights
with(New_data, shapiro.test(Glucose[Gender == "M"])) 
#P-values are not follow normality,hence we need to use transformation or non-parametric version of t test.
#Wilcoxon rank-sum test:


wilcox.test(Glucose ~ Gender , data = New_data)
#The result is that true location shift is not equal to 0





#############################33

#2-What is the average result of DiabetesPedigreeFunction of outcome ?


library(dplyr)
group_by(New_data, Outcome) %>%
  summarise(
    count = n(),
    mean = mean(DiabetesPedigreeFunction, na.rm = TRUE),
    sd = sd(DiabetesPedigreeFunction, na.rm = TRUE)
  )

# Plot weight by group and color by group
library("ggpubr")
ggboxplot(New_data, x = "Outcome", y = "DiabetesPedigreeFunction", 
          color = "Outcome", palette = c("#00AFBB", "#E7B800"),
          ylab = "DiabetesPedigreeFunction", xlab = "Outcome")

#As can be seen from the graph that the rate of DiabetesPedigreeFunction  has a small difference wrt male and female.

#Assumption 1: Are the two samples independents?

#Yes, since the samples from men and women are not related.

#Assumtion 2: Are the data from each of the 2 groups follow a normal distribution?



# Shapiro-Wilk normality test for Men's weights
with(New_data, shapiro.test(DiabetesPedigreeFunction[Gender == "F"]))
# Shapiro-Wilk normality test for Women's weights
with(New_data, shapiro.test(DiabetesPedigreeFunction[Gender == "M"])) 
#P-values are not follow normality,hence we need to use transformation or non-parametric version of t test.
#Wilcoxon rank-sum test:


wilcox.test(DiabetesPedigreeFunction ~ Outcome , data = New_data)

#true location shift is not equal to 0

######################################################


#?3-	What is the frequency distribution of exercise variable,and ....?
# Is the average blood pressure of people under each exercise categories("no", "morning","evening","both") is equal to each other?


group1= Diabetes_data[Diabetes_data$Exercise=="No",]
group2= Diabetes_data[Diabetes_data$Exercise=="Evening",]
group3= Diabetes_data[Diabetes_data$Exercise=="Morning",]
group4= Diabetes_data[Diabetes_data$Exercise=="Both",]



#Ho= : ?(group1)=?(group2)=?(group3)=?(group4)
#Ha: not all equal
# In order to get an idea about the mean of each group, let's visualize the data

library(ggplot2)

boxplot(group1$BloodPressure,group2$BloodPressure,group3$BloodPressure,group4$BloodPressure)

#Look at test :

grouped_data =rbind(group1,group2,group3,group4)

anova <- aov(BloodPressure ~ Exercise, data = grouped_data)
summary(anova)


#According to ANOVA test, we reject the null hypothesis because the p value is less than 0.05. 
#It indicates that some of the group means are different, but we don?t know which pairs of groups are different. So, we need to conduct multiple pairwise comparisons.


TukeyHSD(anova)
# There is a significant difference between No-Evening and No-Morning,s you can see the confidence interval for their mean difference does not include 0.


#Asumptions of ANOVA

#Errors are normally distributed

#Errors have constant variance

#Errors are independent from one another

shapiro.test(residuals(anova))

# p-value = 0.0002022 < alpha ,not normal

plot(anova,2)

plot(anova, 1)
#When we look the fitted values vs residual plot, the variances of each groups are close to each other because the spread of observations is almost the same.




###################################################


#?4-	What do we observe with respect to Diabetes Pedigree Function 
#(rate of having diabetes ) of people by number of pregnancies?


g1= New_data[New_data$Pregnancies=="0",]
g2= New_data[New_data$Pregnancies=="1",]
g3= New_data[New_data$Pregnancies=="2",]
g4= New_data[New_data$Pregnancies=="3",]
g5= New_data[New_data$Pregnancies=="4",]
g6= New_data[New_data$Pregnancies=="5",]

#Ho= : ?(group1)=?(group2)=?(group3)=?(group4)=?(group5)=?(group6)
#Ha: not all equal
# In order to get an idea about the mean of each group, let's visualize the data

library(ggplot2)

boxplot(g1$Pregnancies,g2$Pregnancies,g3$Pregnancies,g4$Pregnancies,g5$Pregnancies,g6$Pregnancies)

#Look at test :

str(g_data)
g_data =rbind(g1,g2,g3,g4,g5,g6)

anova.pregn <- aov(DiabetesPedigreeFunction ~ Pregnancies, data = g_data)
summary(anova.pregn)


#According to ANOVA test, we cannot reject the null hypothesis because the p value is greater than 0.05. 
#It indicates that here is no difference.


#Asumptions of ANOVA

#Errors are normally distributed

#Errors have constant variance

#Errors are independent from one another

shapiro.test(residuals(anova.pregn))

plot(anova.pregn,2)

plot(anova.pregn, 1)
#When we look the fitted values vs residual plot, the variances of each groups are close to each other because the spread of observations is almost the same.



kruskal.test(DiabetesPedigreeFunction ~ Pregnancies, data = g_data)
#The result same with anova.



#5-What is the association between BMI and calorie intake by outcome?

library(biotools)
library("ICSNP")
library(psych)



boxM(New_data[c(7,11)],New_data[,c(10)])

# Compute Hotelling T 2 based on two sample data matrices
HotellingsT2(New_data[c(7)],New_data[c(11)])


#The results indicated that T2 = 3461, with 2 and 1534 df and p = 2.2e-16
#The null hypothesis of no group mean difference is rejected.
# The alternative hypothesis is accepted?true location difference is not equal to c(0,0). 
#Also corr checed correlated to each other.











###############  


set.seed(123) # setting seed to generate a reproducible random sampling
# creating training data as 80% of the dataset
random_sample <- createDataPartition(New_data$Outcome, p = 0.8, list = FALSE)
train  <- New_data[random_sample, ] # generating training dataset from the random_sample
test <- New_data[-random_sample, ] # generating testing dataset from rows which are not included in random_sample
str(train)
str(test)


library(dplyr)
library(MASS)
scale<-function(x) {
  (x-min(x))/(max(x)-min(x))
}




numeric<-train%>%dplyr::select(Glucose)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(Glucose)
numeric2<-scale(numeric2)


train<-train[,-3]
test<-test[,-3]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)
str(test)



numeric<-train%>%dplyr::select(BloodPressure)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(BloodPressure)
numeric2<-scale(numeric2)


train<-train[,-3]
test<-test[,-3]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)
str(test)




numeric<-train%>%dplyr::select(SkinThickness)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(SkinThickness)
numeric2<-scale(numeric2)


train<-train[,-3]
test<-test[,-3]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)
str(test)
str(train)

numeric<-train%>%dplyr::select(Insulin)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(Insulin)
numeric2<-scale(numeric2)


train<-train[,-3]
test<-test[,-3]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)








numeric<-train%>%dplyr::select(BMI)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(BMI)
numeric2<-scale(numeric2)


train<-train[,-3]
test<-test[,-3]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)












numeric<-train%>%dplyr::select(DiabetesPedigreeFunction)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(DiabetesPedigreeFunction)
numeric2<-scale(numeric2)


train<-train[,-3]
test<-test[,-3]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)





test$Age<-NULL
train$Age<-NULL

str(train)

numeric<-train%>%dplyr::select(CalorieIntake)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(CalorieIntake)
numeric2<-scale(numeric2)


train<-train[,-4]
test<-test[,-4]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)

str(train)

numeric<-train%>%dplyr::select(SleepDuration)
numeric<-scale(numeric)

numeric2<-test%>%dplyr::select(SleepDuration)
numeric2<-scale(numeric2)


train<-train[,-5]
test<-test[,-5]

train<-cbind(train,numeric)



test<-cbind(test,numeric2)


summary(train)


train$Gender<-NULL
train$Pregnancies<-NULL
train$Exercise<-NULL
summary(train)
test$Gender<-NULL
test$Pregnancies<-NULL
test$Exercise<-NULL
summary(test)

#############################



# Modeling:

library(caret)#its a classification and regression training


set.seed(123) # setting seed to generate a reproducible random sampling
# creating training data as 80% of the dataset
random_sample <- createDataPartition(New_data$Outcome, p = 0.8, list = FALSE)
train  <- New_data[random_sample, ] # generating training dataset from the random_sample
test <- New_data[-random_sample, ] # generating testing dataset from rows which are not included in random_sample

prop.table(table(train$Outcome))
prop.table(table(test$Outcome))
#the proportion of the levels of the response are close to each other. Now, let us fit the model with Logistic regression

summary(train)
cor(train[,c(3,4,5,6,7,9,11)])

library(devtools)
remotes::install_github("cran/DMwR")
#For smooth:

library(DMwR)
set.seed(111)
trainsmote <- SMOTE(Outcome~.,data = train)

summary(trainsmote)
table(trainsmote$Outcome)   # almost balance data
table(train$Outcome) 


library(tidyverse)
library(smotefamily)
library(caret)
library(leaps)
#install.packages("leaps")


#Insulin and Glucose, BMI and  SkinThickness are correlated,
#to prevent the existence of the multicollinearity, one of the variables are included in the model. 
#
m <- glm(Outcome ~ ., data=trainsmote, family="binomial")
summary(m)

#model with significant variables:
fit.sig <- glm(Outcome ~ Pregnancies +Glucose+ SkinThickness
               +CalorieIntake+Exercise+SleepDuration,
               data=trainsmote, family="binomial")
summary(fit.sig)

# According to result, Glucose, CalorieIntake,Exercise and SleepDuration are significant.

anova(fit.sig, m, test="Chisq") 

fit.sig$coefficients

#coeff :
#The coefficient estimate of the variable glucose is 0.33, hence, increase in glucose is related with rise in the probability of being diabetes.
#Also, when we look at the coefficient ,it can be understan that doing sport has negative relation with being diabets ,while no exercise has positive assotiation with being diabets.


exp(fit.sig$coefficients)

# Odds ratio and 95% CI
exp(cbind(OR = coef(fit.sign), confint(fit.sign)))
#Interp. odds ratios:

# for every one unit increase in glucose, the odds of being diabetes-positive (vs not being diabetes-positive) increases by a factor of 1.03.
#
#install.packages("ROCR")
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(data.table)
dim(predicted3)

library(optimalCutoff)
library(cutpointr)
#install.packages("cutpointr")

# prediction

test$prediction<- predict(fit.sig, test, type="response")
head(test$prediction)
train$prediction <- predict(fit.sig, trainsmote, type="response")


library(OptimalCutpoints)

#optimal cut off:
library(InformationValue)
library(AER)
library(cutpointr)
library(car)
library(tidyverse)
library(dplyr)
library(cutpointr)
optCutOff<- optimalCutoff(test$prediction, prediction)[1]  #0.26
pred.test2 = ifelse(test$prediction > 0.26, 1, 0)
head(pred.test2)

library(InformationValue)

##########3
#Creating confusion matrix

#install.packages("AER")
library("AER")
# Assessing model accuracy:
library(magrittr)
library(pROC)
library(OptimalCutpoints)
#install.packages("cutpointr")
str(test)
##############

library(tidyverse)
library(caret)
library(mlbench)



pred_model1 <- predict(fit.sig,test, type="response")
pred_model1.1  <- as.integer(pred_model1>0.5)
pred_model1.2  <- as.integer(pred_model1>0.26)

confusionMatrix(as.factor(pred_model1.1),test$Outcome)
confusionMatrix(as.factor(pred_model1.2),test$Outcome)


#confusionMatrix(as.factor(pred_model1.2), test[,-1], mode = "everything", positive="1")




pred_LOG<- ifelse(predict(pred_model1.2, type = "response",newdata = test[,-1] ) > 0.26, 1,0)
plot(concrete_model)
head(pred_ann)





#MODELING PART
#1) ann
ctrl <- trainControl(method = "cv",number = 5)
library(neuralnet)
concrete_model<-neuralnet(formula =Outcome~.,,data=train,stepmax = 10000000,act.fct="logistic")

pred_ann<- ifelse(predict(concrete_model, type = "response",newdata = test[,-1] ) > 0.4, 1,0)
plot(concrete_model)
head(pred_ann)

pred_a = apply(pred_ann, 1, function(x){x[1]*0+x[2]*1})


pred_a =as.data.frame(pred_a)
row.names(pred_a)=NULL


confusionMatrix(factor(pred_a$pred_a),factor(test[,1]))

AIC <- concrete_model$result.matrix[4,1]
paste("AIC: ", round(AIC,3))


BIC <- concrete_model$result.matrix[5,1]
paste("BIC: ", round(BIC, 3))

#Support Vector Machine
library(kernlab)
set.seed(123)
#tuning

library(caret)
tuneGrid <- expand.grid(
  C = c(0.25, .5, 1),
  sigma = 0.1
)
svm.model.full.tuned <- train(
  Outcome ~ .,
  data = train,
  method = 'svmRadial',
  
  trControl = ctrl,
  tuneGrid = tuneGrid
)
svm.model.full.tuned
svm.model.full.tuned $bestTune


library(tidyverse)

pred_svm<- predict(svm.model.full.tuned, test %>% dplyr::select(-Outcome)) 
plot(svm.model.full.tuned)

confusionMatrix(factor(pred_svm),factor(test[,1]))





#RANDOM FOREST
library(randomForest)
#tuning
parameters<-tuneRF(train[,-1],train[,1])
plot(parameters)

#tuning sonrasý mtry=2 olarak bulduk

set.seed(123)
rf1<-randomForest(Outcome~.,data=train,mtry=2)
plot(rf1)


#The number of trees is 500 in the model and no. of variable tried at each split are 2. while Classification error in nondiabetes(0) is 0.004,which is approximatelty 0.4%, diabetes (1) is 0.09 which is almost 9%.
pred_RF <- predict(rf1,test[,-1])


confusionMatrix(factor(pred_RF),factor(test[,1]))

length(test[,1])


#xgbus
#install.packages("xgboost")


defaultW <- getOption("warn") 

options(warn = -1) 



train.control <- trainControl( method = "cv", number = 5, search = 'grid')

set.seed(132)
xgb_fit<- train(Outcome~., data=train, 
                method = "xgbTree", 
                trControl = train.control
)
options(warn = defaultW)

plot(xgb_fit)
pred_XG <- predict(xgb_fit,test[,-1])
confusionMatrix(factor(pred_XG),factor(test[,1]))
# load libraries
library(xgboost)
library(caret)
library(dplyr)
library(DiagrammeR)
install.packages("DiagrammeR")

# plot the first tree
xgb.plot.tree(model = xgb_fit$finalModel, trees = 1)

