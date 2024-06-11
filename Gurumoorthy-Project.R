#Libraries used 
library(psych)
library(aod)
library(vcd)
library(ggplot2)
library(caret)
library(pROC)

#Loading the data into the variable 'data'
data = read.csv("/Users/amruguru/Downloads/cardio.csv",header = T)
attach(data)

#Summarizing the data by cardio column (0 = presence of cardiovascular disease, 1 = absence of cardiovascular disease)
table(cardio)

#Adding BMI column using formula : BMI = (weight(kg)/(height(cm)^2))*10000
data$bmi <- (data$weight/((data$height)^2))*10000

#Creating a dataframe with important columns that pertain to the disease
heart_data = subset(data, select = c("gender","cholesterol","gluc","smoke","alco","active","cardio"))
attach(heart_data)

#Important Features summary data
tapply(gender, cardio ,mean)
tapply(cholesterol, cardio, mean)
tapply(gluc, cardio, mean)
tapply(smoke, cardio, mean)
tapply(alco, cardio, mean)
tapply(active, cardio, mean)

#Checking for missing values 
sum(is.na(heart_data))

#Describing the data of Important Features
describe(heart_data)

#Violin plots
ggplot(data, aes(x = gender, y = cardio)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Violin Plot of Value by Category", x = "Gender", y = "Cardio")
ggplot(data, aes(x = cholesterol, y = cardio)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Violin Plot of Value by Category", x = "Cholesterol", y = "Cardio")
ggplot(data, aes(x = gluc, y = cardio)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Violin Plot of Value by Category", x = "Glucose", y = "Cardio")
ggplot(data, aes(x = smoke, y = cardio)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Violin Plot of Value by Category", x = "Smoke", y = "Cardio")
ggplot(data, aes(x = alco, y = cardio)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Violin Plot of Value by Category", x = "Alcohol", y = "Cardio")
ggplot(data, aes(x = active, y = cardio)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Violin Plot of Value by Category", x = "Active", y = "Cardio")

#Box Plots
boxplot(gender~cardio, data=data, main="gender vs. cardio", xlab="gender", 
        ylab="cardio")
boxplot(cholesterol~cardio, data=data, main="cholesterol vs. cardio", xlab="cholesterol", 
        ylab="cardio")
boxplot(gluc~cardio, data=data, main="gluc vs. cardio", xlab="gluc", 
        ylab="cardio")
boxplot(smoke~cardio, data=data, main="smoke vs. cardio", xlab="smoke", 
        ylab="cardio")
boxplot(alco~cardio, data=data, main="alco vs. cardio", xlab="alco", 
        ylab="cardio")
boxplot(active~cardio, data=data, main="active vs. cardio", xlab="active", 
        ylab="cardio")

#Correlation
cor <-cor(heart_data)
cor

#Simple logistic regression
m <- glm(data$cardio ~ data$cholesterol, family=binomial)
summary(m)

#OR per 1 unit increase with the confidence interval 
exp(cbind(OR = coef(m), confint.default(m)))

#P-value and chi statistic
wald.test(b=coef(m), Sigma=vcov(m), Terms = 2)

#ROC Curve
data$prob <- predict(m, type=c("response"))
data$Yhat = ifelse(data$prob > .5, 1, 0)
Yhat <- factor(data$Yhat, levels = c("1", "0"))
event <- factor(data$cardio, levels = c("1", "0"))
confusionMatrix(table(Yhat,event))
g <- roc(data$cardio ~ data$prob)
plot(g)
plot(1-g$specificities, g$sensitivities, type="l", xlab="1-specificity", ylab="Sensitivity", main="ROC curve")
abline(a=0, b=1)
grid()

#Checking the model
auc(g) 

#Multiple Linear Regression
m <- glm(data$cardio ~ data$gender+data$cholesterol+data$gluc+data$smoke+data$alco+data$active, 
         family=binomial)
summary(m)
coef(summary(m))[,"z value"]
#P-value and chi statistic
wald.test(b=coef(m), Sigma=vcov(m), Terms = 2:5)

#ORs per 1 unit increase
exp(cbind(OR = coef(m), confint.default(m)))

#Predicted risk 
risk <- predict(m, type=c("response"))

#ROC Curve
data$prob <- predict(m, type=c("response"))
data$Yhat = ifelse(data$prob > .5, 1, 0)
Yhat <- factor(data$Yhat, levels = c("1", "0"))
event <- factor(data$cardio, levels = c("1", "0"))
confusionMatrix(table(Yhat,event))
g <- roc(data$cardio ~ data$prob)
plot(g)
plot(1-g$specificities, g$sensitivities, type="l", xlab="1-specificity", ylab="Sensitivity", main="ROC curve")
abline(a=0, b=1)
grid()

#Checking the model
auc(g) 


