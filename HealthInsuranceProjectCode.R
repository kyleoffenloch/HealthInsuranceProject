#Kyle Offenloch
#08-28-2025
#Health Insurance Project
#Goal: use past policy data of 1338 policies to price health insurance premiums for future policies given different customer data

#setting seed and deciding 66.67% of data for training
set.seed(123)
split.ratio <- 2/3

library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")


#importing data
data <- read.csv("insurance.csv")
df<-data

train_indices<-sample(seq_len(nrow(data)), size = floor(split.ratio * nrow(data)))

train_data<-data[train_indices,]
test_data<-data[-train_indices,]

#making charts to view raw training data
agechart<-ggplot(train_data, aes(x = age)) +
  geom_histogram(binwidth = 1,fill = "steelblue", color = "white") +
  ggtitle("age Distribution") +
  theme_minimal()
sexchart<-ggplot(train_data, aes(x = "", fill=sex)) +
  geom_bar(width=1, color="blue") +
  coord_polar(theta = "y") +
  ggtitle("sex Distribution") +
  theme_void()
bmichart<-ggplot(train_data, aes(x=bmi)) +
  geom_histogram(binwidth=1, fill="steelblue", color="white") +
  ggtitle("bmi Distribution") +
  theme_minimal()
childrenchart<-ggplot(train_data, aes(x=children)) +
  geom_histogram(binwidth=1, fill="steelblue", color="white") +
  ggtitle("child Distribution") +
  theme_minimal()
smokerchart<-ggplot(train_data, aes(x = "", fill=smoker)) +
  geom_bar(width=1, color="blue") +
  coord_polar(theta = "y") +
  ggtitle("smoker Distribution") +
  theme_void()
regionchart<-ggplot(train_data, aes(x = "", fill=region)) +
  geom_bar(width=1, color="blue") +
  coord_polar(theta = "y") +
  ggtitle("region Distribution") +
  theme_void()
chargeschart<-ggplot(train_data, aes(x=charges)) +
  geom_histogram(bins = 50, fill="steelblue", color="white") +
  ggtitle("expenses Distribution") +
  theme_minimal()


#overview of all raw data
grid.arrange(agechart, sexchart, bmichart, childrenchart, smokerchart, regionchart, chargeschart)


#viewing variables effects on charges

#very obvious higher charges for smokers
ggplot(train_data, aes(x=factor(smoker), y=charges))  + labs(y="Charges", x="Smoking Status", title="Charges by Smoking Status") + geom_boxplot(fill="steelblue")

#very obvious difference between smoker and non-smoker charges, with smoker charges having very high variance
ggplot(train_data, aes(x=age, y=charges, color = factor(smoker))) + labs(y="Charges", x="Age", title="Charges vs Age by Smoker", color="Smoker") + geom_point() + geom_smooth()

#Very little trend in charges with BMI for nonsmokers, upward trend in charges with BMI over 30 when smoker, very high variance
ggplot(train_data, aes(x=bmi, y=charges, color = factor(smoker))) + labs(y="Charges", x="BMI", title="Charges vs BMI by Smoker", color="Smoker") + geom_point() + geom_smooth()

#very little trend in charges with children
ggplot(train_data, aes(x=factor(children), y=charges)) + labs(y="Charges", x="Children", title="Charges vs Number of Children by Smoker") + geom_boxplot(fill="steelblue") + facet_grid(~smoker)

#very little trend in charges with region
ggplot(train_data, aes(x=factor(region), y=charges)) + labs(y="Charges", x="Region", title="Charges vs Region by Smoker") + geom_boxplot(fill="steelblue") + facet_grid(~smoker)

#very little trend in charges by sex
ggplot(train_data, aes(x=factor(sex), y=charges)) + labs(y="Charges", x="Sex", title="Charges vs Sex by Smoker") + geom_boxplot(fill="steelblue") + facet_grid(~smoker)

#viewing correlation between numeric variables
numeric_vars <- train_data[sapply(train_data, is.numeric)]
cor(numeric_vars)


#Model 1: linear regression model with all 6 input variables. gaussian error distributions, includes unrealistic negative charges as a side effect of gaussian
model1<-lm(charges ~ age + sex + bmi + children + smoker + region, data = train_data)

predictions1 <- predict(model1,newdata=test_data)

test_data <- data.frame(test_data,Model1Prediction=predictions1)

#Model 2: linear regression model with only age, bmi, smoker. gaussian error distributions, includes unrealistic negative charges as a side effect of gaussian
model2<-lm(charges ~ age + bmi + smoker, data = train_data)

predictions2 <- predict(model2,newdata=test_data)

test_data <- data.frame(test_data,Model2Prediction=predictions2)

#Model 3: Generalized liner model with all 6 input variables. gamma error with log link, to account for charges being strictly positive and positively skewed errors
model3 <- glm(charges ~ age + sex + bmi + children + smoker + region, data=train_data, family=Gamma(link="log"))

predictions3 <- predict(model3, newdata=test_data, type="response")

test_data <- data.frame(test_data, Model3Prediction=predictions3)

#Model 4: Generalized liner model with only age, bmi, smoker. gamma error with log link, to account for charges being strictly positive and positively skewed errors
model4 <- glm(charges ~ age + bmi + smoker, data=train_data, family=Gamma(link="log"))

predictions4 <- predict(model4, newdata=test_data, type="response")

test_data <- data.frame(test_data, Model4Prediction=predictions4)

#Model 5: Generalized linear model with all 6 input variables, with interaction between smoker and each other variable, using gamma error
model5 <- glm(charges ~ smoker * (age + bmi + children + region + sex),
                      data = train_data,
                      family = Gamma(link = "log"))

predictions5 <- predict(model5, newdata=test_data, type="response")

test_data <- data.frame(test_data, Model5Prediction=predictions5)

#Model 6: Generalized linear model with age and BMI input variables, with interaction between smoker and each other variable, using gamma error
model6 <- glm(charges ~ smoker * (age + bmi),
              data = train_data,
              family = Gamma(link = "log"))

predictions6 <- predict(model6, newdata=test_data, type="response")

test_data <- data.frame(test_data, Model6Prediction=predictions6)

#Testing the efficacy of each model on test data
#finding the RMSE of each model

RMSE1<-sqrt(sum((test_data$Model1Prediction-test_data$charges)^2)/nrow(test_data))
RMSE2<-sqrt(sum((test_data$Model2Prediction-test_data$charges)^2)/nrow(test_data))
RMSE3<-sqrt(sum((test_data$Model3Prediction-test_data$charges)^2)/nrow(test_data)) 
RMSE4<-sqrt(sum((test_data$Model4Prediction-test_data$charges)^2)/nrow(test_data))
RMSE5<-sqrt(sum((test_data$Model5Prediction-test_data$charges)^2)/nrow(test_data))
RMSE6<-sqrt(sum((test_data$Model6Prediction-test_data$charges)^2)/nrow(test_data))


r_squared_1 <- 1-sum((test_data$Model1Prediction-test_data$charges)^2)/sum((test_data$charges-mean(test_data$charges))^2)
r_squared_2 <- 1-sum((test_data$Model2Prediction-test_data$charges)^2)/sum((test_data$charges-mean(test_data$charges))^2)
r_squared_3 <- 1-sum((test_data$Model3Prediction-test_data$charges)^2)/sum((test_data$charges-mean(test_data$charges))^2)
r_squared_4 <- 1-sum((test_data$Model4Prediction-test_data$charges)^2)/sum((test_data$charges-mean(test_data$charges))^2)
r_squared_5 <- 1-sum((test_data$Model5Prediction-test_data$charges)^2)/sum((test_data$charges-mean(test_data$charges))^2)
r_squared_6 <- 1-sum((test_data$Model6Prediction-test_data$charges)^2)/sum((test_data$charges-mean(test_data$charges))^2)

MAE1 <- mean(abs(test_data$Model1Prediction-test_data$charges))
MAE2 <- mean(abs(test_data$Model2Prediction-test_data$charges))
MAE3 <- mean(abs(test_data$Model3Prediction-test_data$charges))
MAE4 <- mean(abs(test_data$Model4Prediction-test_data$charges))
MAE5 <- mean(abs(test_data$Model5Prediction-test_data$charges))
MAE6 <- mean(abs(test_data$Model6Prediction-test_data$charges))


RMSE1
RMSE2
RMSE3
RMSE4
RMSE5
RMSE6

r_squared_1
r_squared_2
r_squared_3
r_squared_4
r_squared_5
r_squared_6

MAE1
MAE2
MAE3
MAE4
MAE5
MAE6




model1Plot <- ggplot(data=test_data, aes(Model1Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-MAE1,MAE1), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 1", x = "Prediction", y = "Charges")

model2Plot <- ggplot(data=test_data, aes(Model2Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-MAE2,MAE2), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 2", x = "Prediction", y = "Charges")

model3Plot <- ggplot(data=test_data, aes(Model3Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  labs(color = "Smoker", title = "Model 3", x = "Prediction", y = "Charges") +
  geom_abline(intercept = c(-MAE3,MAE3), slope=1, color = "purple") +
  ylim(0, max(test_data$Model3Prediction, test_data$charges) + 2000) +
  xlim(0, max(test_data$Model3Prediction, test_data$charges) + 2000)
  
model4Plot <- ggplot(data=test_data, aes(Model4Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  labs(color = "Smoker", title = "Model 4", x = "Prediction", y = "Charges") +
  geom_abline(intercept = c(-MAE4,MAE4), slope=1, color = "purple") +
  ylim(0, max(test_data$Model4Prediction, test_data$charges) + 2000) +
  xlim(0, max(test_data$Model4Prediction, test_data$charges) + 2000)

model5Plot <- ggplot(data=test_data, aes(Model5Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-MAE5,MAE5), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 5", x = "Prediction", y = "Charges")

model6Plot <- ggplot(data=test_data, aes(Model6Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-MAE6,MAE6), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 6", x = "Prediction", y = "Charges")

grid.arrange(model1Plot, model2Plot, model3Plot, model4Plot, model5Plot, model6Plot)




model1Res <- ggplot(data=test_data, aes(Model1Prediction, charges-Model1Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-MAE1,MAE1), slope=0, color = "purple") +
  labs(color = "Smoker", y = "Residual", x = "Prediction", title = "Model 1 Residuals")

model2Res <- ggplot(data=test_data, aes(Model2Prediction, charges-Model2Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-MAE2,MAE2), slope=0, color = "purple") +
  labs(color = "Smoker", y = "Residual", x = "Prediction", title = "Model 2 Residuals")

model3Res <- ggplot(data=test_data, aes(Model3Prediction, charges-Model3Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-MAE3,MAE3), slope=0, color = "purple") +
  labs(color = "Smoker", y = "Residual", x = "Prediction", title = "Model 3 Residuals")

model4Res <- ggplot(data=test_data, aes(Model4Prediction, charges-Model4Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-MAE4,MAE4), slope=0, color = "purple") +
  labs(color = "Smoker", y = "Residual", x = "Prediction", title = "Model 4 Residuals")

model5Res <- ggplot(data=test_data, aes(Model5Prediction, charges-Model5Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-MAE5,MAE5), slope=0, color = "purple") +
  labs(color = "Smoker", y = "Residual", x = "Prediction", title = "Model 5 Residuals")

model6Res <- ggplot(data=test_data, aes(Model6Prediction, charges-Model6Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-MAE6,MAE6), slope=0, color = "purple") +
  labs(color = "Smoker", y = "Residual", x = "Prediction", title = "Model 6 Residuals")


grid.arrange(model1Res, model2Res, model3Res, model4Res, model5Res, model6Res)




