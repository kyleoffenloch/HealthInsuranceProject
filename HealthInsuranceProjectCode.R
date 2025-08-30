#Kyle Offenloch
#08-28-2025
#Health Insurance Project
#Goal: use past policy data of 1338 policies to price health insurance premiums for future policies given different customer data

#setting seed and deciding 66.67% of data for training
set.seed(7997169)
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

ggplot(train_data, aes(x=age, y=charges, color = factor(smoker))) + geom_point() + geom_smooth()
ggplot(train_data, aes(x=bmi, y=charges)) + geom_point() + geom_smooth()
ggplot(train_data, aes(x=smoker, y=charges)) + geom_boxplot()
ggplot(train_data, aes(x=factor(children), y=charges)) + geom_boxplot()
ggplot(train_data, aes(x=factor(region),y=charges))+geom_boxplot()

numeric_vars <- train_data[sapply(train_data, is.numeric)]
cor(numeric_vars)


#Model 1: linear regression model with all 6 input variables. gaussian error distributions
model1<-glm(charges ~ age + sex + bmi + children + smoker + region, data = train_data, family=gaussian(link="log"))
summary(model1)

predictions1 <- predict(model1,newdata=test_data, type="response")

test_data <- data.frame(test_data,Model1Prediction=predictions1)

#Model 2: linear regression model with only age, bmi, smoker. gaussian error distributions
model2<-glm(charges ~ age + bmi + smoker, data = train_data, family=gaussian(link="log"))

predictions2 <- predict(model2,newdata=test_data,type="response")

test_data <- data.frame(test_data,Model2Prediction=predictions2)

#Model 3: Generalized liner model with all 6 input variables. gamma error with log link, to account for charges being strictly positive and positively skewed errors
model3 <- glm(charges ~ age + sex + bmi + children + smoker + region, data=train_data, family=Gamma(link="log"))

predictions3 <- predict(model3, newdata=test_data, type="response")

test_data <- data.frame(test_data, Model3Prediction=predictions3)

#Model 4: Generalized liner model with only age, bmi, smoker. gamma error with log link, to account for charges being strictly positive and positively skewed errors
model4 <- glm(charges ~ age + bmi + smoker, data=train_data, family=Gamma(link="log"))

predictions4 <- predict(model4, newdata=test_data, type="response")

test_data <- data.frame(test_data, Model4Prediction=predictions4)

#Testing the efficacy of each model on test data
#finding the SSE of each model
RMSE1<-sqrt(sum((test_data$Model1Prediction-test_data$charges)^2)/nrow(test_data))
RMSE2<-sqrt(sum((test_data$Model2Prediction-test_data$charges)^2)/nrow(test_data))
RMSE3<-sqrt(sum((test_data$Model3Prediction-test_data$charges)^2)/nrow(test_data)) 
RMSE4<-sqrt(sum((test_data$Model4Prediction-test_data$charges)^2)/nrow(test_data))

RMSE1
RMSE2
RMSE3
RMSE4

model1Plot <- ggplot(data=test_data, aes(Model1Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-RMSE1,RMSE1), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 1", x = "prediction")

model2Plot <- ggplot(data=test_data, aes(Model2Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-RMSE2,RMSE2), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 2", x = "prediction")

model3Plot <- ggplot(data=test_data, aes(Model3Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  labs(color = "Smoker", title = "Model 3", x = "prediction") +
  geom_abline(intercept = c(-RMSE3,RMSE3), slope=1, color = "purple") +
  ylim(0, max(test_data$Model3Prediction, test_data$charges) + 2000) +
  xlim(0, max(test_data$Model3Prediction, test_data$charges) + 2000)
  

model4Plot <- ggplot(data=test_data, aes(Model4Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  labs(color = "Smoker", title = "Model 4", x = "prediction") +
  geom_abline(intercept = c(-RMSE4,RMSE4), slope=1, color = "purple") +
  ylim(0, max(test_data$Model4Prediction, test_data$charges) + 2000) +
  xlim(0, max(test_data$Model4Prediction, test_data$charges) + 2000)

grid.arrange(model1Plot, model2Plot, model3Plot, model4Plot)




model1Res <- ggplot(data=test_data, aes(Model1Prediction, charges-Model1Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-RMSE1,RMSE1), slope=0, color = "purple") +
  labs(color = "Smoker", y = "residual", x = "prediction", title = "Model 1 Residuals")

model2Res <- ggplot(data=test_data, aes(Model2Prediction, charges-Model2Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-RMSE2,RMSE2), slope=0, color = "purple") +
  labs(color = "Smoker", y = "residual", x = "prediction", title = "Model 2 Residuals")

model3Res <- ggplot(data=test_data, aes(Model3Prediction, charges-Model3Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-RMSE3,RMSE3), slope=0, color = "purple") +
  labs(color = "Smoker", y = "residual", x = "prediction", title = "Model 3 Residuals")

model4Res <- ggplot(data=test_data, aes(Model4Prediction, charges-Model4Prediction, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=0, color = "steelblue") +
  geom_abline(intercept = c(-RMSE4,RMSE4), slope=0, color = "purple") +
  labs(color = "Smoker", y = "residual", x = "prediction", title = "Model 4 Residuals")

grid.arrange(model1Res, model2Res, model3Res, model4Res)



model_non <- glm(charges ~ age + bmi + children + region + sex,
                 data = subset(train_data, smoker == "no"),
                 family = Gamma(link = "log"))

model_smoker <- glm(charges ~ age + bmi + children + region + sex,
                    data = subset(train_data, smoker == "yes"),
                    family = Gamma(link = "log"))

model_interact <- glm(charges ~ smoker * (age + bmi + children + region + sex),
                      data = train_data,
                      family = Gamma(link = "log"))
test_data <- data.frame(test_data, Model5Prediction=predict(model_interact, newdata = test_data, type="response"))
test_data

ggplot(data=test_data, aes(Model5Prediction, charges, color = factor(smoker))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, color = "steelblue") +
  geom_abline(intercept = c(-RMSE5,RMSE5), slope=1, color = "purple") +
  labs(color = "Smoker", title = "Model 1", x = "prediction")

RMSE5<-sqrt(sum((test_data$Model5Prediction-test_data$charges)^2)/nrow(test_data))
RMSE5

#also do splitting of model, not just adding smoker interactions

#then make RMD, and comment, and put on github