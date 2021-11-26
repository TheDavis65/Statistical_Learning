# Chapter 3 Lab: Linear Regression 

pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat", "leaps", "caret",
               "car", "devtools")

# Simple Linear Regression


names(Boston)
dim(Boston)
lm.fit=lm(medv~lstat) # medv = median house value.
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
confint(lm.fit, level = 0.99)

confint(lm.fit)
confint(lm.fit)[1,]
confint(lm.fit)[2,]

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat,medv)
abline()
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
?hatvalues
str(lm.fit)
par(mfrow=c(1,1))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

lm.fit1=lm(medv~.-age -indus,data=Boston)


# Interaction Terms

lm.fit_kryds <- lm(medv~lstat*age,data=Boston)
lm(medv~lstat*age,data=Boston)
summary(lm.fit_kryds)

#samme som ovenfor
summary(lm(medv~lstat*age,data=Boston))
lm(medv~lstat*age,data=Boston)

names(Boston)

allekryds2 <- lm(medv~(.)^2,data=Boston)
summary(allekryds2)

# Non-linear Transformations of the Predictors
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
anova(lm.fit2, lm.fit5)


lm.fit5=lm(medv~poly(lstat,5, raw = TRUE))
summary(lm.fit5)
anova(lm.fit2, lm.fit5)


summary(lm(medv~log(rm),data=Boston))

lm.fit.log <- lm(medv~log(rm),data=Boston)
lm.fit.ikkelog <- lm(medv~rm,data=Boston)

par(mfrow=c(1,1))

summary(lm.fit.log)
plot(log(rm), medv, col="red")
abline(lm.fit.log,lwd=3)

summary(lm.fit.ikkelog)
plot(rm, medv, col="red")
abline(lm.fit.ikkelog,lwd=3)



# Qualitative Predictors

?Carseats
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)

names(Carseats)
lmfit_qmed <- lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
lmfit_qikkemed <- lm(Sales~.+Income:Advertising+Price:Age-ShelveLoc,data=Carseats)
anova(lmfit_qmed, lmfit_qikkemed)
#

ISLR2::Credit

lm.fit=lm(Balance~Married,data=Credit)
summary(lm.fit)
contrasts(Credit$Married)

names(Credit)
lm.fit01=lm(Balance~Region ,data=Credit)
summary(lm.fit01)
contrasts(Credit$Region)

lm.fit02=lm(Balance~.,data=Credit)
summary(lm.fit02)

lm.fit03=lm(Balance~.-Region,data=Credit)
summary(lm.fit03)

anova(lm.fit03,lm.fit02)



Advertising <- read_csv("data/Advertising.csv")
Advertising
lm.fit04=lm(sales ~ . -...1 -newspaper, data = Advertising)
summary(lm.fit04)


lm.fit04=lm(sales ~ radio + TV, data = Advertising)
summary(lm.fit04)

lm.fit05=lm(sales ~ .+radio*TV -X1, data = Advertising)
summary(lm.fit05)


names(Credit)
lm.fit07=lm(Balance ~ Income + Income*Student,  data = Credit)
summary(lm.fit07)

lm.fit08=lm(mpg~poly(horsepower,2), data = Auto)
summary(lm.fit08)


minmodel <- lm(Balance ~ 1, data = Credit)
biggest <- formula(lm(Balance ~., data = Credit))
biggest
fwmmodel <- step(minmodel, direction = 'forward', scope = biggest)



devtools::install_github("ekstroem/dataReporter")
 library(dataReporter)
makeDataReport(data=Credit, replace = TRUE, output = "word")

