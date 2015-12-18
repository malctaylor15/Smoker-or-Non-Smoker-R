## Malcolm Taylor 

## You may need to install packages initialize them 

## install.packages("haven")
## install.packages("boot")
library(haven)
library(boot)

# Remember to change directory to where the file is 
smoking <- read_dta("C:/Users/board/Documents/GitHub/Statistics-420-Fall-2015/Smoking Predictors Project/Smoking.dta")

## Number of observations
n = length(smoking$smoker)

## Vector that will contain the CV estimates 
CV_est = rep(1,6)

## Fit Basic Model with the predictors 
basic_fit <- glm(smoker~., family = "binomial", data = smoking)
summary(basic_fit)

## Converts the Betas from log odds to probability 
round((exp(basic_fit$coefficients)/(1+exp(basic_fit$coefficients))),4)

## Cross Validation for Huge Model
CV_est[1] <- cv.glm(smoking,basic_fit, K=10)$delta[1]

## Narrow Basic Model with backward BIC
mod_basic <- step(basic_fit,direction = "backward", trace = 0, k = log(n))
summary(mod_basic)

## Converts the Betas from log odds to probability 
round((exp(mod_basic$coefficients)/(1+exp(mod_basic$coefficients))),4)

## Cross Validation for Modified Basic Model 
CV_est[2] <- cv.glm(smoking, mod_basic, K=10)$delta[1]

##__ Repeat this process for the three models (huge and basic-education)

## Huge Model with all of the interaction terms  
huge_fit <- glm(smoker~.*., family = "binomial", data = smoking)
summary(huge_fit)

## Converts the Betas from log odds to probability 
round((exp(huge_fit$coefficients)/(1+exp(huge_fit$coefficients))),4)

## Cross Validation for Huge Model
CV_est[3] <- cv.glm(smoking,huge_fit, K=10)$delta[1]

## Narrow Huge Model with backward BIC 
mod_huge <- step(huge_fit,direction = "backward", trace = 0, k = log(n))
summary(mod_huge)

## Converts the Betas from log odds to probability 
round((exp(mod_huge$coefficients)/(1+exp(mod_huge$coefficients))),4)

## Cross Validation for Modified Huge Model
CV_est[4] <- cv.glm(smoking,mod_huge, K=10)$delta[1]

## ___ End of 2 Start 3 

## Basic Minus Education 
basic_edu_fit <- glm(smoker~. -hsdrop -hsgrad -colsome - colgrad, family = "binomial", data = smoking)
summary(basic_edu_fit)

## Converts the Betas from log odds to probability 
round((exp(basic_edu_fit$coefficients)/(1+exp(basic_edu_fit$coefficients))),4)

## Cross Validation for Basic Minus Education Model
CV_est[5] <- cv.glm(smoking,basic_edu_fit, K=10)$delta[1]

## Narrow Basic Minus Education Model using backward BIC 
mod_basic_edu <- step(basic_edu_fit, direction = "backward", trace = 0, k = log(n))
summary(mod_basic_edu)

## Converts the Betas from log odds to probability 
round((exp(mod_basic_edu$coefficients)/(1+exp(mod_basic_edu$coefficients))),4)

## Cross Validation for Modified Basic Minus Education
CV_est[6] <- cv.glm(smoking,huge_fit, K=10)$delta[1]


## Summary for the cross validation results
round(CV_est,4)
## Range for the CV estimates 
range(CV_est)[1]-range(CV_est)[2]
## The predictive capabilities of the models and finding the best model
pred <- 1- CV_est
which.max(pred)
## The best model corresponds to the modified huge model however estimates are fairly close and all the models will most likely be good predictors 

## Predict the likelihood of a person being a smoker given data about them 
predict(mod_huge, data.frame(smkban = 1, age = 40, hsdrop = 1, 
                             hsgrad = 0, colsome = 0, colgrad = 0, 
                             black = 1, hispanic = 0, female = 1), type = "response")
