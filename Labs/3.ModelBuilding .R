##########################
# Data & ME
#Adapted from https://philippbroniecki.github.io/statistics1/
##########################

#Set working directory and delete everything from the workspace

rm(list = ls())


### 6.1.1 Packages

install.packages("texreg") # installs package into your R - only once
library(texreg) # load in the beginning of every R session

# explore the relationship between the unemployment rate and low education
dat <- read.csv("https://raw.githubusercontent.com/philippbroniecki/statistics1/master/data/communities.csv")

# rename the variable names as they are not clear in the current dataset

names(dat)
names(dat)[which(names(dat) == "PctUnemployed")] <- "UnemploymentRate"

# rename yourself PctNotHSGrad to NoHighSchool
*******

# explore summary statistics of the variables of interest

summary(dat$UnemploymentRate)

# summary of NoHighSchool
******

# for the ease of interpretation, convert the variables into percentages

dat$UnemploymentRate <- dat$UnemploymentRate*100
dat$NoHighSchool <- dat$NoHighSchool*100

# draw a scatterplot with the percentage of unemployed people on the y-axis 
# and the percentage of adults without high-school education on the x-axis

plot(
  y = *****,
  x = *****, 
  xlab = "Adults without High School education (%)",
  ylab = "Unemployment (%)",
  bty = "n",
  pch = 16,
  col = rgb(red = 110, green = 200, blue = 110, alpha = 80, maxColorValue = 255)
)

# to empirically assess the relationship we see in the scatterplot run a linear regression using the lm() function

model1 <- lm(UnemploymentRate ~ NoHighSchool, data = dat)
summary(model1)  #shows us the summary information about the linear model we have just created


### 6.1.1.1 Interpreting Regression Output
### for a detailed description of the regression output see the github script

# add a regression line to our scatter plot - abline()

# first run the same "plot" function as before 
plot(
  ********
  xlab = "Adults without High School education (%)",
  ylab = "Unemployment (%)",
  frame.plot = FALSE,
  pch = 16,
  col = rgb(red = 110, green = 200, blue = 110, alpha = 80, maxColorValue = 255)
)

# then use the "abline" function to plot the regression line from our saved model object
abline(model1, lwd = 3,
       col = rgb(red = 230, green = 150, blue = 0, alpha = 255, maxColorValue = 255))
    
    # -> is the regression line good match to our data?
    # -> does the visualisation of the regression line correspond to our estimated coefficients?


# summary() function gaves us information about our regression model but not in a very user-firendly way
# use the screenreg() function from the texreg package we installed earlier to improve this

screenreg(model1)

  # -> output includes the most salient details we need for interpretation
  # -> variable and intercept coefficients, standard errors, R^2, the adjusted R^2, the number of observations (n) and the root-mean-squared-error (RMSE).
  # -> however, NO t-statistics or p-values for the coefficents. 
  # -> Instead, stars denote whether a variable is statistically significant at a given alpha level.

# what other factors could impact unemployment rate?
# explore effect of population variable = the proportion of adults who live in cities

dat$population <- dat$population*100
model2 <- lm(***************)
summary(*******)

# visualise your model

plot(
  ******************,
  xlab = "Adults living in cities (%)",
  ylab = "unemployment (%)",
  frame.plot = FALSE,
  pch = 16,
  col = rgb(red = 110, green = 200, blue = 110, alpha = 100, maxColorValue = 255)
)
abline(model2, lwd = 2,
       col = rgb(red = 230, green = 150, blue = 0, alpha = 255, maxColorValue = 255))

# display the results of both our models

screenreg(list(model1, model2))

# save the output as a Microsoft Word document

htmlreg(list(model1, model2), file = "Regressions_on_Unemployment.doc")


### 6.1.2 Fitted values
### To calculate fitted values use the predict() function.
### This will find the value of our dependent variable for a specific value of our independent variable

# what is the unemployment rate for a community with 10% of adults without a high-school education?

predict(model1, newdata = data.frame(NoHighSchool = 10))


############################# Folks that's all for today! ################################





