# Logistic regression is one of the most simple
# and effective algorithms to solve
# classification problems

# Classification problems are problems
# where we want to classify new data as part
# of n classes.

# In our example, we will look at a fraud dataset
# and try to map fraudulent transactions based
# on amount and hour of the transaction

#=======================================

# Let's load the ggplot library first
library(ggplot2)
library(readxl)

# Let's start by loading our data

fraud_data <- read_xlsx(
  './data/Fraud Dataset.xlsx'
  )

p <- ggplot(
  fraud_data,
  aes(x=Hour, y=Fraud, color=factor(Fraud))
) + geom_point()

p

# By looking at a plot there might be some line that
# will divide our fraudulent and non-fraudulent
# transactions

# Is this solved using a linear equation as we've
# done before?

# Not quite! Let's see why:

# Starting with building the same intuition
# calculating a linear model to our 

lm(Fraud ~ Hour, data=fraud_data)

# The coefficients from the lm are: 
b0 = 0.94678
b1 = -0.05309

# Let's try to predict new examples:
new_data = data.frame(
  hour = c(0, 4, 8, 12, 16, 20, 24)
)

# The predicted value for the fraud is extremely strange
new_data$Fraud = (
  b0+new_data$hour*b1
)

# If we plot a line with the prediction:
p + geom_line(
  data=new_data, 
  aes(x=hour,y=Fraud, group=1)
) + theme(legend.position="none")

# We now have some fraud prediction
# that is negative.

# Our problem is a classification problem so there
# the concept of "negative" fraud does not exist

# This is where we will introduce the concept
# of the sigmoid function - this function will
# help us to create a function that will help us to
# create a boundary between values of 0 and 1 and
# will "approximate" our result into probability


new_data$Fraud = (
  1/(1+exp(-(b0+new_data$hour*b1)))
)

p + geom_line(
  data=new_data, 
  aes(x=hour,y=Fraud, group=1)
) + theme(legend.position="none")

# Our model doesn't seem that good because
# the linear error optimization is not the best one
# for classification problems

glm(Fraud ~ Hour, data=fraud_data, family=binomial)

# We have obtained new coefficients: 
b0 = 6.110
b1 = -0.846

new_data$Fraud = (
  1/(1+exp(-(b0+new_data$hour*b1)))
)

p + geom_line(
  data=new_data, 
  aes(x=hour,y=Fraud, group=1)
) + theme(legend.position="none")

logistic_reg <- glm(
  Fraud ~ Hour, data=fraud_data, family=binomial
  )

summary(logistic_reg)

# How to you evaluate your regression if we have
# no R-Squared? 

# We will learn more metrics for evaluation in subsequent
# sections and here we will look at a simple one: 
# Accuracy

# Let's classify our original table examples
fraud_data$predicted_fraud <- (
  1/(1+exp(-(b0+fraud_data$Hour*b1)))
)

# Based on the prediction, if the
# predicted value for fraud is higher than 0.5
# we will classify it as fraud
fraud_data$predicted_fraud_binary <- (
  ifelse(fraud_data$predicted_fraud > 0.5, 1, 0)
)

# How many examples did we got right?
sum(fraud_data$predicted_fraud_binary==fraud_data$Fraud)

# We can now divide the total examples we got correct
# with our model and divide by the total number of transactions
correct_examples <- (
  sum(fraud_data$predicted_fraud_binary==fraud_data$Fraud)
)

correct_examples/nrow(fraud_data)

# We have around 98% of accuracy in our model! 98% of the
# transactions were correctly flagged.



