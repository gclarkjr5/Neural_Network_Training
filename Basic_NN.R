# Clear environment
rm(list = ls())

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# input data to input neurons
# 4x3 matrix
training_input = matrix(
  c(0,0,1,1,1,1,1,0,1,0,1,1),
  nrow = 4,
  ncol = 3)

# Output data
# 1x4 matrix (transpose it to make it 4x1)
training_ouput = t(matrix(
  c(0,1,1,0),
  nrow = 1,
  ncol = 4
))

# create rarndom weights
set.seed(0415)
weights = round(runif(3, min = -1, max = 1), 2)
# turn array into 3x1 matrix
weightsMat = matrix(
  weights,
  nrow = 3,
  ncol = 1
)


# create empty vectors for visualization purposes
outputVec = c()
weightsVec = c()
errorVec = c()
index = c()

# iterate
for(i in 1:10000) {
  index[i] = i # add to index vector
  
  # activation function (sigmoid), X is the dot product between input and weights
  output = 1/(1 + exp(-training_input %*% weightsMat))
  outputVec[i] = output # add to output vector
  
  # calculate the error
  error = training_ouput - output
  errorVec[i] = error
  
  # calculate the gradient (derivative of the activation function)
  sigmoidDeriv = output * (1 - output)
  
  # adjustment is dot T(input) dot (error * gradient)
  adjustment = t(training_input) %*% (error * sigmoidDeriv)
  weightsMat = weightsMat + adjustment
  weightsVec[i] = weightsMat
}

# create data frame to visualize metrics
df = data.frame(index, outputVec, weightsVec, errorVec) %>%
  gather("key", "value", -index)

# visualize progression of 
df %>%
  ggplot(aes(x = index, y = value)) +
  geom_point() +
  facet_grid(key~., scales = "free_y") +
  theme_bw()

  

  