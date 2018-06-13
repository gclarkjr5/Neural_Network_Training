rm(list = ls())

library(ggplot2)
library(dplyr)

# input neurons
training_input = matrix(
  c(0,0,1,1,1,1,1,0,1,0,1,1),
  nrow = 4,
  ncol = 3)

training_ouput = t(matrix(
  c(0,1,1,0),
  nrow = 1,
  ncol = 4
))

# create rarndom weights
set.seed(0415)
weights = round(runif(3, min = -1, max = 1), 2)
weightsMat = matrix(
  weights,
  nrow = 3,
  ncol = 1
)


# iterate
outputVec = c()
weightsVec = c()
index = c()
for(i in 1:10000) {
  index[i] = i
  output = 1/(1 + exp(-training_input %*% weightsMat))
  outputVec[i] = output
  weightsMat = weightsMat + (t(training_input) %*% ((training_ouput - output) * output * (1- output)))
  weightsVec[i] = weightsMat
}

df = data.frame(index, outputVec, weightsVec)

df %>%
  ggplot(aes(x = index, y = outputVec)) + geom_point()

df %>%
  ggplot(aes(x = index, y = weightsVec)) + geom_point()
  
df %>%
  ggplot(aes(x = outputVec, y = weightsVec)) + geom_point()

  

  