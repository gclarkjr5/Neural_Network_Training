rm(list = ls())

library(ggplot2)
library(dplyr)

initializeWeights = function() {
  # create rarndom weights
  set.seed(0415)
  weights = round(runif(3, min = -1, max = 1), 2)
  weightsMat = matrix(
    weights,
    nrow = 3,
    ncol = 1
  )
  return(weightsMat)
}


sigmoid = function(x) {
  1/(1 + exp(-x))
}

sigmoidDeriv = function(x) {
  x * (1 - x)
}

train = function(training_inputs, training_outputs, numIterations) {
  for(i in 1:numIterations) {
    output = think(training_inputs)
    
    error = training_outputs - output
    
    preCalcAdjustment = output %>%
      sigmoidDeriv() * error
    
    adjustment = t(training_inputs) %*% preCalcAdjustment 
    
    weights = weights + adjustment
  }
  return(weights)
}

think = function(inputs) {
  inputs %*% weights %>%
    sigmoid()
}

training_input = matrix(
  c(0,0,1,1,1,1,1,0,1,0,1,1),
  nrow = 4,
  ncol = 3)

inputs = training_input

training_ouput = t(matrix(
  c(0,1,1,0),
  nrow = 1,
  ncol = 4
))

outputs = training_ouput

weights = initializeWeights()
weights = train(inputs, outputs, 100)

test = matrix(
  c(1, 0, 1),
  nrow = 1,
  ncol = 3
)

think(test)
