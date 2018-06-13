# Create a Basic Neural Network in a functional way

# Clear environment
rm(list = ls())

# Load Libraries
library(dplyr)

# Create the initial random weights matrix (3x1)
initializeWeights = function() {
  set.seed(0415)
  weights = round(runif(3, min = -1, max = 1), 2)
  weightsMat = matrix(
    weights,
    nrow = 3,
    ncol = 1
  )
  return(weightsMat)
}

# Activation Function (Sigmoid outputs probability between 0 and 1)
sigmoid = function(x) {
  1/(1 + exp(-x))
}

# Gradient Function - Derivative of the activation function
sigmoidDeriv = function(x) {
  x * (1 - x)
}

# Train function where the NN will iterate using backpropagation until 
# the weights converge to a point where the error is minimized
train = function(training_inputs, training_outputs, numIterations) {
  for(i in 1:numIterations) {
    
    # Get the outputs of the activation function on the inputs & weights
    output = think(training_inputs)
    
    # Caclulate the squared error
    error = (training_outputs - output)^2
    
    # Calculate the Gradients of the outputs times the error
    preCalcAdjustment = output %>%
      sigmoidDeriv() * error
    
    # Weight adjustment = T(inputs) dot (error * gradient)
    adjustment = t(training_inputs) %*% preCalcAdjustment 
    
    # Adjust the weights
    weights = weights + adjustment
  }
  
  # Return the new weights
  return(weights)
}

# Run the sigmoid function over the dot product of inputs and weights
think = function(inputs) {
  inputs %*% weights %>%
    sigmoid()
}


inputs = matrix(
  c(0,0,1,1,1,1,1,0,1,0,1,1),
  nrow = 4,
  ncol = 3)

outputs = t(matrix(
  c(0,1,1,0),
  nrow = 1,
  ncol = 4
))

weights = initializeWeights()
weights = train(inputs, outputs, 50)

test = matrix(
  c(1, 0, 1),
  nrow = 1,
  ncol = 3
)

think(test)
