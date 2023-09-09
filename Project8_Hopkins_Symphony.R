#############################################
#                                           #
# Author:     Symphony Hopkins              #
# Date:       05/02/2023                    #
# Subject:    Project 8                     #
# Class:      DSCI 512                      #
# Section:    01W                           #         
# Instructor: Juan David Munoz              #
# File Name:  Project7_Hopkins_Symphony.R   #
#                                           #
#############################################

# 1. Load the dataset wine.csv into memory.
#    Answer: See code.

wine <- read.csv("~/Documents/Maryville_University/DSCI_512/Week_8/wine.csv")
View(wine)

# 2. Preprocess the inputs:
# 2a.Standardize the inputs using the scale() function.
#    Answer: See code.

scaled_wine <- scale(wine)

# 2b.Convert the standardized inputs to a data frame 
#    using the as.data.frame() function.
#    Answer: See code.

scaled_wine <- as.data.frame(scaled_wine)

# 2c.Split the data into a training set containing 3/4 
#    of the original data (test set containing the remaining 
#    1/4 of the original data).
#    Answer: See code.

#setting seed for reproducibility
set.seed(1)

#performing train-test split
index <- sample(1:nrow(scaled_wine), 0.75*nrow(scaled_wine))
train <- scaled_wine[index,]
test <- scaled_wine[-index,]

#3.  Build a neural networks model:
#3a. The response is quality and the inputs are: volatile.acidity, 
#    density, pH, and alcohol. Please use 1 hidden layer with 
#    1 neuron.
#    Answer: See code.

#importing library
library(neuralnet)

#building neural network
wine_nn <- neuralnet(quality ~ volatile.acidity + density + pH + alcohol,
                     data=train, hidden=c(1))

#3b. Plot the neural networks.
#    Answer: See code.

plot(wine_nn)

#3c. Forecast the wine quality in the test dataset.
#    Answer: See code.

wine_nn_pred <- compute(wine_nn, test[, c('volatile.acidity', 'density', 'pH', 'alcohol')])

#3d. Get the observed wine quality of the test dataset.
#    Answer: See code.

obs_test <- test$quality

#3e. Compute test error (MSE).
#    Answer: The MSE is 0.701339.

mean((obs_test-wine_nn_pred$net.result)^2)

#End Assignment




