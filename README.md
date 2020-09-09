# EY NextWave Data Science Competition 2019

This project aims to predict whether a person will enter a city center between a specific time period. The preduiction is intended to provide insights on the behaviour of citizens in order to construct better infrastructure and improve city planning.

The project involves a lot of exploatory data anaylsis and experiments with varies classification models, including GLM, lasso, ridge, random forest. Finally, the best model was found to be a similation approach using empirical distribution.

## Data

The data are provided are in a form of GPS tracking data mobile phones. The data includes a few coordiates and timestamps for each person, which intends to summerize the movement behaviour of that person in a day. The objective of the model is to predict the whether the datapoint after the last provided one will be inside the city center.

The data is not uploaded due to copyrights.

## Methodology

The best model uses a similation approch to predict the probablility of the person entering the city center. The movement each person is broken down into direction and speed, which are sampled from conditional empiracal distributions generated using the training data. Other factors, for example initial start point, time travelled, number of records tracks, are also considered when generating the distributions. Finally, a cutoff probablity is set to balance sensitivity and specificity for a higher F1 score.

## Result

The best model produces a test F1 score of 0.86323, which gave me a rank of 13th in Hong Kong.
