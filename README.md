# TSE-Thesis-ConflictForecasting

This project contains the replication data and code for the graphs and tables presented in my master thesis "The Power of the People - Accounting for Public Sentiment in Conflict Prediction". [^1]

## Overview 
The paper builds on the extensive dataset of structural variables constructed by Bazzi et al. (2021) and adds a new, theoretically informed measure of public sentiment with considerable within-variation that is used to improve predictions of violence between Colombian Guerrillas and the Colombian military between 2011 and 2015. 
This newly created time-varying variable is rooted in the information sharing model by Berman et al. (2011) and is constructed from the ICEWS event dataset. Employing a Random Forest model, I find that the inclusion of the public sentiment variable outperforms other apporaches in most tested instances. 

## Structure
The 'CODE' folder contains three scripts:
* _Master_script:_ calls the RandomForest script and contains the code needed to replicate the results presented in the paper
   + _RandomForest:_ contains function used to run and evaluate the models. The function includes the following steps:
       1. conducts hyperparameter tuning
       2. splits sample in training and testing set
       3. accounts for imbalancedness of outcome variable
       4. computes the AUC
* _Graphs:_ used to create the graphs presented in the paper


[^1]: The code to clean the raw data is available upon request
