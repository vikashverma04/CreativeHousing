# CreativeHousing

Problem Description:  
Predicting the house prices with the help of multiple features such as built-up area, No of rooms, floors, condition, year of built etc. 
Check data_description.JPG for features invovlved as dependent and independent variables.

Below are the feature engineering and modeling techniques involved for the prediction. 

* Missing data imputation (Used Mice library, Method - CART).
* Outlier detection and resolution.
* Checking normal distribution and skewness. Data Transformation.
* Linear regression model to predict the dependent variable (Price).
* Building various models including Polynomial regression.
* Analyzing summary and accuracy using various plots to check residuals.



![Alt text](/data_description.JPG?raw=true "data description")


##### R libraries used/explored in the analysis :

library(VIM),
library(mice),
library(missForest)

Factor Analysis lib : 

library(psych),
library(psy),
library(GPArotation),
library(stats),
library(FactoMineR)

regression libs :

library(Metrics),
library(lattice),
library(corpcor),
library(ggplot2),
library(car),
library(moments)

