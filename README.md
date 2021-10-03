### Ames Iowa Dataset Predict Property Prices

The data of this assignment refer to the database of the Ames City Assessor’s Office. It includes a large number of variables and observations within the data set and they refer 
to 2930 property sales that had occurred in Ames, Iowa between 2006 and 2010. A mix of 82 nominal, ordinal, continuous, and discrete variables were used in the calculation of the 
assessed values. The dataset included physical property measurements in addition to computation variables used in the city’s assessment process. All variables focus on the 
quality and quantity of many physical attributes of the property. Most of the variables are exactly the type of information that a typical home buyer would want to know about a 
potential property (e.g. When was it built? How big is the lot? How many feet of living space is in the dwelling? Is the basement finished? How many bathrooms are there?). For 
more details see at De Cock (2011). 

The main goal is to compare the predictive performance of multiple regression models and to identify the best model for predicting the prices of the properties. Data cleaning 
and variable transformation were mandatory as the data set includes many missing values with not well – specified variables. The multicollinearity was also a problem as many
attributes were highly correlated. Before the creation of the multiple regression model, categorical variables were converted in dummies. Lasso was used in order to reduce the 
number of columns. Then, the first model according to AIC in the Stepwise procedure was estimated. At this point, we have to refer that only variables with correlation greater
than the absolute value of 0.25 were kept. Based on various visualizations of the variables, we could detect some possible transformations of the response and predictor variables
in order to face the violation of residual’s assumptions of the model. On the whole three models were estimated and based on Leave One Out and 10 – Fold cross validation methods
we kept the model with the minimum RMSE. Finally, a test data set was used to assess the out – of – sample predictive ability of the model.
