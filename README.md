# TIME_SERIES_ANALYSIS
A comparison between different models, ensembles and their accuracy on seasonal and non seasonal time series 

# Algorithms used
The fact that time series has only a single feature given against a time line makes the time series analysis different from normal regression. For regression the number of values to be predicted never becomes a problem. In regression algorithms the focus is to learn the variations of target feature with respect to the available training features rather than the result of last point. This makes regression algorithms better for shorter predictions. We have used three types of algorithms and techniques. Standard time series forecasting algorithms ARIMA, Holt winter’s and SSA. Regression algorithms GLM net (Lasso and Ridge regression), Random Forest, XGboost and SVM. Neural networks RNN (LSTMs). After applying all these algorithms we have taken the ensamble (weighted mean) of all the trained models.
I have used R for implementing the machine learning algorithms except for RNN which we have implemented using keras in python.
