# Artificial Neural Network

# Installing Theano
# pip install --upgrade --no-deps git+git://github.com/Theano/Theano.git

# Installing Tensorflow
# pip install tensorflow

# Installing Keras
# pip install --upgrade keras

#************************************************************************
# Part 1 - Data Preprocessing
#************************************************************************

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('Churn_Modelling.csv')
# Get the Independent Variable Columns from 3:12 but last is +1. X Should be big X
X = dataset.iloc[:, 3:13].values
y = dataset.iloc[:, 13].values

# We have the categorical data so we have to encode them,
# Encoding categorical data
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X_1 = LabelEncoder()
# If we print X, we will get countries we wanted to make it to categories.
X[:, 1] = labelencoder_X_1.fit_transform(X[:, 1])

labelencoder_X_2 = LabelEncoder()
X[:, 2] = labelencoder_X_2.fit_transform(X[:, 2])
onehotencoder = OneHotEncoder(categorical_features = [1])
X = onehotencoder.fit_transform(X).toarray()
X = X[:, 1:]

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

# Feature Scaling ?
# When we have very intensive calculation, we can apply Feature Scaling inorder to ease calculations.
# Formula for feature scaling is X-Mean/standard deviation
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# Output: At this point my data is completely pre-processed.

#************************************************************************
# Part 2 - Now let's make the ANN!
#************************************************************************

# Importing the Keras libraries and packages
import keras
from keras.models import Sequential # To initialize our Neural Network.
from keras.layers import Dense      # That requires to build our ANN.
from keras.layers import Dropout

# Initialising the ANN. (We wanted to make our Sequence of Layers Model)
classifier = Sequential()

# Adding the input layer and the first hidden layer
# Why units = 6. Explanation: It is first Hidden Layer. We have 11 Independent Variables. Formula use 11+1/2 ==> 12/2 ==>6    
# Activation = relu. Explanation: We take Activation function in hidden layer as Rectifier Function and Output as Sigmoid.
# input_dim = 11. Explanation: As we have 11 independent variables.
classifier.add(Dense(units = 6, kernel_initializer = 'uniform', activation = 'relu', input_dim = 11))
#Dropout to be used when you find, the model is too much overfitted. 0.1 means it is going to eliminate 10% of neurons from the current layer. 
# Ifthe overfitting problem does not solve, keep increasing dropout to 0.1 till 0.5 but not go beyonf 0.5 else underftting problem will occur.

# classifier.add(Dropout(p = 0.1))

# Adding the second hidden layer
classifier.add(Dense(units = 6, kernel_initializer = 'uniform', activation = 'relu'))
# Everything remains the same only input_dim will not be used here, because that is only required for 1st Hidden Layer.
# classifier.add(Dropout(p = 0.1))

# Adding the output layer
# Since this is an Output Layer, we wanated to have the Activation Function as Sigmoid.
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))

# Compiling the ANN
# optimizer = The Algorithm you wanna to use to find the optimal set of weights in NN. The algorithm use is Stochastic Gradient Descent. And the best Algo for this is Adam.
# loss      = Loss function within the Adam (Stochastic GD Algo) algo. That is the loss function we wanted to optimize for the optimal weights. binary_crossentropy because we have 2 categories only.
# metrics   = is required to be in list hence we added the values in parenthesis.
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])

# Fitting the ANN to the Training set
# batch_size = Can be understood from the flowchart of training ANN using Stochastic GD. Setp 6. We need to update the weights after each observation (Reinforcement Learning) 
# Or we can update the weights after only batch of observations. Hence we took batch size = 10. which means after 10 observation, we will update the weights.
# epochs = Number of Iterations.
classifier.fit(X_train, y_train, batch_size = 10, epochs = 100)


#************************************************************************
# Part 3 - Making predictions and evaluating the model
#************************************************************************


# Predicting the Test set results
y_pred = classifier.predict(X_test)
y_pred = (y_pred > 0.5)

# Predicting a single new observation
"""Predict if the customer with the following informations will leave the bank:
Geography: France
Credit Score: 600
Gender: Male
Age: 40
Tenure: 3
Balance: 60000
Number of Products: 2
Has Credit Card: Yes
Is Active Member: Yes
Estimated Salary: 50000"""
new_prediction = classifier.predict(sc.transform(np.array([[0.0, 0, 600, 1, 40, 3, 60000, 2, 1, 1, 50000]])))
new_prediction = (new_prediction > 0.5)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

# If we check the Test Prediction of Confusion Matrix we will observe the Accuracy is 86% which is greater than Test Set. 
# Hence we conclude for different sets we have different accuracy.
# Judging the accuracy from One Set does not give good accuracy and it will get changed the moment we change some values.
# To improve this we have a technique call K-Fold Cross Validation..

# Part 4 - Evaluating, Improving and Tuning the ANN

# Evaluating the ANN
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score
from keras.models import Sequential
from keras.layers import Dense
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(units = 6, kernel_initializer = 'uniform', activation = 'relu', input_dim = 11))
    classifier.add(Dense(units = 6, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier, batch_size = 10, epochs = 100)
# cv = We have to do the cross validation 10 times.
# n_jobs will use all the cpu's to build the CV in order to complete the processing fast. But this is not working in my system.
accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = 10, n_jobs = 1)
mean = accuracies.mean()
variance = accuracies.std()

# Improving the ANN
# Dropout Regularization to reduce overfitting if needed

# Tuning the ANN
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from keras.models import Sequential
from keras.layers import Dense
def build_classifier(optimizer):
    classifier = Sequential()
    classifier.add(Dense(units = 6, kernel_initializer = 'uniform', activation = 'relu', input_dim = 11))
    classifier.add(Dense(units = 6, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = optimizer, loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size': [25, 32],
              'epochs': [100, 500],
              'optimizer': ['adam', 'rmsprop']}
grid_search = GridSearchCV(estimator = classifier,
                           param_grid = parameters,
                           scoring = 'accuracy',
                           cv = 10)
grid_search = grid_search.fit(X_train, y_train)
best_parameters = grid_search.best_params_
best_accuracy = grid_search.best_score_