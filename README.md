# Machine Learning Project 
The goal is to develop a classification model to classifiy images from the COREL database.

## Content
* `main.R`: main program which runs that runs the tunning of hyperparameters of the SVM and validatates the results.
* `process_data.R`: processes the raw images and returns the data into `./data/test.RData` and `./data/train.RData`.
* `report.pdf`: A self-contained document of our experimentation.
* `svm.R` Implementation of a grid search using cross validation of a SVM to tune the kernel and `C` parameter of the SVM, the kernel hyperparameters are also searched and evaluated. It prints the results in a folder called `results` where various files are returned with the confusion matrix of the best result, the best hyperparameters for each best result, cross-validation error of the tuning, the validation error, and test error are returned.
* `knn.R` Implementation of a hyperparameter search using cross validation for the `K` for a KNN to solve the classification problem. It returns the validation error and test error.
* `cnn.ipynb`: Implemenation of the Convolutional Neural Network part of the project, implemented in pytorch. The results of the experiment are already present in the network. Advisable to run it with a GPU for the training time.
* `requirements.R`: configuration file used in [Getting Started](#Getting-Started)
* `README.md`: This file.

### About data
The dataset is a subset of 1000 images from the COREL dataset. We selected 100 images from 10 classes. The data has been processed into 2D color histograms flattened to a matrix containing the label of each image class. The data has been split into 90% train data and 10% test data.


## Getting Started
To run this project follow these steps:
* Install the requirements: `Rscript requirements.R`
* Deploy the project: `Rscript main.R`

## Authors
* **Benjamí Parellada**
* **Sígrid Vila**

## License
This project is licensed under the MIT License.

