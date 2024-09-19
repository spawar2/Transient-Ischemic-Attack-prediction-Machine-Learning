# Transient Ischemic Attack (TIA) prediction using support vector machines (SVM), Collaboration with Dr. A. Hande, Department of Neurological Surgery, Fortis Hospital, Navi Mumbai. https://www.drashokhande.com/about-us
# Variables examples weight, height, BMI, physical activity, diet, smoking, etc.

# Set working directory
setwd("/Users/pawar/Desktop/K Award Proposal/Dr-Hande-Project")

# Read dataset
data <- read.csv("Final-TIA.csv", header = TRUE, sep = ",")

# Splitting the dataset into the Training set and Test set 
library(caTools)

set.seed(123) 
split = sample.split(data$Gender, SplitRatio = 0.75) 
  
training_set = subset(data, split == TRUE) 
test_set = subset(data, split == FALSE)

training_set = training_set[,c(3, 4, 5, 7)]
training_set = na.omit(training_set)
test_set = test_set[,c(3, 4, 5, 7)]
test_set = na.omit(test_set)

# Feature Scaling 
training_set[-4] = scale(training_set[-4]) 
test_set[-4] = scale(test_set[-4]) 

# Fitting SVM to the Training set 
library(e1071) 
  
classifier = svm(formula = TIA ~ .,data = training_set, type = 'C-classification', kernel = 'linear')

Call:
svm(formula = TIA ~ ., data = training_set, 
    type = "C-classification", kernel = "linear")


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  linear 
       cost:  1 

Number of Support Vectors:  280

# Predicting the Test set results 
y_pred = predict(classifier, newdata = test_set[-4])

# Making the Confusion Matrix 
cm = table(test_set[, 4], y_pred)

   y_pred
      0   1
  0 151   0
  1  41   0
  
# Plotting the test data set results 
model.ksvm = svm(TIA ~ Gender + Weight. + height.in.m, data = training_set)
plot(model.ksvm, training_set)

plot(classifier, training_set, Weight. ~ height.in.m, slice=list(Gender=3))


 
  


