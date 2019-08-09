#### Auto-MPG predictions using regression  ####
autos <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data", header=FALSE, sep="", as.is=TRUE)

head(autos, 5)   # get a peek at the first five rows of data
tails (autos, 5)   # get a peek at the last five rows of data
summary(autos)   # look at the summary of the data
str(data)   # take a look at the structure of the data

sum(autos["V4"] == "?")   # find the total of missing values from column "horsepower" It is 6.
colnames(autos) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "modelYear", "origin", "carName") #rename the columns
autos$horsepower <- as.numeric(autos$horsepower)   # change the data type of horsepower to numeric
sum(is.na(autos["horsepower"]))   # confirm that the six missing values were converted to na
autos$horsepower[is.na(autos$horsepower)] <- mean(autos$horsepower, na.rm-TRUE)   # replace missing values with the mean of the entire column
sum(is.na(autos["horsepower"]))   # verify that the operation worked and there are no missing values.
autos$origin <- factor(autos$origin)   # change the following three attributes to factors
autos$modelYear <- factor(autos$modelYear)
autos$cylinders <- factor(autos$cylinders)
autos$carName <- NULL   # remove this attribute
str(autos)   # view the prepared data

trainSize <- round(nrows(autos) * 0.7)   # calculate and store the sizes of each set
testSize <- nrow(autos) - trainSize

trainSize   # check that the output is 279
testSize   # check that the output is 119
set.seed(123)   # make a training dataset and a test dataset that are truly representative of the entire set
training_indices <- sample(seq_len(nrows(autos)), size=trainSize)
trainSet <- autos[training_indices, ]
testSet <- autos[-training_indices, ]

model <- lm(formula=trainSet$mpg ~ , / data=trainSet)   # create the linear regression model

summary(model)

predictions <- predict(model, testSet, interval="predict", level-.95)

head(predictions)

comparison <- cbind(testSet$mpg, predictions[,1])   # create a two-column matrix with the acutal and predicted values
colNames(comparison) <-c("actual", "predicted")   # change the column names to actual and predicted
summary(comparison)

# use the mean absolute percent error (mape) to measure the accuracy of the regression model
mape <- (sum(abs(comparison[,1]-comparison[,2]/ abs(comparison[,1]))/nrow(comparison))*100
mape   # should be around 10.93689

# view the results and errors in a table view
mapeTable <- cbind(comparison[,1]- comparision[,2])/comparison[,1]*100)
colnames(mapeTable)[3] <- "absolute percent error"
head(mapeTable)

sum(mapeTable[,3])/nrow(comparison)   # should be around 10.93689

# make predictions with new data
newPrediction <- predict(model, list(cylinders=factor(4), displacement=370, horsepower=150, weight=3904, acceleration=12, modelYear=factor(70), origin=factor(1)), interval="predict", level=.95)
newPrediction
