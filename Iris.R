### Loading data set ##
#########################

library(datasets)
data("iris")
View(iris)

iris<-datasets::iris

## Display summary statistics
##############################

iris
head(iris,4)
tail(iris,5)


# Summary()
summary(iris)
summary(iris$Petal.Length)


# Check any missing data:
is.na(iris)
sum(is.na(iris))


# skimr() - expands on summary() by providing larger sets of statistics.
install.packages('skimr')
library(skimr)

skim(iris)

# Group by species and then perform skimr
iris%>%
  dplyr::group_by(Species)%>%
  skim()


### Data Vizualization ##
#########################

# Panel plots
plot(iris)
plot(iris, col='red')


# Scatter plot:
plot(iris$Sepal.Width, iris$Sepal.Length, col='red', xlab='Sepal width', ylab='Sepal Length')


# Histogram:
hist(iris$Sepal.Width, col='blue')


# Feature plot:
library('caret')
featurePlot(x=iris[,1:4],
            y=iris$Species,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation='free'),
                        y=list(relation='free')))

set.seed(100)

# Random Splitting of data:
TrainingIndex = createDataPartition(iris$Species,p = 0.8,list = FALSE)
TrainingSet = iris[TrainingIndex,]
TestingSet = iris[-TrainingIndex,]


plot(TrainingSet$Sepal.Width, TrainingSet$Sepal.Length, main = 'TrainingSet', col='red', xlab='Sepal width', ylab='Sepal Length')
plot(TestingSet$Sepal.Width, TestingSet$Sepal.Length, main = 'TestingSet', col='red', xlab='Sepal width', ylab='Sepal Length')



# SVM MODEL:
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")