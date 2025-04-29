#Importing libraries
library(caret)
library(ggplot2)
#Package for machine algorithms

#importing the iris dataset
irs<-datasets::iris
irs<-data.frame(irs)

#missing values
sum(is.na(irs))

#apply column transformation
#Built a function
col_names_transformation<-function(x){
  new_names =names(x)
  new_names =gsub("\\.","",new_names)

  names(x)=new_names
  return(x)
} 

irs<-col_names_transaformation(irs)

#to archive reproducible model;set the random seed number
set.seed(189)  

#perform stratified random split of the data set
TrainingIndex<-createDataPartition(iris$Species,p=0.8,list=FALSE)
TrainingSet<-irs[TrainingIndex,]#Training Set
TestingSet<-irs[-TrainingIndex,]#Test Set

#Compare scatter plot of the 80 and 20 data subsets

# Scatter plot for Training Set
#TrainingPlot <- ggplot(TrainingSet, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#  geom_point() +
 # labs(title = "Scatter Plot: Training Set (80%)")

# Scatter plot for Testing Set
#TestingPlot <- ggplot(TestingSet, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  #geom_point() +
  #labs(title = "Scatter Plot: Testing Set (20%)")

# Display the plots
#print(TrainingPlot)
#print(TestingPlot)


###########################

#SVM Model(polynomial hernel)

#Build Training Model
Model<-train(Species ~ .,data =TrainingSet
             ,method = "svmPoly",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trControl= trainControl(method="none"),
             tuneGrid = data.frame(degree=1,scale=1,C=1)
             )

#Build CV Model
Model.cv<-train(Species~.,data=TrainingSet,
                method="svmPoly",
                na.action=na.omit,
                preProcess=c("scale","center"),
                trControl=trainControl(method = "cv",number = 10),
                tuneGrid =data.frame(degree=1,scale=1,C=1))

#Apply model for prediction
Model.training<-predict(Model,TrainingSet)#Apply model to make prediction on training set
Model.testing<-predict(Model,TestingSet)#Apply model to make prediction on the testing set 
Model.cv<-predict(Model.cv,TrainingSet)#perform cross validation

#Model performance(Displays confusion matrix and statistics)
Model.training.confusion<-confusionMatrix(Model.training,TrainingSet$Species)
Model.testing.confusion<-confusionMatrix(Model.testing,TestingSet$Species)
Model.cv.confusion<-confusionMatrix(Model.cv,TrainingSet$Species)




print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

#Calculate additional metrics:Accuracy,Precision,Recall,F1 score
calculate_metrics<-function(conf_matrix){
  round(accuracy<-conf_matrix$overall["Accuracy"],2)
  round(precision<-mean(Model.training.confusion$byClass[,"Precision"],2))
  round(recall<-min(Model.training.confusion$byClass[,"Recall"],2))
  f1_score<-2 *(precision * recall)/(precision +recall)
  
  metrics<-data.frame(
    Metric = c("Accuracy","Precision","Recall","F1 Score"),
    Value = c(accuracy,precision,recall,f1_score)
  )
 return(metrics) 
}

#Training metrics
training_metrics<-calculate_metrics(Model.training.confusion)
print("Training Metrics:")
print(training_metrics)

#visualize the confuson matrix
visualize_confusion_matrix<-function(conf_matrix,title){
  cm<-as.data.frame(conf_matrix$table)
  colnames(cm)<-c("Actual","Prediction","Freq")
  
  ggplot(cm,aes(x = Prediction,y = Actual))+
    geom_tile(aes(fill = Freq),color = "white")+
    scale_fill_gradient(low = "white",high = "blue")+
    geom_text(aes(label = Freq),vjust = 1)+
    ggtitle(title)+
    theme_minimal()
}

#plot confusion metrics
plot_training_cm<-visualize_confusion_matrix(Model.training.confusion,
                                             "Training Set Confusion Matrix")
plot_testing_cm<-visualize_confusion_matrix(Model.testing.confusion,
                                            "Testing Set Confusion Matrix")

print(plot_training_cm)
print(plot_testing_cm)

#Feature importance 
#Importance<-varImp(Model)
#plot(Importance)
#plot(Importance,col="red")

#ASSIGNMENT:We have used SVM,now 
#Mode of submission-Create a new model_nameofyourmodelalgorithm.R file and build there for each model algorithm
#to be submitted on monday!!!





















