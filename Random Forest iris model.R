# Build the Random Forest Model
RF_Model <- train(Species ~ ., 
                  data = TrainingSet, 
                  method = "rf", 
                  preProcess = c("scale", "center"), 
                  trControl = trainControl(method = "cv", number = 10),
                  tuneGrid = expand.grid(mtry = 2)) # Adjust 'mtry' as needed

# Predictions for Training and Testing Sets
RF_Training <- predict(RF_Model, TrainingSet)
RF_Testing <- predict(RF_Model, TestingSet)

# Confusion Matrices
RF_Training_Confusion <- confusionMatrix(RF_Training, TrainingSet$Species)
RF_Testing_Confusion <- confusionMatrix(RF_Testing, TestingSet$Species)

# Print Confusion Matrices
print(RF_Training_Confusion)
print(RF_Testing_Confusion)

# Metrics
RF_Training_Metrics <- calculate_metrics(RF_Training_Confusion)
print("Random Forest Training Metrics:")
print(RF_Training_Metrics)

# Visualize Confusion Matrix
RF_Training_CM_Plot <- visualize_confusion_matrix(RF_Training_Confusion, "Random Forest Training Set Confusion Matrix")
RF_Testing_CM_Plot <- visualize_confusion_matrix(RF_Testing_Confusion, "Random Forest Testing Set Confusion Matrix")

print(RF_Training_CM_Plot)
print(RF_Testing_CM_Plot)
