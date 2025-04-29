# Build the KNN Model
KNN_Model <- train(Species ~ ., 
                   data = TrainingSet, 
                   method = "knn", 
                   preProcess = c("scale", "center"), 
                   tuneGrid = data.frame(k = 3), # Adjust 'k' as needed
                   trControl = trainControl(method = "cv", number = 10))

# Predictions for Training and Testing Sets
KNN_Training <- predict(KNN_Model, TrainingSet)
KNN_Testing <- predict(KNN_Model, TestingSet)

# Confusion Matrices
KNN_Training_Confusion <- confusionMatrix(KNN_Training, TrainingSet$Species)
KNN_Testing_Confusion <- confusionMatrix(KNN_Testing, TestingSet$Species)

# Print Confusion Matrices
print(KNN_Training_Confusion)
print(KNN_Testing_Confusion)

# Metrics
KNN_Training_Metrics <- calculate_metrics(KNN_Training_Confusion)
print("KNN Training Metrics:")
print(KNN_Training_Metrics)

# Visualize Confusion Matrix
KNN_Training_CM_Plot <- visualize_confusion_matrix(KNN_Training_Confusion, "KNN Training Set Confusion Matrix")
KNN_Testing_CM_Plot <- visualize_confusion_matrix(KNN_Testing_Confusion, "KNN Testing Set Confusion Matrix")

print(KNN_Training_CM_Plot)
print(KNN_Testing_CM_Plot)
