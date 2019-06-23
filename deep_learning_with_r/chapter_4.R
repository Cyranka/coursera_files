remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/deep_learning_with_r/")
library(keras);library(tidyverse)

# Regularization -----------------------------------------
imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels),c(test_data, test_labels)) %<-%imdb

str(train_data[[1]])
train_labels[[1]]

# Encoding the integer sequence in a binary matrix ------------------------
vectorize_sequences <- function(sequences, dimension = 10000){
    results <- matrix(0,nrow = length(sequences),ncol = dimension)
    for(i in 1:length(sequences)){
        results[i,sequences[[i]]] <- 1
    }
    results
}

##One hot encoding of predictor matrices
x_train <- vectorize_sequences(train_data) ##Notice how large the matrix is
x_test <- vectorize_sequences(test_data)

str(x_train[1,]) ##Check work

##Converting train and test labels
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

# Model with l2 regularization ----------------------------------------------------
model_l2 <- keras_model_sequential() %>%
    layer_dense(units = 16,
                kernel_regularizer = regularizer_l2(0.001),
                activation = "relu",
                input_shape = c(10000)) %>%
    layer_dense(units = 16,
                kernel_regularizer = regularizer_l2(0.001),
                activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

# Compiling the model -----------------------------------------------------
model_l2 %>%
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
    )

# Create validation indices -----------------------------------------------------
val_indices <- 1:10000


x_val <- x_train[val_indices,] ##Subset training set
partial_x_train <- x_train[-val_indices,] ##Create a "partial" training set
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

history <- model_l2 %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20,
    batch_size = 512,
    validation_data = list(x_val, y_val)
)

plot(history)

l2_results <- model_l2 %>% evaluate(x_test, y_test) ##Predict on the test set

l2_results$acc ##Model accuracy

# Model with dropout ------------------------------------------------------
model_dropout <- keras_model_sequential() %>%
    layer_dense(units = 16,
                activation = "relu",
                input_shape = c(10000)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 16,
                activation = "relu") %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = "sigmoid")

model_dropout %>%
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
    )

history_dropout <- model_dropout %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20,
    batch_size = 512,
    validation_data = list(x_val, y_val)
)

dropout_results <- model_dropout %>% evaluate(x_test, y_test)
dropout_results$acc ##Slightly better than regularized model
