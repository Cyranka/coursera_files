remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/deep_learning_with_r/")
library(keras);library(tidyverse)


# Loading the IMDB dataset ------------------------------------------------
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

# Building the network ----------------------------------------------------
model <- keras_model_sequential() %>%
    layer_dense(units = 16,activation = "relu", input_shape = c(10000)) %>%
    layer_dense(units = 16,activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")


# Compiling the model -----------------------------------------------------
model %>%
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
    )

# Validating approach -----------------------------------------------------
val_indices <- 1:10000

x_val <- x_train[val_indices,] ##Subset training set
partial_x_train <- x_train[-val_indices,] ##Create a "partial" training set
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

# Model training ----------------------------------------------------------
history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20,
    batch_size = 512,
    validation_data = list(x_val, y_val)
)

str(history) ##Check
plot(history)

# Model training 2: only four epochs --------------------------------------
model <- keras_model_sequential() %>%
    layer_dense(units = 16,activation = "relu",input_shape = c(10000)) %>%
    layer_dense(units = 16,activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>% 
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
    )

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test) ##Predict on the test set

results$loss ##Total loss
results$acc ##Model accuracy

# Predictions on new data -------------------------------------------------
model %>% predict(x_test[1:10,]) ##Prediction of being sample being positive

# Classifying newswires ---------------------------------------------------
library(reticulate)
np <- import("numpy")
reuters <- np$load("reuters.npz", allow_pickle = TRUE)
text_data <- reuters$f[["x"]] 
labels <- reuters$f[["y"]]

# Find index of top words -------------------------------------------------
first <- bind_rows(lapply(text_data, function(i)as_tibble(i)))
top_10000 <- count(first, value, sort = TRUE) %>% slice(1:10000)
remove(first, top_10000)
##Filter data
new_text <- lapply(text_data, function(i)i[i<=10000])

# Split -------------------------------------------------------------------
train_data <- new_text[1:8982]
test_data <- new_text[8983:11128]

train_labels <- labels[1:8982]
test_labels <- labels[8983:11128]

vectorize_sequences <- function(sequences, dimension = 10000){
    results <- matrix(0,nrow = length(sequences),ncol = dimension)
    for(i in 1:length(sequences)){
        results[i,sequences[[i]]] <- 1
    }
    results
}

x_train <- vectorize_sequences(train_data) ##Notice how large the matrix is
x_test <- vectorize_sequences(test_data)


