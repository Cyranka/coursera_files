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


# Encoding the data -------------------------------------------------------
vectorize_sequences <- function(sequences, dimension = 10000){
    results <- matrix(0,nrow = length(sequences),ncol = dimension)
    for(i in 1:length(sequences)){
        results[i,sequences[[i]]] <- 1
    }
    results
}

# One-hot encoding of output labels ---------------------------------------
to_one_hot <- function(labels, dimension = 46){
    ##Create a matrix of outputs
    results <- matrix(0,nrow = length(labels),ncol = dimension)
    for(i in 1:length(labels)){
        results[i,labels[[i]] + 1] <- 1
    }
    return(results)
}

one_hot_train_labels <- to_one_hot(train_labels)           
one_hot_test_labels <- to_one_hot(test_labels)       

# Building the network ----------------------------------------------------
model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 46, activation = "softmax") ##This is the output

# Compiling the model -----------------------------------------------------
model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

# Setting aside a validation set ------------------------------------------
val_indices <- 1:1000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- one_hot_train_labels[val_indices,]
partial_y_train <- one_hot_train_labels[-val_indices,]

# Training the model ------------------------------------------------------
history <- model %>% 
    fit(
        partial_x_train,
        partial_y_train,
        epochs = 20,
        batch_size = 512,
        validation_data = list(x_val, y_val)
    )

plot(history) ##Plot

##Access accuracy metrics: 
#In the validation set, where did we reach maximum accuracy (epoch)
which(history$metrics$val_acc == max(history$metrics$val_acc))

# Train a network over 12 epochs ------------------------------------------
model <-keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 46, activation = "softmax")

model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

history <- model %>% 
    fit(
        partial_x_train,
        partial_y_train,
        epochs = 12,
        batch_size = 512,
        validation_data = list(x_val, y_val)
    )

results <- model %>% evaluate(x_test, one_hot_test_labels)
results$loss
results$acc ##77% of accuracy: not similar to the book


# Generating predictions on new data --------------------------------------
predictions <- model %>% predict(x_test)

# Creating a model with 128 hidden units and 20 epochs ----------------------------------
model_128 <-keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu", input_shape = c(10000)) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = 46, activation = "softmax")

model_128 %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

history <- model_128 %>% 
    fit(
        partial_x_train,
        partial_y_train,
        epochs = 20,
        batch_size = 512,
        validation_data = list(x_val, y_val)
    )

which(history$metrics$val_acc == max(history$metrics$val_acc))
results <- model_128 %>% evaluate(x_test, one_hot_test_labels)

results$acc 

# Predicting housing prices -----------------------------------------------
remove(list =ls())

dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

str(train_data)
str(test_data)

# Normalizing the data ----------------------------------------------------
mean <- apply(train_data, 2, mean) ##applying over train data
std <- apply(train_data, 2, sd)

train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

# Define model function ---------------------------------------------------
build_model <- function(){
    model <- keras_model_sequential() %>%
        layer_dense(units = 64,
                    activation = "relu",
                    input_shape = dim(train_data)[[2]]) %>%
        layer_dense(units = 64,
                    activation = "relu") %>%
        layer_dense(units = 1)
    
    model %>% compile(
        optimizer = "rmsprop",
        loss = "mse", ##Mean squared error is the loss function
        metrics = c("mae")
    )
}

# K-fold validation implementation ----------------------------------------
set.seed(10)

k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 100
all_scores <- c()

for(i in 1:k){
    cat("processing fold #", i, "\n")
    
    val_indices <- which(folds == i, arr.ind = TRUE) ##Access indices
    val_data <- train_data[val_indices,] ##Validation data
    val_targets <- train_targets[val_indices] ##Validation labels
    
    partial_train_data <- train_data[-val_indices,]
    partial_train_targets <- train_targets[-val_indices]
    
    model <- build_model()
    
    model %>% fit(partial_train_data,
                  partial_train_targets,
                  epochs = num_epochs,
                  batch_size = 1,
                  verbose = 1)
    
    results <- model %>% evaluate(val_data,
                                  val_targets,
                                  verbose = 1)
    all_scores <- c(all_scores, results$mean_absolute_error)
}

all_scores ##
mean(all_scores)

# Increase the number of epochs -------------------------------------------
num_epochs <- 250 ##Doing 250 so it does not take forever
all_mae_histories <- NULL

for(i in 1:k){
    cat("processing fold #", i, "\n")
    
    val_indices <- which(folds == i, arr.ind = TRUE) ##Access indices
    val_data <- train_data[val_indices,] ##Validation data
    val_targets <- train_targets[val_indices] ##Validation labels
    
    partial_train_data <- train_data[-val_indices,]
    partial_train_targets <- train_targets[-val_indices]
    
    model <- build_model()
    
    history <- model %>% 
              fit(partial_train_data,
                  partial_train_targets,
                  validation_data = list(val_data, val_targets),
                  epochs = num_epochs,
                  batch_size = 1,
                  verbose = 1)
    
    mae_history <- history$metrics$val_mean_absolute_error
    all_mae_histories <- rbind(all_mae_histories, mae_history)
}

average_mae_history <- data_frame(
    epoch = seq(1:ncol(all_mae_histories)),
    validation_mae = apply(all_mae_histories, 2, mean)
)

ggplot(average_mae_history,
       aes(x = epoch,
           y= validation_mae)) + 
    geom_line()

ggplot(average_mae_history,
       aes(x = epoch,
           y= validation_mae)) + ##Adjusting y-axis
    geom_line(alpha = 0.6) + 
    geom_smooth(se = FALSE, size = 1)

average_mae_history %>%
    filter(validation_mae == min(validation_mae))


# Train a simpler model ---------------------------------------------------
model <- build_model()
model %>% fit(train_data,
              train_targets,
              epochs = 39,
              batch_size = 16,
              verbose = 1)

result <- model %>% evaluate(test_data, test_targets)

result$mean_absolute_error 