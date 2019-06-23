remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/deep_learning_with_r/")
library(keras);library(tidyverse)

# Instantiating a small convnet -------------------------------------------
model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32,
                  kernel_size = c(3,3), ##Size of the 'square' extracted
                  activation = "relu",
                  input_shape = c(28,28,1)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 64,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 64,
                  kernel_size = c(3,3),
                  activation = "relu")

# Adding a classifier on top of the convnet -------------------------------
model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 64,
                activation = "relu") %>%
    layer_dense(units = 10,
                activation = "softmax")

# Training the convnet on MNIST images ------------------------------------
mnist <- dataset_mnist()
c(c(train_images, train_labels),c(test_images,test_labels)) %<-% mnist

train_images <- array_reshape(train_images, c(60000,28,28,1))
train_images <- train_images/255

test_images <- array_reshape(test_images, c(10000,28,28,1))
test_images <- test_images/255

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

model %>%
    compile(
        optimizer = "rmsprop",
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )

model %>% 
    fit(train_images,
        train_labels,
        epochs = 5,
        batch_size = 64)


# Checking results --------------------------------------------------------
results <- model %>% evaluate(test_images,test_labels)
results$loss
results$acc

