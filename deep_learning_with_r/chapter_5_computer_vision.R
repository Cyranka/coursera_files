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


# Preparing training and test data ----------------------------------------
# train_dir <- file.path("cats_dogs_small/", "train")
# dir.create(train_dir)
# validation_dir <- file.path("cats_dogs_small/", "validation")
# dir.create(validation_dir)
# test_dir <- file.path("cats_dogs_small/","test")
# dir.create(test_dir)
# 
# ##Create training sets
# train_cats_dir <- file.path(train_dir, "cats")
# dir.create(train_cats_dir)
# train_dogs_dir <- file.path(train_dir, "dogs")
# dir.create(train_dogs_dir)
# 
# ##Create validation sets
# validation_cats_dir <- file.path(validation_dir, "cats")
# dir.create(validation_cats_dir)
# validation_dogs_dir <- file.path(validation_dir, "dogs")
# dir.create(validation_dogs_dir)
# 
# ##Create test sets
# test_cats_dir <- file.path(test_dir,"cats")
# dir.create(test_cats_dir)
# test_dogs_dir <- file.path(test_dir, "dogs")
# dir.create(test_dogs_dir)
# 
# ##Copy files
# #cats
# fnames <- paste0("cat.", 1:1000,".jpg")
# file.copy(file.path("dogs-vs-cats/train/", fnames),
#           file.path(train_cats_dir))
# 
# fnames <- paste0("cat.", 1001:1500,".jpg")
# file.copy(file.path("dogs-vs-cats/train/", fnames),
#           file.path(validation_cats_dir))
# 
# fnames <- paste0("cat.", 1501:2000,".jpg")
# file.copy(file.path("dogs-vs-cats/train/", fnames),
#           file.path(test_cats_dir))
# 
# #dogs
# fnames <- paste0("dog.", 1:1000,".jpg")
# file.copy(file.path("dogs-vs-cats/train/", fnames),
#           file.path(train_dogs_dir))
# 
# fnames <- paste0("dog.", 1001:1500,".jpg")
# file.copy(file.path("dogs-vs-cats/train/", fnames),
#           file.path(validation_dogs_dir))
# 
# fnames <- paste0("dog.", 1501:2000,".jpg")
# file.copy(file.path("dogs-vs-cats/train/", fnames),
#           file.path(test_dogs_dir))

# Instantiating convnet for dogs-versus-cats classification ---------------
model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32,
                  kernel_size = c(3,3),
                  activation = "relu",
                  input_shape = c(150,150,3)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 64,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 128,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 128,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_flatten() %>%
    layer_dense(units = 512, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")


# Configuring the model for training --------------------------------------
model %>%
    compile(
        loss ="binary_crossentropy",
        optimizer = optimizer_rmsprop(lr =0.0001),
        metrics = c("acc") ## Setting accuracy as the metric
    )

train_datagen <- image_data_generator(rescale = 1/255)
validation_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
    train_dir, ##Tell where the working directory is
    train_datagen, ##Pass the rescale generator
    target_size = c(150,150), ##Compress image size
    batch_size = 20, ##Each batch
    class_mode = "binary" ##Class mode binary
)

validation_generator <- flow_images_from_directory(
    validation_dir,
    validation_datagen,
    target_size = c(150,150),
    batch_size = 20,
    class_mode = "binary"
)

batch <- generator_next(train_generator)
str(batch) ##20 instances of a 3d tensor of dimensions 150x150x3

# Fitting the model using a batch generator -------------------------------
history <- model %>% fit_generator(
    train_generator,
    steps_per_epoch = 100,
    epochs = 30,
    validation_data = validation_generator,
    validation_steps = 50
)

model %>% save_model_hdf5("cats_and_dogs_small_1.h5")

plot(history)

# Setting up a data augmentation configuration ----------------------------
datagen <- image_data_generator(
    rescale = 1/255,
    rotation_range = 40,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    shear_range = 0.2,
    zoom_range = 0.2,
    horizontal_flip = TRUE, 
    fill_mode = "nearest"
)

# Defining a new convnet that includes dropout ----------------------------
model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32,
                  kernel_size = c(3,3),
                  activation = "relu",
                  input_shape = c(150,150,3)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 64,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 128,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 128,
                  kernel_size = c(3,3),
                  activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_flatten() %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 512, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>%
    compile(
        loss ="binary_crossentropy",
        optimizer = optimizer_rmsprop(lr =0.0001),
        metrics = c("acc") ## Setting accuracy as the metric
    )

test_datagen <- image_data_generator(rescale = 1/255)
train_generator <- flow_images_from_directory(
    train_dir,
    datagen,
    target_size = c(150,150),
    batch_size = 32,
    class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
    validation_dir,
    test_datagen,
    target_size = c(150,150),
    batch_size = 32,
    class_mode = "binary"
)


history <- model %>% fit_generator(
    train_generator,
    steps_per_epoch = 100,
    epochs = 35,
    validation_data = validation_generator,
    validation_steps = 50
)

model %>% save_model_hdf5("cats_and_dogs_small_2.h5")

# Using a pretrained model ------------------------------------------------
# Instantiating the VGG16 convolutional base ------------------------------
conv_base <- application_vgg16(
    weights = "imagenet",
    include_top = FALSE,
    input_shape = c(150,150,3)
)

conv_base

# Extracting features using the pretrained convolutional base -------------
base_dir <-  "cats_dogs_small/"
train_dir <- file.path(base_dir, "train")
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")

datagen <- image_data_generator(rescale = 1/255)
batch_size <- 20

extract_features <- function(directory, sample_count) {
    features <- array(0, dim = c(sample_count, 4, 4, 512))
    labels <- array(0, dim = c(sample_count))
    generator <- flow_images_from_directory(
        directory = directory,
        generator = datagen,
        target_size = c(150, 150),
        batch_size = batch_size,
        class_mode = "binary"
    )
    i <- 0
    while(TRUE) {
        batch <- generator_next(generator)
        inputs_batch <- batch[[1]]
        labels_batch <- batch[[2]]
        features_batch <- conv_base %>% predict(inputs_batch)
        index_range <- ((i * batch_size)+1):((i + 1) * batch_size)
        features[index_range,,,] <- features_batch
        labels[index_range] <- labels_batch
        i <- i + 1
        if (i * batch_size >= sample_count)break }
    list(
        features = features,
        labels = labels
    ) 
}
train <- extract_features(train_dir,2000)
validation <- extract_features(validation_dir,1000)
test <- extract_features(test_dir,1000)

# Flattening the outputs --------------------------------------------------
reshape_features <- function(features){
    array_reshape(features, dim = c(nrow(features),4*4*512))
}

train$features <- reshape_features(train$features)
validation$features <- reshape_features(validation$features)
test$features <- reshape_features(test$features)


# Defining and training the densely connected classifier ------------------
model <- keras_model_sequential() %>%
    layer_dense(units = 256,
                activation = "relu",
                input_shape = 4*4*512) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1,
                activation = "sigmoid")

model %>% compile(
    optimizer = optimizer_rmsprop(lr = 2e-5),
    loss = "binary_crossentropy",
    metrics = c("accuracy")
)

history <- model %>% fit(
    train$features,
    train$labels,
    epochs = 30,
    batch_size = 20,
    validation_data = list(validation$features, validation$labels)
)

plot(history)
history$metrics$val_acc


# Visualizing intermediate activations ------------------------------------
remove(list = ls())
model <- load_model_hdf5("cats_and_dogs_small_2.h5")

img_path <- "cats_dogs_small/test/cats/cat.1700.jpg"
img <- image_load(img_path, target_size = c(150,150)) ##Load and convert
img_tensor <- image_to_array(img) ##Pass it to an array
img_tensor <- array_reshape(img_tensor, c(1,150,150,3))
img_tensor <- img_tensor/255 ##Shring the variability
dim(img_tensor)

plot(as.raster(img_tensor[1,,,]))


layer_outputs <- lapply(model$layers[1:8], function(layer)layer$output) ##Extract putputs
activation_model <- keras_model(inputs = model$input, outputs = layer_outputs)

activations <- activation_model %>% predict(img_tensor)
first_layer_activation <- activations[[1]] ##First output
dim(first_layer_activation)

# Function to plot a channel ----------------------------------------------
plot_channel <- function(channel) {
    rotate <- function(x) t(apply(x, 2, rev))
    image(rotate(channel), axes = FALSE, asp = 1,
          col = terrain.colors(12))
}

plot_channel(first_layer_activation[1,,,2])
plot_channel(first_layer_activation[1,,,7]) ##Nose ears

image_size <- 58
images_per_row <- 16
for (i in 1:8) {
    layer_activation <- activations[[i]]
    layer_name <- model$layers[[i]]$name
    n_features <- dim(layer_activation)[[4]]
    n_cols <- n_features %/% images_per_row
    png(paste0("cat_activations_", i, "_", layer_name, ".png"),
        width = image_size * images_per_row,
        height = image_size * n_cols)
    op <- par(mfrow = c(n_cols, images_per_row), mai = rep_len(0.02, 4))
    for (col in 0:(n_cols-1)) {
        for (row in 0:(images_per_row-1)) {
            channel_image <- layer_activation[1,,,(col*images_per_row) + row + 1]
            plot_channel(channel_image)
        }
    }
    par(op)
    dev.off()
}

# Visualizing convnets filters --------------------------------------------
remove(list = ls())

model <- application_vgg16(
    weights = "imagenet",
    include_top = FALSE
)

layer_name <- "block3_conv1"
filter_index <- 1
layer_output <- get_layer(model, layer_name)$output
loss <- k_mean(layer_output[,,,filter_index])
grads <- k_gradients(loss, model$input)[[1]]
grads <- grads/(k_sqrt(k_mean(k_square(grads))) + 1e-5)
iterate <- k_function(list(model$input),list(loss, grads))
c(loss_value, grads_value) %<-%iterate(list(array(0,dim = c(1,150,150,3))))
input_img_data <- array(runif(150 * 150 * 3),
                        dim = c(1, 150, 150, 3)) * 20 + 128
step <- 1
for (i in 1:40) {
    c(loss_value, grads_value) %<-% iterate(list(input_img_data))
    input_img_data <- input_img_data + (grads_value * step)
}

deprocess_image <- function(x) {
    dms <- dim(x)
    x <- x - mean(x)
    x <- x / (sd(x) + 1e-5)
    x <- x * 0.1
    x <- x + 0.5
    x <- pmax(0, pmin(x, 1))
    array(x, dim = dms)
}

generate_pattern <- function(layer_name, filter_index, size = 150) {
    layer_output <- model$get_layer(layer_name)$output
    loss <- k_mean(layer_output[,,,filter_index])
    grads <- k_gradients(loss, model$input)[[1]]
    grads <- grads / (k_sqrt(k_mean(k_square(grads))) + 1e-5)
    iterate <- k_function(list(model$input), list(loss, grads))
    input_img_data <-
        array(runif(size * size * 3), dim = c(1, size, size, 3)) * 20 + 128
    step <- 1
    for (i in 1:40) {
        c(loss_value, grads_value) %<-% iterate(list(input_img_data))
        input_img_data <- input_img_data + (grads_value * step)
    }
    img <- input_img_data[1,,,]
    deprocess_image(img)
}

library(grid)
grid.raster(generate_pattern("block3_conv1",1))