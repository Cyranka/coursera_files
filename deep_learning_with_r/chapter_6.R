remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/deep_learning_with_r/")
library(keras);library(tidyverse)

# Word-level one-hot encoding ---------------------------------------------
samples <- c("The cat sat on the mat.", "The dog ate my homework.")
token_index <- list()

for(sample in samples){
    for(word in strsplit(sample," ")[[1]]){
        if(!word %in% names(token_index)){
            token_index[[word]] <- length(token_index) + 2
        }
    }
}

max_length <- 10
results <- array(0,dim = c(length(samples),
                           max_length,
                           max(as.integer(token_index))))

for(i in 1:length(samples)){
    sample <- samples[[i]]
    words <- head(strsplit(sample, " ")[[1]], n = max_length)
    for(j in 1:length(words)){
        index <- token_index[[words[[j]]]]
        results[[i,j,index]] <- 1
    }
}
results[1,,] ##This is a 3-D tensor

# Using Keras for word-level one hot-encoding -----------------------------
library(keras)
samples <- c("The cat sat on the mat.", "The dog ate my homework.")
tokenizer <- text_tokenizer(num_words = 1000) %>% ##Select the 1,000 most common words
    fit_text_tokenizer(samples) ##Build the word index

sequences <- texts_to_sequences(tokenizer, samples)
one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary")
word_index <- tokenizer$word_index

# Instantiating an embedding layer ----------------------------------------
embedding_layer <- layer_embedding(input_dim = 1000,output_dim = 64)

# 6.6 Loading the IMDB data for use with an embedding layer ---------------
rm(list = ls())
max_features <- 10000
maxlen <- 40

imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train),c(x_test,y_test)) %<-% imdb

x_train <- pad_sequences(x_train, maxlen = maxlen) ##Trim the reviews
x_test <- pad_sequences(x_test, maxlen = maxlen) 

model <- keras_model_sequential() %>%
    layer_embedding(
        input_dim = 10000,
        output_dim = 8,
        input_length = 40
    ) %>%
    layer_flatten() %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>% 
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("acc")
    )

summary(model)

history <- model %>% 
    fit(
        x_train, 
        y_train,
        epochs = 10,
        batch_size = 32,
        validation_split = 0.2
    )

mean(history$metrics$val_acc) ##Merely adding more words, improves accuracy

# 6.8 Processing the labels of raw IMDB data ------------------------------
rm(list = ls())
imdb_dir <- "aclImdb/"
train_dir <- file.path(imdb_dir, "train")

labels <- c()
texts <- c()

##Creates two vectors with text data: one for texts, the other for labels
for(label_type in c("neg", "pos")){
    label <- switch(label_type, neg = 0, pos = 1)
    dir_name <- file.path(train_dir, label_type)
    for(fname in list.files(dir_name, pattern = glob2rx("*.txt"),full.names = TRUE)){
        texts <- c(texts, readChar(fname, file.info(fname)$size))
        labels <- c(labels, label)
    }
}

# 6.9 Tokenizing the text of the raw IMDB data ----------------------------
maxlen <- 100 #Cuts reviews after 100 words
training_samples <- 200 #Trains on 200 samples
validation_samples <- 10000 #Validates on 10,000 samples
max_words <- 10000 ##Consider only the top 10,000 words

tokenizer <- text_tokenizer(num_words = max_words) %>%
    fit_text_tokenizer(texts) ##Fit tokenizer

sequences <- texts_to_sequences(tokenizer, texts) ##Each sentence is now a vector of integers
word_index <- tokenizer$word_index ##Grab index of each word: look up dictionary

cat("Found", length(word_index), "unique tokens.\n") #Real vocabulary size

data <- pad_sequences(sequences, maxlen = maxlen) ##Ensure that all vectors have the same size
labels <- as.array(labels) ##Make labels array

cat("Shape of data tensor:", dim(data), "\n")
cat("Shape of label tensor:", dim(labels), "\n") 

##Shuffle the data to perform split
set.seed(2)
indices <- sample(1:nrow(data))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):(training_samples + validation_samples)]

x_train <- data[training_indices,] 
y_train <- labels[training_indices]

x_val <- data[validation_indices,]
y_val <- labels[validation_indices]

# 6.10 Parsing the GloVe word-embedding file ------------------------------
glove <- "glove.6B/"
lines <- readLines(file.path(glove, "glove.6B.100d.txt"))

embeddings_index <- new.env(hash = TRUE, parent = emptyenv())
for(i in 1:length(lines)){
    line <- lines[[i]]
    values <- strsplit(line, " ")[[1]]
    word <- values[[1]]
    embeddings_index[[word]] <- as.double(values[-1])
}

# 6.11: Preparing the GloVe word-embeddings matrix ------------------------
embedding_dim <- 100
embedding_matrix <- array(0,c(max_words, embedding_dim))

for(word in names(word_index)){
    index <- word_index[[word]]
    if(index < max_words){
        embedding_vector <- embeddings_index[[word]]
        if(!is.null(embedding_vector)){
            embedding_matrix[index + 1, ] <- embedding_vector
        }
    }
}

# Model definition --------------------------------------------------------
model <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_words,
                    output_dim = embedding_dim,
                    input_length = maxlen) %>%
    layer_flatten() %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

get_layer(model, index = 1) %>%
    set_weights(list(embedding_matrix)) %>%
    freeze_weights() ##Basically, setting the output of the embedding layer as the output matrix

model %>% 
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("acc")
    )

history <- model %>% 
    fit(
        x_train, y_train,
        epochs = 20,
        batch_size = 32,
        validation_dta = list(x_val, y_val)
    )

save_model_weights_hdf5(model, "pre_trained_glove_model.h5")
plot(history)

# Training the same model w/out pretrained word embeddings ----------------
model <- keras_model_sequential() %>%
    layer_embedding(
        input_dim = max_words,
        output_dim = embedding_dim,
        input_length = maxlen
    ) %>%
    layer_flatten() %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>% 
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("acc")
    )

history <- model %>%
    fit(
        x_train, y_train,
        epochs = 20,
        batch_size = 32,
        validation_data = list(x_val, y_val)
    )

mean(history$metrics$val_acc) ##52% accuracy on validation set

# 6.17: Tokenizing the test data set --------------------------------------
test_dir <- file.path(imdb_dir, "test")
labels <- c()
texts <- c()

for(label_type in c("neg", "pos")){
    label <- switch(label_type, neg = 0, pos = 1)
    dir_name <- file.path(test_dir, label_type)
    for(fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                            full.names = TRUE)){
        texts <- c(texts, readChar(fname, file.info(fname)$size))
        labels <- c(labels, label)
    }
}

sequences <- texts_to_sequences(tokenizer, texts)
x_test <- pad_sequences(sequences, maxlen = maxlen)
y_test <- as.array(labels)

model %>%
    load_model_weights_hdf5("pre_trained_glove_model.h5") %>%
    evaluate(x_test, y_test)

# 6.19 Pseudocode RNN -----------------------------------------------------
state_t <- 0
for(input_t in input_sequence){ ##Iterates through the elements of each sequence
    output_t <- f(input_t, state_t) #Output at t depends on the input and the state
    state_t <- output_t #The previous output becomes the state for the next iteration
}

state_t <- 0
for(input_t in input_sequence){
    output_t <- activation(dot(W, input_t) + dot(U, state_t) + b)
    state_t <- output_t
}

# 6.21 Implementation of a simple RNN -------------------------------------
set.seed(2)
timesteps <- 100
input_features <- 32
output_features <- 64

random_array <- function(dim){
    array(runif(prod(dim)), dim = dim)
}

inputs <- random_array(dim = c(timesteps, input_features))
state_t <- rep_len(0,length.out =c(output_features))

#Create random weight matrices
W <- random_array(dim = c(output_features, input_features))
U <- random_array(dim = c(output_features, output_features))
b <- random_array(dim = c(output_features,1))

output_sequence <- array(0,dim = c(timesteps,output_features))
for(i in 1:nrow(inputs)){
    input_t <- inputs[i,]
    output_t <- tanh(as.numeric((W %*% input_t) + (U %*% state_t) + b)) ##Process the input
    output_sequence[i,] <- as.numeric(output_t) ##Convert to numeric
    state_t <- output_t ##Save output computed at time T
}

# 6.22 b) Pseudo-implementation of RNNs ----------------------------------

remove(list = ls())
model <- keras_model_sequential() %>%
    layer_embedding(input_dim = 10000, output_dim = 32) %>%
    layer_simple_rnn(units = 32) ##Model that returns only the output of each batch

summary(model)

model <- keras_model_sequential() %>%
    layer_embedding(input_dim = 10000, output_dim = 32) %>%
    layer_simple_rnn(units = 32, return_sequences = TRUE)

summary(model) ##Returns each state for each sequence step for each batch

##Stacking RNN layers
model <- keras_model_sequential() %>%
    layer_embedding(input_dim = 10000, output_dim = 32) %>%
    layer_simple_rnn(units = 32, return_sequences = TRUE)%>%
    layer_simple_rnn(units = 32, return_sequences = TRUE)%>%
    layer_simple_rnn(units = 32, return_sequences = TRUE)%>%
    layer_simple_rnn(units = 32)

summary(model)

# 6.22 Preparing the IMDB data -------------------------------------------------
remove(list = ls())

max_features <- 10000
maxlen <- 500
batch_size <- 32

imdb <- dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
cat(length(input_train), "train sequences\n")
cat(length(input_test), "train sequences\n")

cat("Pad sequences (samples x time)\n")
input_train <- pad_sequences(input_train, maxlen = maxlen)
input_test <- pad_sequences(input_test, maxlen = maxlen)

cat("input_train shape:", dim(input_train), "\n")
cat("input_test shape:", dim(input_test), "\n")

# 6.23 Training the model with embedding and simple RNN layers ------------
model <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features, output_dim = 32) %>%
    layer_simple_rnn(units = 32) %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("acc")
)

history <- model %>% fit(
    input_train, y_train,
    epochs = 10,
    batch_size = 128,
    validation_split = 0.2
)

mean(history$metrics$val_acc)

# 6.27 using the LSTM layer in Keras --------------------------------------
model <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features, output_dim = 32) %>%
    layer_lstm(units = 32) %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>% 
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("acc")
    )

history <- model %>%
    fit(
        input_train, y_train,
        epochs = 10,
        batch_size = 128,
        validation_split = 0.2
    )

plot(history)
mean(history$metrics$val_acc)## 87% average validation accuracy

# 6.28 Inspecting the data of the Jena weather dataset --------------------
data <- read_csv("jena_climate/jena_climate_2009_2016.csv")
glimpse(data)

data %>%
    ggplot(aes(x = 1:nrow(data), y = `T (degC)`)) + 
    geom_line()

ggplot(data[1:1440,],aes(x = 1:1440, y = `T (degC)`)) + 
    geom_line()

# 6.31 Converting the data into a floating-point matrix -------------------
data <- data.matrix(data[,-1])
train_data <- data[1:200000,]

mean <- apply(train_data, 2, mean) #Obtain scale mean
std <- apply(train_data, 2, sd) ##obtain scale sd

data <- scale(data, center = mean, scale = std) ##Notice that you are applying to the whole dataset

# 6.33 Generator yielding timeseries samples and their targets ------------
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
    if (is.null(max_index)) max_index <- nrow(data) - delay - 1
    i <- min_index + lookback
    function() {
        if (shuffle) {
            rows <- sample(c((min_index+lookback):max_index), size = batch_size)
        } else {
            if (i + batch_size >= max_index)
                i <<- min_index + lookback
            rows <- c(i:min(i+batch_size, max_index))
            i <<- i + length(rows)
        }
        samples <- array(0, dim = c(length(rows),
                                    lookback / step,
                                    dim(data)[[-1]]))
        targets <- array(0, dim = c(length(rows)))
        for (j in 1:length(rows)) {
            indices <- seq(rows[[j]] - lookback, rows[[j]],
                           length.out = dim(samples)[[2]])
            samples[j,,] <- data[indices,]
            targets[[j]] <- data[rows[[j]] + delay,2]
        }
        list(samples, targets)
    }
}

# 6.34 Preparing the training, validation, and test generators ------------
lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

train_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 1,
    max_index = 200000,
    shuffle = TRUE,
    step = step,
    batch_size = batch_size
)

val_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 200001,
    max_index = 300000,
    shuffle = TRUE,
    step = step,
    batch_size = batch_size
)

test_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 300001,
    max_index = NULL,
    shuffle = TRUE,
    step = step,
    batch_size = batch_size
)

val_steps <- (300000 - 200001 - lookback)/batch_size
test_steps <- (nrow(data) - 300001 - lookback)/batch_size

# 6.36 Computing the common-sense baseline MAE ---------------------------------
evaluate_naive_method <- function(){
    batch_maes <- c()
    for(step in 1:val_steps){
        c(samples, targets) %<-% val_gen()
        preds <- samples[,dim(samples)[[2]],2]
        mae <- mean(abs(preds - targets))
        batch_maes <- c(batch_maes, mae)
    }
    print(mean(batch_maes))
}

evaluate_naive_method()

# 6.37 A basic machine-learning approach ----------------------------------
model <- keras_model_sequential() %>%
    layer_flatten(input_shape = c(lookback/step, dim(data)[-1])) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)

model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mae"
)

history <- model %>% fit_generator(
    train_gen,
    steps_per_epoch = 500,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps
)

plot(history)

# 6.39 Training and evaluating a model with layer_gru ---------------------
model <- keras_model_sequential() %>%
    layer_gru(
        units = 32,
        input_shape = list(NULL,dim(data)[[-1]])
    ) %>%
    layer_dense(units = 1)

model %>%
    compile(
        optimizer = optimizer_rmsprop(),
        loss = "mae"
    )

history <- model %>% 
    fit_generator(
        train_gen,
        steps_per_epoch = 500,
        epochs = 20,
        validation_data = val_gen,
        validation_steps = val_steps
    ) 

plot(history)
mean(history$metrics$val_loss) ##Average MAE with GRU layer: better than naive


# 6.40 Training and evaluating a dropout regularized model with layer_gru ---------------------
model <- keras_model_sequential() %>%
    layer_gru(
        units = 32,
        dropout = 0.2,
        recurrent_dropout = 0.2,
        input_shape = list(NULL,dim(data)[[-1]])
    ) %>%
    layer_dense(units = 1)


model %>%
    compile(
        optimizer = optimizer_rmsprop(),
        loss = "mae"
    )

history <- model %>% 
    fit_generator(
        train_gen,
        epochs = 40,
        validation_data = val_gen,
        validation_steps = val_steps
    ) 

# 6.42 Training and evaluating an LSTM using reversed sequences -----------
max_features <- 10000
maxlen <- 500

imdb <- dataset_imdb(num_words = max_features)
c(c(x_train,y_train),c(x_test, y_test)) %<-% imdb

x_train <- lapply(x_train,rev) ##Reverse training feature matrix
x_test <- lapply(x_test, rev) ##Reverse test feature matrix

x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)

model <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features,
                    output_dim = 128) %>%
    layer_lstm(units = 32) %>%
    layer_dense(units = 1,
                activation = "sigmoid")

model %>%
    compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = c("acc")
    )

history <- model %>% fit(
    x_train, y_train,
    epochs = 10,
    batch_size = 128,
    validation_split = 0.2
)

mean(history$metrics$val_acc)

# Training and evaluating a bidirectional LSTM ----------------------------
model <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_features,
                    output_dim = 32) %>%
    bidirectional(
        layer_lstm(units = 32)
    ) %>%
    layer_dense(units = 1,
                activation = "sigmoid")

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("acc")
)

history <- model %>% 
    fit(
        x_train, y_train,
        epochs = 10,
        batch_size = 128,
        validation_split = 0.2
    )

mean(history$metrics$val_acc)


# Preparing the IMDB data for convnets ------------------------------------
remove(list =ls())
max_features <- 10000
max_len <- 500

imdb <- dataset_imdb(num_words = max_features)
c(c(x_train,y_train),c(x_test,y_test)) %<-% imdb
x_train <- pad_sequences(x_train, maxlen = max_len)
x_test <- pad_sequences(x_test, maxlen = max_len)


# 6.46 Training and evaluating a simple 1D convnet ------------------------
model <- keras_model_sequential() %>%
    layer_embedding(
        input_dim = max_features,
        output_dim = 128,
        input_length = max_len
    ) %>%
    layer_conv_1d(filters = 32,kernel_size = 7,activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 5) %>%
    layer_conv_1d(filters = 32,kernel_size = 7,activation = "relu") %>%
    layer_global_average_pooling_1d() %>%
    layer_dense(units = 1)

model %>% compile(
    optimizer = optimizer_rmsprop(lr = 1e-4),
    loss = "binary_crossentropy",
    metrics = c("acc")
)

history <- model %>% fit(
    x_train, y_train,
    epochs = 10,
    batch_size = 128,
    validation_split = 0.2
)

