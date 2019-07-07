remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/deep_learning_with_r/")
library(keras);library(tidyverse)

# 8.1 Reweighting a probability distribution to a new temperature ---------
reweight_distribution <- function(original_distribution,temperature = 0.5){
    distribution <- log(original_distribution)/temperature
    distribution <- exp(distribution)
    return(distribution/sum(distribution))
}

# 8.2 Downloading and parsing the initial text file -----------------------
path <- get_file(
    "nietzsche.txt",
    origin = "https://s3.amazonaws.com/text-datasets/nietzsche.txt"
)

text <- tolower(readChar(path, file.info(path)$size))

# 8.3 Vectorizing sequences of characters ---------------------------------
##Every three characters, extract a sequence of 60 characters
maxlen <- 60 ##Extract sequences of 60 characters (maximum)
step <- 3 ##Sample a new sequence every 3 characters

text_indexes <- seq(1, nchar(text) - maxlen, by = step) ##Extract breaks
sentences <- str_sub(text, text_indexes,text_indexes + maxlen -1)
next_chars <- str_sub(text, text_indexes + maxlen, text_indexes + maxlen) ##Targets

cat("Number of sequences: ", length(sentences), "\n")
chars <- unique(sort(strsplit(text, "")[[1]])) ##Get unique characters
cat("Unique characters: ", length(cars), "\n")
char_indices <- 1:length(chars)
names(char_indices) <- chars

cat("Vectorization...\n") ##Create one hot encodings of each sequence
x <- array(0L, dim = c(length(sentences),maxlen,length(chars))) ##Create matrices of 60x57
y <- array(0L, dim = c(length(sentences), length(chars)))

for (i in 1:length(sentences)) {
    sentence <- strsplit(sentences[[i]], "")[[1]]
    for (t in 1:length(sentence)) {
        char <- sentence[[t]]
        x[i, t, char_indices[[char]]] <- 1
    }
    next_char <- next_chars[[i]]
    y[i, char_indices[[next_char]]] <- 1
}

# 8.4 Single-layer LSTM model for next-character prediction ---------------
model <- keras_model_sequential() %>%
    layer_lstm(units = 128, input_shape = c(maxlen, length(chars))) %>%
    layer_dense(units = length(chars), activation = "softmax")

optimizer <- optimizer_rmsprop(lr = 0.01) ##Large learning rate
model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer
)

# 8.6 Function to sample next character given the model’s predicti --------
sample_next_char <- function(preds, temperature = 1.0){
    preds <- as.numeric(preds)
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp_preds)
    
    return(which.max(t(rmultinom(1,1,preds))))
}

# 8.7 Text-generating loop ------------------------------------------------
for(epoch in 1:2) {
    cat("epoch", epoch, "\n")

    model %>% fit(x, y, batch_size = 128, epochs = 1)
    
    start_index <- sample(1:(nchar(text) - maxlen - 1), 1)
    seed_text <- str_sub(text, start_index, start_index + maxlen - 1)
    
    cat("--- Generating with seed:", seed_text, "\n\n")
    
    for (temperature in c(0.2, 0.5, 1.0, 1.2)) {
        cat("------ temperature:", temperature, "\n")
        cat(seed_text, "\n")
        
        generated_text <- seed_text
        
        for (i in 1:400) {
            sampled <- array(0, dim = c(1, maxlen, length(chars)))
            generated_chars <- strsplit(generated_text, "")[[1]]
            
            for (t in 1:length(generated_chars)) {
                char <- generated_chars[[t]]
                sampled[1, t, char_indices[[char]]] <- 1
            }
            
            preds <- model %>% predict(sampled, verbose = 0)
            next_index <- sample_next_char(preds[1,], temperature)
            next_char <- chars[[next_index]]
            
            generated_text <- paste0(generated_text, next_char)
            generated_text <- substring(generated_text, 2)
            
            cat(next_char)
        }
        cat("\n\n")
    }
}

##The query text must have 60 characters: the size of the window
start_index <- sample(1:(nchar(text) - maxlen - 1), 1)
seed_text <- "the christian philosophers have tried to interpret the world"

for (temperature in c(0.2, 0.5, 1.0, 1.2)) {
    cat("------ temperature:", temperature, "\n")
    cat(seed_text, "\n")
    
    generated_text <- seed_text
    
    for (i in 1:400) {
        sampled <- array(0, dim = c(1, maxlen, length(chars)))
        generated_chars <- strsplit(generated_text, "")[[1]]
        
        for (t in 1:length(generated_chars)) {
            char <- generated_chars[[t]]
            sampled[1, t, char_indices[[char]]] <- 1
        }
        
        preds <- model %>% predict(sampled, verbose = 0)
        next_index <- sample_next_char(preds[1,], temperature)
        next_char <- chars[[next_index]]
        
        generated_text <- paste0(generated_text, next_char)
        generated_text <- substring(generated_text, 2)
        
        cat(next_char)
    }
    cat("\n\n")
}