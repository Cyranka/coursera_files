set.seed(2)
x <- rnorm(n = 100)
y <- x + rnorm(100)


my_lm <- lm(y ~x)
summary(my_lm)

##Coefficient = 0.95290, if I multiply x by 10, the new coefficient should be 1/10
x_10 <- x*10
new_lm <- lm(y ~ x_10)
summary(new_lm) 

##New Coefficient = 0.095290

##new b = b/multiplier of the correspondent x
