

# example code ------------------------------------------------------------
x <- c(1, 2)
x

y <- c(3, 4)
y

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

## quick coding
z <- c(1, 2, 3)


# exercise ----------------------------------------------------------------

# create a vector with length 10 assign it to z
z <- c(1:10)
length(z)

# create a numeric matrix with 2 rows x 2 columns

m <- matrix(1:4, nrow = 2, ncol = 2)
m
m <- cbind(c(1, 2), c(3, 4))
m
m <- rbind(c(1, 2), c(3, 4))
m

# data frame

df0 <- data.frame(name = c("Smith", "John", "Kate", "Cynthia"), height = c(154, 170, 156, 175))
df0
