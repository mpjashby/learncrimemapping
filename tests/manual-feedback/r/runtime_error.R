# Create data before the deliberate error
values <- 1:5
mean_value <- mean(values)
stop("This is an intentional runtime error.")

# This line must not execute
later_value <- 100
