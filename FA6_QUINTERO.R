
install.packages("moments")
library(moments)

p <- 0.2
n <- 1000
r_generator <- rgeom(n, p)

mean_x <- mean(r_generator)
var_x <- var(r_generator)
sd_x <- sd(r_generator)

cat("The number of trials required to achieve the first success is: ", which(r_generator == 1)[1], "\n")
cat("The Mean is: ", round(mean_x, digits = 2), "\n")
cat("The Variance is: ", round(var_x, digits = 2), "\n")
cat("The Standard Deviation is: ", round(sd_x, digits = 2), "\n")

#histogram

hist(r_generator, main = "Geometric Distribution", xlab= "Number of Trials to Achieve the First Success", col="lightpink"
     ,border="black")
