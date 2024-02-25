# Given probabilities
prob_R0_given_T0 <- 0.95       
prob_R1_given_T1 <- 0.75       
prob_T0 <- 0.70                

# Calculate the complement of prob_R1_given_T1
prob_R1_given_T1_comp <- 1 - prob_T0 * (1 - prob_R1_given_T1)

# (a) Probability that a 1 was received
prob_R1 <- prob_T0 * (1 - prob_R0_given_T0) + (1 - prob_T0) * prob_R1_given_T1_comp

# (b) Probability that a 1 was transmitted given that a 1 was received
prob_T1_given_R1 <- prob_T0 * prob_R0_given_T0 / prob_R1

# Print the results
cat("Probability that a 1 was received:", prob_R1, "\n")
cat("Probability that a 1 was transmitted given that a 1 was received:", prob_T1_given_R1, "\n")


# 2.

# Employee percentages of programming
percentage <- c(0.10, 0.30, 0.60)

# Error rates for each employee
error <- c(0.08, 0.05, 0.01)

# Employee names
employees <- c("Jane", "Amy", "Ava")

# Calculate overall error rate
overallerror <- sum(percentage * error)

# Print the overall error rate
cat("Overall Error Rate:", overall_error_rate, "\n")

# Identify the most likely person to have written a program with an error
most_likely_person <- which.max(percentage * error)
most_likely_person_name <- employees[most_likely_person]

# Print the most likely person's name
cat("Most likely employee to have a error in her program is:", most_likely_person_name, "\n")

