
search_simulation <- function(number_searches, p) {
  searches_success<- numeric(number_searches)
  for (i in 1:number_searches) {
    searches <- 0
    while (runif(1) > p) {
      searches <- searches + 1
    }
    searches_success[i] <- searches + 1 
  }
  return(searches_success)
}
number_searches <- 10000
p <- 0.6

searches_success <- search_simulation(num_searches, p)
hist(searches_success, breaks = 30, prob = TRUE, main = "Simulated PDF of Searches to First Success", xlab = "Number of Searches to First Success")

mean_searches <- mean(searches_success)
variance_searchers<- var(searches_success)
cat("Mean of Searches to First Success:", mean_searches, "\n")
cat("Variance of Searches to First Success:", variance_searches, "\n")

conditional_distribution <- searches_to_success[searches_to_success > 3]
mean_conditional <- mean(conditional_distribution)

variance_conditional <- var(conditional_distribution)
cat("Mean of Conditional Distribution (X > 3):", mean_conditional, "\n")
cat("Variance of Conditional Distribution (X > 3):", variance_conditional, "\n")

prob_1 <- mean(searches_success == 1)
prob_4_given_3 <- mean(searches_success == 4 & searches_success > 3)
prob_2_given_3 <- mean(searches_success == 2 & searches_success > 3)

cat("a) P(X = 4 | X > 3):", prob_4_given_3, "\n")
cat("   P(X = 1):", prob_1, "\n")

cat("b) P(X = 2 | X > 3):", prob_2_given_3, "\n")
cat("   P(X = 2):", mean(searches_success == 2), "\n")


