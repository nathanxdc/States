num_simulations <- 10000

num_steps <- 30

transition_matrix <- matrix(c(
  NaN,             NaN,             NaN,             NaN,             NaN,
  0.0,       0.3758170,       0.1459695,       0.1492375,       0.3289760,
  0.0,       0.3171355,       0.1534527,       0.2173913,       0.3120205,
  0.0,       0.3159145,       0.1805226,       0.1591449,       0.3444181,
  0.0,       0.3602273,       0.1375000,       0.1488636,       0.3534091
), nrow = 5, byrow = TRUE)

states <- c("No Change", "Slight Increase", "Significant Increase", "Slight Decrease", "Significant Decrease")

simulate_next_state <- function(current_state, transition_matrix, states) {
  current_row <- match(current_state, states)
  next_state_probabilities <- transition_matrix[current_row, ]
  next_state <- sample(states, 1, prob = next_state_probabilities, replace = TRUE)
  return(next_state)
}

set.seed(123) 
simulated_end_states <- character(num_simulations)

for (i in 1:num_simulations) {

  current_state <- "Slight Increase"
  
  for (step in 1:num_steps) {
    current_state <- simulate_next_state(current_state, transition_matrix, states)
  }
  
  simulated_end_states[i] <- current_state
}

end_state_distribution <- table(simulated_end_states) / num_simulations

print("Estimated probability distribution of ending in each state after 30 steps:")
print(end_state_distribution)
