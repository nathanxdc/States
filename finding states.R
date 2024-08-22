library(dplyr)

data <- read.csv("C:/Users/Nathan/OneDrive - University of Sussex/Documents/GBP to USD Historical Data.csv")

print("Column names in the data:")
print(names(data))

data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data <- data %>% arrange(Date)

if("Change" %in% names(data)) {
  
  print("First few values in 'Change' column:")
  print(head(data$Change))
  
  data <- data %>%
    mutate(State = case_when(
      Change == 0 ~ "No Change",
      Change > 0 & Change <= 0.5 ~ "Slight Increase",
      Change > 0.5 ~ "Significant Increase",
      Change < 0 & Change >= -0.5 ~ "Slight Decrease",
      Change < -0.5 ~ "Significant Decrease",
      TRUE ~ NA_character_  
    ))
  
  data <- data %>% filter(!is.na(State))
  
  print("State distribution:")
  print(table(data$State))
  
  states <- c("No Change", "Slight Increase", "Significant Increase", "Slight Decrease", "Significant Decrease")
  
  transition_matrix <- matrix(0, nrow = length(states), ncol = length(states),
                              dimnames = list(states, states))
  
  for (i in 1:(nrow(data) - 1)) {
    current_state <- data$State[i]
    next_state <- data$State[i + 1]
    
    if (!is.na(current_state) & !is.na(next_state) & current_state %in% states & next_state %in% states) {
      transition_matrix[current_state, next_state] <- transition_matrix[current_state, next_state] + 1
    } else {
      print(paste("Skipping invalid transition from", current_state, "to", next_state))
    }
  }
  
  print("Transition Matrix (Counts):")
  print(transition_matrix)
  
  transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN = "/")
  
  print("Transition Matrix (Probabilities):")
  print(transition_matrix)
  
} else {
  stop("'Change' column not found in the data.")
}

