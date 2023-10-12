# Set the number of simulations
num_simulations <- 10000

# Initialize variables to keep track of wins when switching and staying
switch_wins <- 0
stay_wins <- 0

# Perform the simulations
for (i in 1:num_simulations) {
  # Create three doors with one car and two goats
  doors <- sample(c("car", "goat", "goat"))

  # Player's initial choice
  player_choice <- sample(1:3, 1)

  # Monty Hall reveals a goat behind one of the unchosen doors
  monty_reveals <- which(doors[-player_choice] == "goat")
  monty_reveals <- monty_reveals[1]  # Monty reveals the first goat he encounters

  # Determine the other unchosen door
  other_door <- setdiff(1:3, c(player_choice, monty_reveals))

  # Simulate switching doors
  switch_choice <- other_door[1]

  # Check if the player wins when switching
  if (doors[switch_choice] == "car") {
    switch_wins <- switch_wins + 1
  }

  # Check if the player wins when staying
  if (doors[player_choice] == "car") {
    stay_wins <- stay_wins + 1
  }
}

# Calculate win percentages
switch_win_percent <- (switch_wins / num_simulations) * 100
stay_win_percent <- (stay_wins / num_simulations) * 100

# Print results
cat("When switching doors, the win percentage is:", switch_win_percent, "%\n")
cat("When staying with the initial choice, the win percentage is:", stay_win_percent, "%\n")

