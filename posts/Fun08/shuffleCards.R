# Install the plotly package if not already installed
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

# Load the plotly package
library(plotly)

# Function to simulate the position of the ace of hearts after shuffling
simulate_ace_of_hearts_position <- function(num_shuffles, num_simulations) {
  original_deck <- 1:52
  ace_position <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    shuffled_deck <- faro(original_deck, num_shuffles)
    ace_position[i] <- which(shuffled_deck == 12)
  }
  
  return(ace_position)
}

# Simulation parameters
num_shuffles_list <- seq(1, 30, by = 1)
num_simulations <- 100

# Run the simulation
ace_positions <- lapply(num_shuffles_list, function(num_shuffles) {
  simulate_ace_of_hearts_position(num_shuffles, num_simulations)
})

# Prepare data for the plot
df <- data.frame(
  Num_Shuffles = rep(num_shuffles_list, each = num_simulations),
  Ace_Position = unlist(ace_positions)
)

# Create a line plot with a ribbon in Plotly
plot_ly(df, x = ~Num_Shuffles, y = ~Ace_Position, type = 'scatter', mode = 'lines', line = list(color = 'black')) %>%
  add_ribbons(ymin = ~Ace_Position - 5, ymax = ~Ace_Position + 5, fillcolor = 'lightblue' ) %>%
  layout(
    title = "Ace of Hearts Position relative to Initial Position",
    xaxis = list(title = "Number of Shuffles"),
    yaxis = list(title = "Ace of Hearts Position"),
    showlegend = FALSE
  )


