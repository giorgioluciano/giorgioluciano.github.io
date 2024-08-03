library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Monty Hall Problem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_simulations", "Number of Simulations:", value = 1000),
      actionButton("run_simulation", "Run Simulation")
    ),
    mainPanel(
      plotOutput("results_plot"),
      verbatimTextOutput("result_text")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Initialize variables to keep track of wins when switching and staying
  switch_wins <- 0
  stay_wins <- 0

  observeEvent(input$run_simulation, {
    num_simulations <- input$num_simulations
    for (i in 1:num_simulations) {
      # Create three doors with one car and two goats
      doors <- sample(c("car", "goat", "goat"))

      # Player's initial choice
      player_choice <- sample(1:3, 1)

      # Monty Hall reveals a goat behind one of the unchosen doors
      monty_reveals <- which(doors[-player_choice] == "goat")
      monty_reveals <- monty_reveals[1]

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

    switch_win_percent <- (switch_wins / num_simulations) * 100
    stay_win_percent <- (stay_wins / num_simulations) * 100

    output$results_plot <- renderPlot({
      barplot(c(switch_win_percent, stay_win_percent),
              names.arg = c("Switch", "Stay"),
              ylab = "Win Percentage",
              col = c("green", "blue"),
              main = "Win Percentage When Switching vs. Staying")
    })

    output$result_text <- renderText({
      paste("When switching doors, the win percentage is:", switch_win_percent, "%\n",
            "When staying with the initial choice, the win percentage is:", stay_win_percent, "%")
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
