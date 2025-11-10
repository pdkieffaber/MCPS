# This is a self-contained Shiny app.
# Save this code as 'app.R' and click "Run App" in RStudio.

# 1. Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Use the built-in 'cars' dataset
data(cars)

# Calculate the true OLS model once
ols_model <- lm(dist ~ speed, data = cars)
ols_coef <- coef(ols_model)
ols_sse <- sum(residuals(ols_model)^2)

# 2. Define the User Interface (UI)
ui <- fluidPage(
  
  # App title
  titlePanel("Interactive OLS Regression"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs (sliders)
    sidebarPanel(
      
      h4("Adjust Your Line's Parameters:"),
      
      # Slider for Intercept (b0)
      # We'll center the default near the true OLS intercept
      sliderInput("b0",
                  "Intercept (β₀):",
                  min = -40,
                  max = 20,
                  value = -17,
                  step = 0.5),
      
      # Slider for Slope (b1)
      # We'll center the default near the true OLS slope
      sliderInput("b1",
                  "Slope (β₁):",
                  min = 0,
                  max = 8,
                  value = 3.9,
                  step = 0.1),
      
      hr(),
      h4("Sum of Squared Errors (SSE):"),
      p("This is the value OLS tries to minimize."),
      h3(textOutput("sse_text")),
      
      hr(),
      p(paste0("True OLS Intercept: ", round(ols_coef[1], 2))),
      p(paste0("True OLS Slope: ", round(ols_coef[2], 2))),
      p(paste0("True OLS Minimized SSE: ", round(ols_sse, 0)))
    ),
    
    # Main panel for displaying outputs (the plot)
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)

# 3. Define the Server logic
server <- function(input, output) {
  
  # Create a reactive data frame that updates when sliders change
  model_data <- reactive({
    # Get current slider values
    user_b0 <- input$b0
    user_b1 <- input$b1
    
    # Calculate predicted values and errors for the *user's* line
    cars %>%
      mutate(
        user_y_hat = user_b0 + user_b1 * speed,
        user_error = dist - user_y_hat
      )
  })
  
  # Calculate the SSE for the user's line
  user_sse <- reactive({
    sum(model_data()$user_error^2)
  })
  
  # Render the SSE text
  output$sse_text <- renderText({
    round(user_sse(), 0)
  })
  
  # Render the plotly graph
  output$distPlot <- renderPlotly({
    
    # Get the reactive data
    plot_data <- model_data()
    
    # Get the slider values
    user_b0 <- input$b0
    user_b1 <- input$b1
    
    p <- ggplot(plot_data, aes(x = speed, y = dist)) +
      
      # 1. Plot the raw data points
      geom_point(alpha = 0.7, size = 2) +
      
      # 2. Plot the dotted error lines (from points to the user's line)
      geom_segment(
        aes(xend = speed, yend = user_y_hat),
        color = "red",
        linetype = "dashed",
        alpha = 0.5
      ) +
      
      # 3. Plot the user's line (from sliders)
      geom_abline(
        intercept = user_b0,
        slope = user_b1,
        color = "red",
        size = 1.5,
        aes(text = paste("Your Line<br>SSE:", round(user_sse(), 0)))
      ) +
      
      # 4. Plot the "true" OLS line for comparison
      geom_abline(
        intercept = ols_coef[1],
        slope = ols_coef[2],
        color = "blue",
        size = 1,
        linetype = "dotted",
        aes(text = paste("True OLS Line<br>SSE:", round(ols_sse, 0)))
      ) +
      
      labs(
        title = "Adjust Sliders to Minimize the Red Dotted Lines (SSE)",
        x = "Speed (mph)",
        y = "Stopping Distance (ft)"
      ) +
      theme_bw() +
      # Set plot limits
      coord_cartesian(xlim = c(0, 25), ylim = c(-20, 120))
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = c("x", "y", "text"))
  })
}

# 4. Run the app
shinyApp(ui = ui, server = server)
