library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Random Variable Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("variable_type", "Select Variable Type:",
                   choices = c("Univariate", "Bivariate")),
      conditionalPanel(
        condition = "input.variable_type == 'Univariate'",
        textInput("values", "Values (comma-separated):"),
        textInput("probabilities", "Probabilities (comma-separated):")
      ),
      conditionalPanel(
        condition = "input.variable_type == 'Bivariate'",
        textInput("values_x", "Values of X (comma-separated):"),
        textInput("values_y", "Values of Y (comma-separated):"),
        textInput("probabilities", "Joint Probabilities (comma-separated):")
      ),
      actionButton("calculate", "Calculate"),
      br(),
      h4("Results:"),
      verbatimTextOutput("results")
    ),
    mainPanel(
      plotOutput("pdf_plot"),
      plotOutput("cdf_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    # Convert input values to numeric vectors
    if (input$variable_type == "Univariate") {
      values <- as.numeric(unlist(strsplit(input$values, ",")))
    } else {
      values_x <- as.numeric(unlist(strsplit(input$values_x, ",")))
      values_y <- as.numeric(unlist(strsplit(input$values_y, ",")))
    }
    probabilities <- as.numeric(unlist(strsplit(input$probabilities, ",")))
    
    # Check validity of probabilities
    if (any(probabilities < 0) || any(probabilities > 1) || sum(probabilities) != 1) {
      output$results <- renderText("Invalid probabilities!")
      return(NULL)
    }
    
    mean_value <- sum(values * probabilities)
    variance <- sum((values - mean_value)^2 * probabilities)
    
    df <- data.frame(x = values, p = probabilities)
    df <- df[order(df$x), ]
    df$cdf <- cumsum(df$p)
    
    output$pdf_plot <- renderPlot({
      ggplot(df, aes(x, p)) + geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Probability Density Function", x = "Value", y = "Probability")
    })
    
    output$cdf_plot <- renderPlot({
      ggplot(df, aes(x, cdf)) + geom_step(color = "blue") +
        labs(title = "Cumulative Distribution Function", x = "Value", y = "Cumulative Probability")
    })
    
    output$results <- renderPrint({
      paste("Mean:", mean_value, "\nVariance:", variance)
    })
  })
}

shinyApp(ui = ui, server = server)

    
    