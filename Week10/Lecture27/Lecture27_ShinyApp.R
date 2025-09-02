# Power Analysis Shiny App
# This app allows the user to set parameters and visualize the null/alternative distributions and power.

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Two-Sample Power Analysis Visualizer"),
  fluidRow(
    column(4,
      wellPanel(
        numericInput("n1", "Sample size for group 1:", value = 30, min = 2),
        numericInput("n2", "Sample size for group 2:", value = 30, min = 2),
        numericInput("sd1", "Standard deviation for group 1:", value = 10, min = 0.1),
        numericInput("sd2", "Standard deviation for group 2:", value = 10, min = 0.1),
  numericInput("null_mean", "Null hypothesis mean:", value = 0),
  numericInput("alt_mean", "Alternative hypothesis mean:", value = 5),
        numericInput("alpha", "Significance level (alpha):", value = 0.05, min = 0.0001, max = 0.5, step = 0.01)
      )
    ),
    column(8,
      plotOutput("powerPlot"),
      verbatimTextOutput("powerText")
    )
  )
)

server <- function(input, output) {
  output$powerPlot <- renderPlot({
  null_mean <- input$null_mean
  alt_mean <- input$alt_mean
    se <- sqrt((input$sd1^2 / input$n1) + (input$sd2^2 / input$n2))
    effect_size <- abs(alt_mean - null_mean)
    critical_value <- qnorm(1 - input$alpha / 2) * se
    x <- seq(-4 * se + min(null_mean, alt_mean), 4 * se + max(null_mean, alt_mean), length.out = 1000)
    null_dist <- dnorm(x, mean = null_mean, sd = se)
    alt_dist <- dnorm(x, mean = alt_mean, sd = se)
    plot_df <- rbind(
      data.frame(x = x, y = null_dist, dist = "Null"),
      data.frame(x = x, y = alt_dist, dist = "Alternative")
    )
    z_upper <- (critical_value - effect_size) / se
    z_lower <- (-critical_value - effect_size) / se
    power <- pnorm(z_lower) + (1 - pnorm(z_upper))
    ggplot(plot_df, aes(x = x, y = y, color = dist)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("Null" = "steelblue", "Alternative" = "darkgreen")) +
      geom_vline(xintercept = c(-critical_value, critical_value), linetype = "dashed", color = "steelblue") +
      geom_area(
        data = subset(plot_df, dist=="Alternative" & x >= critical_value),
        aes(y = y),
        fill = 'darkgreen', alpha = 0.5) +
      geom_area(
        data = subset(plot_df, dist=="Alternative" & x <= -critical_value),
        aes(y = y),
        fill = 'darkgreen', alpha = 0.5) +
      labs(title = "Null and Alternative Hypothesis Distributions",
           x = "Difference in Sample Means",
           y = "Density",
           color = "Distribution")
  })
  output$powerText <- renderText({
  se <- sqrt((input$sd1^2 / input$n1) + (input$sd2^2 / input$n2))
  effect_size <- abs(input$alt_mean - input$null_mean)
  critical_value <- qnorm(1 - input$alpha / 2) * se
  z_upper <- (critical_value - effect_size) / se
  z_lower <- (-critical_value - effect_size) / se
  power <- pnorm(z_lower) + (1 - pnorm(z_upper))
  sprintf("Power of the test: %.3f", power)
  })
}

shinyApp(ui = ui, server = server)
