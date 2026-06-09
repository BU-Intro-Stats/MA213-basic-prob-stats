# Power Analysis Shiny App
# This app allows the user to set parameters and visualize the null/alternative distributions and power.

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Two-Sample Power Analysis Visualizer"),
  fluidRow(
    column(4,
      wellPanel(
        numericInput("se", "Standard error:", value = 2, min = 0.01),
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
    se <- input$se
    effect_size <- abs(alt_mean - null_mean)
    critical_value <- qnorm(1 - input$alpha / 2) * se
    x <- seq(-4 * se + min(null_mean, alt_mean), 4 * se + max(null_mean, alt_mean), length.out = 1000)
    null_dist <- dnorm(x, mean = null_mean, sd = se)
    alt_dist <- dnorm(x, mean = alt_mean, sd = se)
    plot_df <- rbind(
      data.frame(x = x, y = null_dist, dist = "Null"),
      data.frame(x = x, y = alt_dist, dist = "Alternative")
    )
    # Compute critical boundaries using null mean
    zstar <- qnorm(1 - input$alpha / 2)
    upper_boundary <- null_mean + zstar * se
    lower_boundary <- null_mean - zstar * se
    # Compute power using alternative mean
    power <- pnorm(lower_boundary, mean = alt_mean, sd = se) + (1 - pnorm(upper_boundary, mean = alt_mean, sd = se))
    ggplot(plot_df, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("Null" = "steelblue", "Alternative" = "darkgreen")) +
      geom_vline(xintercept = c(lower_boundary, upper_boundary), linetype = "dashed", color = "steelblue") +
      geom_area(
        data = subset(plot_df, dist=="Alternative" & x >= upper_boundary),
        aes(y = y),
        fill = 'darkgreen', alpha = 0.5) +
      geom_area(
        data = subset(plot_df, dist=="Alternative" & x <= lower_boundary),
        aes(y = y),
        fill = 'darkgreen', alpha = 0.5) +
      labs(title = "Null and Alternative Hypothesis Distributions",
           x = "Difference in Sample Means",
           y = "Density",
           color = "Distribution")
  })
  output$powerText <- renderText({
    null_mean <- input$null_mean
    alt_mean <- input$alt_mean
    se <- input$se
    zstar <- qnorm(1 - input$alpha / 2)
    upper_boundary <- null_mean + zstar * se
    lower_boundary <- null_mean - zstar * se
    power <- pnorm(lower_boundary, mean = alt_mean, sd = se) + (1 - pnorm(upper_boundary, mean = alt_mean, sd = se))
    sprintf("Power of the test: %.3f", power)
  })
}

shinyApp(ui = ui, server = server)
