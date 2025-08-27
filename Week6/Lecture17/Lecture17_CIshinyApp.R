library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Confidence Interval Simulation for a Proportion"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Sample size (N):", value = 850, min = 1),
      numericInput("p_true", "True population proportion (p):", value = 0.7, min = 0, max = 1, step = 0.01),
      numericInput("alpha", "Significance level (alpha):", value = 0.05, min = 0, max = 1, step = 0.01),
  actionButton("run1", "Run 1 Experiment"),
  actionButton("run10", "Run 10 Experiments"),
  actionButton("reset", "Reset Results", style = "color: white; background-color: #d9534f;"),
      hr(),
      verbatimTextOutput("coverage")
    ),
    mainPanel(
      plotOutput("ciPlot", height = "600px"),
      hr(),
      plotOutput("coveragePlot", height = "250px")
    )
  )
)

server <- function(input, output, session) {
  # Store all intervals and coverage
  vals <- reactiveValues(
    df = data.frame(trial = integer(), p_hat = numeric(), lower = numeric(), upper = numeric(), covers = logical()),
    n_trials = 0
  )

  observeEvent(input$run1, {
    add_trials(1)
  })
  observeEvent(input$run10, {
    add_trials(10)
  })
  observeEvent(input$reset, {
    vals$df <- data.frame(trial = integer(), p_hat = numeric(), lower = numeric(), upper = numeric(), covers = logical())
    vals$n_trials <- 0
  })

  add_trials <- function(n) {
    N <- input$N
    p_true <- input$p_true
    alpha <- input$alpha
    z <- qnorm(1 - alpha/2)
    samples <- rbinom(n = n, size = N, prob = p_true)
    p_hats <- samples / N
    SEs <- sqrt(p_hats * (1 - p_hats) / N)
    lower <- p_hats - z * SEs
    upper <- p_hats + z * SEs
    covers <- (lower <= p_true) & (upper >= p_true)
    new_df <- data.frame(
      trial = seq(vals$n_trials + 1, vals$n_trials + n),
      p_hat = p_hats,
      lower = lower,
      upper = upper,
      covers = covers
    )
    vals$df <- rbind(vals$df, new_df)
    vals$n_trials <- vals$n_trials + n
  }

  output$ciPlot <- renderPlot({
    if (nrow(vals$df) == 0) return(NULL)
    ggplot(vals$df, aes(y = trial, x = p_hat)) +
      geom_point(aes(color = covers), size = 2) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = covers), height = 0.3) +
      geom_vline(xintercept = input$p_true, color = "green", linetype = "solid") +
      scale_color_manual(values = c("FALSE" = "firebrick", "TRUE" = "black"),
                         labels = c("FALSE" = "Miss", "TRUE" = "Cover"),
                         name = "CI Contains True p?") +
      scale_y_reverse() +
      labs(x = "Sample Proportion (p̂)", y = "Trial",
           title = sprintf("Confidence Intervals for p (Coverage: %.1f%%)", 100*mean(vals$df$covers))) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  output$coverage <- renderText({
    if (nrow(vals$df) == 0) return("")
    sprintf("Coverage: %.1f%% (%d out of %d)", 100*mean(vals$df$covers), sum(vals$df$covers), nrow(vals$df))
  })

  output$coveragePlot <- renderPlot({
    if (nrow(vals$df) == 0) return(NULL)
    running_coverage <- cumsum(vals$df$covers) / seq_len(nrow(vals$df))
    df_cov <- data.frame(trial = seq_len(nrow(vals$df)), running_coverage = running_coverage)
    ggplot(df_cov, aes(x = trial, y = running_coverage)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = 1 - input$alpha, linetype = "dashed", color = "darkgreen") +
      ylim(0, 1) +
      labs(x = "Number of Experiments", y = "Running Coverage",
           title = "Running Coverage vs. Nominal Confidence Level") +
      theme_minimal()
  })
}

shinyApp(ui, server)
