library(ggplot2)
library(shiny)

# ---- UI ----
ui <- fluidPage(
  titlePanel("CLT Demonstration for p-hat"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min=0, max=500, value=10, step=10),
      sliderInput("p", "Population Proportion (p):", min=0.01, max=1, 
                  value=0.25, step=0.05),
      checkboxInput("use_succession", "Rule of Succession-adjusted p-hat ((X+1)/(n+2))", value = FALSE),
      actionButton("simulate", "Run Simulation"),
      hr()
    ), mainPanel(plotOutput("histPlot"))))

# ---- Server ----
server <- function(input, output) {
  
  # Reactive function to create population
  population_result <- reactive({
    pop_size <- 250000
    possible_entries <- c(rep("support", round(input$p * pop_size)), 
                          rep("not", round((1 - input$p) * pop_size)))
    possible_entries <- sample(possible_entries)
    return(possible_entries)
  })
  
  observeEvent(input$simulate, {
    output$histPlot <- renderPlot({
      K <- 5000  # Number of simulations 
      population <- population_result()  # Get updated population
      
      # choose sampling function based on checkbox
      sampler_fn <- if (isTRUE(input$use_succession)) succession_phat_fn else sample_get_phat_fn
      
      # Run simulation
      simulation <- replicate(K, sampler_fn(population, input$n))
      
      title <- sprintf("Histogram of p-hat values from experiment with %sx%s samples%s", 
                       K, input$n, ifelse(isTRUE(input$use_succession), " (succession-adjusted)", ""))
      
      # set Normal approx mean and SE depending on estimator
      if (isTRUE(input$use_succession)) {
        mean_norm <- (input$p * input$n + 1) / (input$n + 2)   # E[(X+1)/(n+2)] = (np+1)/(n+2)
        SE <- sqrt((input$p * (1 - input$p)) / (input$n + 2))  # approximate variance for adjusted estimator
      } else {
        mean_norm <- input$p
        SE <- sqrt((input$p*(1-input$p))/input$n)
      }

      # Normal PDF
      x_vals = seq(0, 1, length.out = 1000)
      normal_pdf_df <- data.frame(
        x = x_vals,
        prob = dnorm(x_vals, mean = mean_norm, sd = SE)
      )
      
      # Plot the histogram with the Normal density:
      ggplot(data=as.data.frame(simulation), aes(x=simulation)) +
        geom_histogram(aes(y=..density..), 
                       breaks=seq(0,1,0.01), alpha=0.5, color=4, fill="white") +
        geom_line(
          data = normal_pdf_df,
          aes(x=x, y=prob),
          color = "darkred"
        ) +
        geom_vline(aes(xintercept=input$p), color="red") +
        xlab("Sample proportion") +
        ggtitle(title) +
        xlim(c(0,1))
      
    })
  })
}

# ------ Functions ---------

sample_get_phat_fn <- function(population, n) {
  sampled_entries <- sample(population, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}

succession_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- (sum(sampled_entries == "support")+1) / (n+2)
  return(phat)
}

