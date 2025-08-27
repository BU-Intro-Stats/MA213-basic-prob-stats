library(shiny)
source("Lecture16_DemoFunctions.R")

# ---- UI ----
ui <- fluidPage(
  titlePanel("CLT Demonstration for p-hat"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min=0, max=500, value=10, step=10),
      sliderInput("p", "Population Proportion (p):", min=0.01, max=1, 
                  value=0.25, step=0.05),
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
  
  # TODO: add checkbox for adjusted estimators (small sample sizes)
  observeEvent(input$simulate, {
    output$histPlot <- renderPlot({
      K <- 1000  # Number of simulations 
      population <- population_result()  # Get updated population
      
      # Run simulation
      simulation <- replicate(K, sample_get_phat_fn(population, input$n))
      title <- sprintf("Histogram of p-hat values from experiment with %sx%s samples", 
                       K, input$n)
      
      bw <- 0.01
      n_obs <- length(simulation)
      SE <- sqrt((input$p*(1-input$p))/input$n)

      # Create & display histogram plot
      ggplot(data=as.data.frame(simulation), aes(x=simulation)) +
        geom_histogram(binwidth=bw, alpha=0.5, color=4, fill="white") +
        stat_function(fun = function(x) 
          dnorm(x, mean=input$p, sd=SE)*bw*n_obs) +
        xlab("Sample proportion") +
        ggtitle(title) +
        xlim(c(0,1))
    })
  })
}
