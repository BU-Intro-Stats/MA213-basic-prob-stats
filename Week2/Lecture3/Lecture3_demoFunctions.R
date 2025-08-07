set.seed(42)

generate_data1 <- function() {
  x1 <- rchisq(100, 3)
  x2 <- 20-rchisq(100, 2)
  x3 <- c(rchisq(60, 5.8), rnorm(40, 16.5, 2))
  x4 <- c(rchisq(20, 3), rnorm(35, 12), rnorm(45, 18, 1.5))
  x5 <- runif(100,0,20)
  
  values = c(x1, x2, x3, x4, x5)
  datasets = c(rep("A", 100), rep("B", 100), rep("C", 100), 
               rep("D", 100), rep("E", 100))
  
  data <- do.call(rbind, Map(data.frame, x=values, dataset=datasets))
  return(data)
}

generate_data2 <- function() {
  x1 <- rnorm(100, 0, 1)
  x2 <- rnorm(100, 0, 5)
  x3 <- rnorm(100, 0, 10)
  
  values = append(append(x1, x2), x3)
  datasets = c(rep("F", 100), rep("G", 100), rep("H", 100))
  
  data <- do.call(rbind, Map(data.frame, x=values, dataset=datasets))
  return(data)
}

generate_data3 <- function() {
  x1 <- rchisq(100, 2)/2
  x2 <- rchisq(100, 2)*2
  x3 <- rchisq(100, 2)*5
  
  values = append(append(x1, x2), x3)
  datasets = c(rep("I", 100), rep("J", 100), rep("K", 100))
  
  data <- do.call(rbind, Map(data.frame, x=values, dataset=datasets))
  return(data)
}



# ---- Plotting functions

histograms_plusminus2sd <- function(df) {
  library(dplyr)
  library(ggplot2)
  
  # Compute mean, sd, segment start/end
  summary_df <- df %>%
    group_by(dataset) %>%
    summarise(
      m = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)
    ) %>%
    mutate(
      xstart = m - 2*sd,
      xend = m + 2*sd
    )
  
  # histogram with segment
  p <- ggplot(df, aes(x = x, fill = as.factor(dataset))) +
    geom_histogram() +
    facet_wrap(~dataset, ncol = 1) +
    geom_segment(
      data = summary_df,
      aes(x = xstart, xend = xend, y = -5, yend = -5),
      color = "black",
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = summary_df,
      aes(x = m, y = -5),
      color = "black",
      shape = 4, # shape 4 is an "x"
      size = 1,
      stroke = 1,
      inherit.aes = FALSE
    ) +
    guides(fill = "none")+
    ylab("count")
  
  return(p)
}


histograms_plusIQR <- function(df) {
  library(ggplot2)

  p <- ggplot(df, aes(x = x, fill = as.factor(dataset))) +
    geom_histogram() +
    geom_boxplot(aes(group = as.factor(dataset)),  # Place boxplot below x-axis
                 width = 5, 
                 outlier.shape = NA,
                 coef = 0,  # remove the whiskers
                 position = position_nudge(y = -5)
    ) +
    facet_wrap(~dataset, ncol=1) +
    guides(fill = "none")
  
  return(p)
}
