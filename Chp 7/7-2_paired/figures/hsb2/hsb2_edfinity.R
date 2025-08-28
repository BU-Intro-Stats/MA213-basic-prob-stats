# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Load data
data <- read.csv("hsb2.csv")

# Grab just the reading and writing scores for this analysis
scores <- data %>% select(c(read, write))

# Convert to long format
scores_long <- scores %>% pivot_longer(
  cols=c("read", "write"), 
  names_to="scoreType",
  values_to="score"
)

ggplot(data=scores_long, aes(x=score, fill=scoreType)) +
  geom_histogram(alpha=0.6, binwidth=3)

# Save figure for Edfinity quiz, lecture
ggsave("hsb2_read_write_scores_hist.png")

# Save filtered data to lecture folder for R demos
write_csv(scores, "scores.csv")
