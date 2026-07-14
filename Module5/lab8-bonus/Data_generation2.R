set.seed(42)

# Simulate n students
n <- 100

# Study hours for each student (between 0 and 16)
study_hours <- round(runif(n, min=0, max=16), 1)

# Exam scores based on study_hours with some noise
exam_score <- round(50 + 3.5 * study_hours + rnorm(n, mean=0, sd=10), 1)
exam_score <- pmin(pmax(exam_score, 0), 100) # Ensure scores between 0 and 100

# Create data frame
exam_performance <- data.frame(
  StudyHours = study_hours,
  ExamScore = exam_score
)

# Save as CSV if desired
write.csv(exam_performance, "exam_performance.csv", row.names=FALSE)

head(exam_performance)