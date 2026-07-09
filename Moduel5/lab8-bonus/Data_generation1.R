set.seed(123)

# Create a sample size
n <- 120

# Randomly assign students to majors
majors <- c("Math", "Biology", "History", "Engineering")
student_major <- sample(majors, n, replace = TRUE)

# Generate GPAs (simulate slightly different distributions for each major)
student_gpa <- sapply(student_major, function(mj) {
  if (mj == "Engineering") {
    rnorm(1, mean = 3.0, sd = 0.4)
  } else if (mj == "Math") {
    rnorm(1, mean = 3.3, sd = 0.3)
  } else if (mj == "Biology") {
    rnorm(1, mean = 3.1, sd = 0.35)
  } else { # History
    rnorm(1, mean = 3.2, sd = 0.25)
  }
})

# Keep GPAs within realistic bounds
student_gpa <- pmax(pmin(student_gpa, 4.0), 0.0)

# Add outlier GPAs for a few students
student_gpa[c(10, 50)] <- 0.1   # Very low outliers
student_gpa[c(25, 75)] <- 4.3   # High outliers (will exceed normal bounds)

# Insert NA values randomly
na_indices <- sample(1:n, 5)
student_gpa[na_indices] <- NA

# Optionally add some names
student_name <- paste("Student", 1:n)

# Create data frame
student_df <- data.frame(
  Name = student_name,
  Major = student_major,
  GPA = round(student_gpa, 2)
)

# Save to CSV
write.csv(student_df, "student_gpa.csv", row.names = FALSE)

# View the first few rows
head(student_df)

library(ggplot2)
ggplot(data=student_df)+geom_histogram(aes(x=GPA))
