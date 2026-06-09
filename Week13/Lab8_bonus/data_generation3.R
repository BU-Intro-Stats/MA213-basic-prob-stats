set.seed(2024)
n <- 200
departments <- c("HR", "Engineering", "Sales", "Marketing")

department <- sample(departments, n, replace = TRUE, 
                     prob = c(0.2, 0.4, 0.25, 0.15))
promotion_status <- mapply(function(dept) {
  sample(c("Promoted", "Not Promoted", "In Review"), 1,
         prob = c(0.1, 0.8, 0.1))
}, department)

company_df <- data.frame(
  Department = department,
  PromotionStatus = promotion_status
)

write.csv(company_df, "promotion.csv", row.names = FALSE)

# Create the 4 by 3 table
table_department_promotion <- table(company_df$Department, company_df$PromotionStatus)
print(table_department_promotion)