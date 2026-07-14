# Set the working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load the original .csv file containing survey data results
data_og <- read.csv("lecture_1_classroom_survey.csv")

# Remove unneeded columns (also anonymizes the data)
data_clean <- subset(data_og, select = -c(1:11))
head(data_clean)

# Rename the remaining columns to be more useful
colnames(data_clean) <- c("sleep", "bedtime", "countries",
                          "caffeine", "class", "zodiac", 
                          "intro_extra")

# Instead of A/B for introverted/extroverted, replace with the words
data_clean$`intro_extra`[data_clean$`intro_extra` == "A"] <- "introverted"
data_clean$`intro_extra`[data_clean$`intro_extra` == "B"] <- "extraverted"

# Save the now-clean and -anonymized data into a new .csv file
write.csv(data_clean, "lecture_1_survey_data_clean.csv", row.names=FALSE)
