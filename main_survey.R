library(reshape2)
library(data.table)

# Load the results of the presurvey
presurvey_results <- read.csv("data/presurvey_results.csv")

# Set up vectors of socially acceptable (q_positive) and unacceptable (q_negative) questions.
# These will help filter down columns in the main results for later analysis.
q_positive <- summary[preferred_response == 1 & significant_05, as.character(variable)]
q_negative <- summary[preferred_response == 2 & significant_05, as.character(variable)]

# Load the raw Qualtrics data. I did have to manually edit one column name because
# there were to marked "Q19", one of which is now "email"
main_raw <- fread("data/main_raw.csv")

# Only retain columns useful for this analysis, and only rows originating from mturk,
columns <- c("ResponseId", "email_timing", "email", 
             "Q1", "Q2", "Q3", "Q5", "Q6", "Q7", 
             "Q9", "Q10", "Q11", "Q13", "Q14",
             "Q15", "Q16", "Q18", "Q19", "Q20")
main_responses <- main_raw[DistributionChannel == "anonymous", columns, with=FALSE]

# Convert email question timing to 0 (pre) or 1 (post)
main_responses[, email_timing:= as.integer(email_timing == "post")]

# Don't care about the actual addresses, convert to 0 (no email provided) or 1 (email provided)
main_responses[, email := as.integer(email != "")]

# Melt to long format. May be useful in analysis, but mostly it was useful to 
# easily change all the 2s to 0s.
main_responses_long <- melt(main_responses, "ResponseId")
main_responses_long[, value := as.integer(value)]
# Convert all 2 values to 0, so the Q answers are all 0 (No) or 1 (Yes)
main_responses_long[value == 2, value := 0]

# Recast to wide format
main_responses_wide <- dcast(main_responses_long, ResponseId ~ variable)

# Output both formats for later use
write.csv(main_responses_long, "data/main_responses_long.csv")
write.csv(main_responses_wide, "data/main_responses_wide.csv")

