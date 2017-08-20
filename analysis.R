library(reshape2)
library(data.table)
library(stargazer)

####################
# DATA PREPARATION #
####################

# Generate vectors to identify different question types
presurvey_results <- fread("data/presurvey_results.csv")
q_positive <- presurvey_results[preferred_response == 1 & significant_05, as.character(variable)]
q_negative <- presurvey_results[preferred_response == 2 & significant_05, as.character(variable)]
q_selected <- c("Q1",  "Q2",  "Q3",  "Q5",  "Q6",  "Q7",  "Q9",  "Q10", 
                "Q11", "Q13", "Q14", "Q15", "Q16", "Q18", "Q19", "Q20")

# Load the responses
main_responses_long = fread("data/main_responses_long.csv")
main_responses_wide = fread("data/main_responses_wide.csv")

# Aggregate per-respondent answers to question types
main_responses_wide[, yes_all := rowSums(.SD), .SDcols=q_selected]
main_responses_wide[, yes_pos := rowSums(.SD), .SDcols=q_positive]
main_responses_wide[, yes_neg := rowSums(.SD), .SDcols=q_negative]

# Flag individual question answers with question type
questions_long = main_responses_long[
  variable %in% q_selected, 
  .(ResponseId, variable, value, "negative"=as.integer(variable %in% q_negative))
  ]

# ANALYSIS ONE: IMPACT OF TIMING
# Q1: Is there a difference in email disclosure between pre and post?
a1.q1 <- lm(email ~ email_timing, main_responses_wide)
summary(a1.q1)
# Insignificant difference, but it is in the correct direction (slightly less disclosure in the post group)

# Q2: Is there a difference in "Yes" answers between pre and post?
a1.q2 <- lm(yes_all ~ email_timing, main_responses_wide)
summary(a1.q2)
# Same

# Q3: Is there a difference in "Yes" answers on invasive questions
#     between pre and post?
a1.q3 <- lm(yes_neg ~ email_timing, main_responses_wide)
summary(a1.q3)
# Opposite expected direction and WAY not significant.

# ANALYSIS TWO: IMPACT OF QUESTIONS ON ANSWERS
# Q1: Does the invasiveness of a question impact the number of yes 
#     answers it receives?

a2.q1 <- lm(value ~ negative, questions_long)
summary(a2.q1)
# V significant (the only one), medium effect size (R^2).
# Definitely fewer yes answers on socially undesirable behaviors.
# Unclear whether people don't do them or just don't admit to them.

# ANALYSIS THREE: IMPACT OF QUESTIONS ON DISCLOSURE
# Q1: In the post group, is there a difference in email disclosure caused 
#     by the number of "Yes" answers?
a3.q1 <- lm(email ~ yes_all, main_responses_wide[email_timing==1])
summary(a3.q1)
# Insignificant, v. small effect, 

# Q2: In the post group, is there a difference in email disclosure caused
#     by the number of invasive questions answered "Yes"?
a3.q2 <- lm(email ~ yes_neg, main_responses_wide[email_timing==1])
summary(a3.q2)

# ANALYSIS FOUR: IMPACT OF DISCLOSURE ON QUESTIONS
# Q1: In the pre group, is there a difference in number of "Yes" answers
#     caused by email disclosure?
a4.q1 <- lm(yes_all ~ email, main_responses_wide[email_timing==0])
summary(a4.q1)
# Nope


# Q2: In the pre group, is there a difference in number of "Yes" answers
#     to invasive questions caused by email disclosure?
a4.q2 <- lm(yes_neg ~ email, main_responses_wide[email_timing==0])
summary(a4.q2)
# Noooooope
