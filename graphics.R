library(ggplot2)
library(data.table)

questions_long[, .("value"=sum(value, na.rm=TRUE)), by=variable]
q_all <- c("Q1",  "Q2",  "Q3",  "Q4",  "Q5",
           "Q6",  "Q7",  "Q8",  "Q9",  "Q10",
           "Q11", "Q12", "Q13", "Q14", "Q15", 
           "Q16", "Q17", "Q18", "Q19", "Q20")

color_pos <- "#3333CC"
color_neg <- "#CC3333"

# Reponse rate to questions, colored by type
ggplot(
  questions_long[, .("value"=sum(value, na.rm=TRUE)), by=variable], 
  aes(factor(variable, levels=q_all), value, fill=(variable %in% q_positive)), fontface="Times"
  ) +
  geom_col() +
  scale_fill_manual(name="Question Type", values=c(color_neg, color_pos), labels=c("Socially Unacceptable", "Socially Acceptable")) +
  labs(x="Question", y="Yes answers") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Presurvey results
ggplot(presurvey_results) +
  geom_col(aes(factor(variable, levels=q_all), response_1, alpha=(significant_05 & preferred_response == 1), fill="pos")) +
  geom_col(aes(factor(variable, levels=q_all), response_2*-1, alpha=(significant_05 & preferred_response == 2), fill="neg")) +
  scale_y_continuous(labels=abs) +
  scale_alpha_discrete(name="p < .05", range=c(0.5, 1.0)) +
  scale_fill_manual(name="Question Type", values=c(pos=color_pos, neg=color_neg), labels=c(pos="Socially Acceptable", neg="Socially Unacceptable")) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Question", y="Presurvey Responses")
