library(reshape2)
library(data.table)

## Response codes: 1=Socially Acceptable, 2=Not Socially Acceptable

questions <- c(
  "Lying about your income.",
  "Lying on an insurance claim.",
  "Lying on a tax return.",
  "Lying about your age on a dating website.",
  "Lying about your age to secure alcohol or other controlled product.",
  "Lying about your contact information to avoid further interactions with someone.",
  "Lying to a sexual partner, about a sexually transmitted disease.",
  "Lying to a law enforcement officer, to protect yourself.",
  "Lying to a law enforcement officer, to protect a friend or family member.",
  "Lying to a sexual partner, about past sexual activity.",
  "Lying to your friends about your sexual activity.",
  "Lying to a police officer about your speed at a traffic stop.",
  "Lying to a child, to avoid a difficult conversation.",
  "Lying to a child, to maintain a tradition (such as Santa Claus or the Easter Bunny).",
  "Lying about a food allergy at a restaurant, to avoid an ingredient you dislike.",
  "Lying to a partner or friend about their appearance.",
  "Lying about an illness, to take a day off work.",
  "Lying to a friend to protect their feelings.",
  "Embellishing minor characteristics, while on a date.",
  "Lying on a resume or in a job interview.")

binom.pvalue <- function(x, n) {
  return(binom.test(x, n)$p.value)
}

# Load Qualtrics response data
raw <- fread("data/presurvey_raw.csv")
responses <- raw[DistributionChannel == "anonymous", c(9, 18:37)]

# Reshape to long format
responses_long <- melt(responses, "ResponseId")

# Aggregate results for each question
summary <- responses_long[, .("response_1"=sum(value==1), "response_2"=sum(value==2)), by=variable]
summary[, c("total_responses", "preferred_response") := list(response_1+response_2, ifelse(response_1 > response_2, 1, 2))]

# Run a binomial test on all Q's and get the p value and significance flag
summary[, p_value:=mapply(binom.pvalue, response_1, total_responses)]
summary[, significant_05 := p_value < 0.05]
summary[, "question" := questions]

write.csv(summary, "data/presurvey_results.csv")
