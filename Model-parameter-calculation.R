library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(readxl)
library(msm)

input_estimates <- data.frame(variable = c(
  "Knowledge_generation",
  "Activities_on_market",
  "Knowledge_spreading",
  "Reaching_customers",
  "Running_costs",
  "Market_income",
  
  "Health_investment_reduction",
  "SQ_Health_investment_reduction",
  "SQ_Cheap_Food",
  "Workforce",
  "SQ_Workforce",
  
  "var_slight", "discount_rate",
  "payout_months", "investment_months",
  "failure_risk","SQ_failure_risk"),
  lower = c(4000,5000,3000,
            5000,8000,8000,500,1,
            1000,1000,1,
            1,1,10,5,0.06,0.04),
  median = NA,
  upper = c(5000,6000,4000,
            6000,9000,9000,800,40,
            2000,2000,20,
            1,1,10,5,0.06,0.04),
  distribution = c("posnorm", "posnorm",
                   "posnorm","posnorm","posnorm",
                   "posnorm","posnorm","posnorm",
                   "posnorm","posnorm","posnorm",
                   "const","const","const","const",
                   "const","const"),
  label = c("Knowledge generation investment (Dollar/Month)",
            "Knowledge payoff (Dollar/Month)",
            "Marketing investment (Dollar/Month)",
            "Marketing payoff (Dollar/Month)",
            "Running costs (Dollar/Month)",
            "Market income (Dollar/Month)",
            "Health after intervention (Dollar/Month)",
            "Health without intervention (Dollar/Month)",
            "Low priced food (Dollar/Month)",
            "Workforce after intervention(Dollar/Month)",
            "Workforce without intervention (Dollar/Month)",
            "Coefficient of variation",
            "Discout rate",
            "Months of receiving money (Dollar/Month)",
            "Months of paying into empowerment efforts (Dollar/Month)",
            "Risk failure", "Risk failure SQ"),
  Description = c("Education investment", "try",
                  "Education payout",
                  "Marketing investment",
                  "Marketing payout",
                  "Running costs",
                  "Market income",
                  "Health after intervention",
                  "Health without intervention",
                  "Low priced food",
                  "Workforce",
                  "Workforce SQ",
                  "Discout rate",
                  "Months of receiving money",
                  "Months of paying into empowerment efforts",
                  "Risk failure", "Risk failure SQ"))


input_estimates <- input_estimates %>%
  mutate(variable = as.character(variable),
         distribution = as.character(distribution),
         label = as.character(label),
         Description = as.character(Description))


failure <- chance_event((1-0.04), 1, 0,
                        n = (24 + 3))

Knowledge_generation <- c(vv(var_mean = 500, 
                             var_CV = 1, 
                             n = 3))

Activities_on_market <- c(rep(0,100), 
                          vv(var_mean = 100, 
                             var_CV = 1, 
                             n = 10))

input_estimates$failure_risk
