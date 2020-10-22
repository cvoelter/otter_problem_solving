library(psych)
library(readxl)
library(tidyverse)

rel_data <- read_xlsx("exp2/reli/WOOF_data 2016_reli.xlsx", sheet = 2)

rel_data <- rel_data%>%
  mutate(match_success = ifelse(Success == success_reli,1,0))

# success

mean(rel_data$match_success)

cohen.kappa(cbind(rel_data$Success,rel_data$success_reli))

# number turns

cor.test(rel_data$Number_of_turns, rel_data$Number_of_turns_reli)


# number latency

cor.test(rel_data$Latency, rel_data$Latency_reli)
