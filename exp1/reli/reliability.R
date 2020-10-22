library(psych)
library(readxl)

rel_data <- read_xlsx("exp1/reli/160628 Otter multiaccess box_data 2016attempts_reli.xlsx", sheet = 2)

# success

mean(rel_data$match_success)

cohen.kappa(cbind(rel_data$Success,rel_data$Success_reli))

# Method

mean(rel_data$match_method)

cohen.kappa(cbind(rel_data$Success_Method,rel_data$Success_Method_reli))

# transitions

mean(rel_data$match_trans)

cohen.kappa(cbind(rel_data$Number_transitions ,rel_data$Number_transitions_reli))

#Spearman correlation:
cor.test(rel_data$Number_transitions ,rel_data$Number_transitions_reli, method = "spearman", use ="complete.obs")
