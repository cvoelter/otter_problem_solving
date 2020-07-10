library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp1/data/160628 Otter multiaccess box_data 2016attempts.txt", header = T, sep = "\t")

str(all.data)

xdata <- subset(all.data, Step != 4)
summary(xdata)

hist(xdata$Latency)
hist(xdata$Step)

# box-cox power transformation using library car
trans<-powerTransform(xdata$Latency)$lambda
xdata$log.Latency<-((xdata$Latency^trans-1)/trans)
hist(xdata$log.Latency)


xdata$z.step <- as.vector(scale(xdata$Step))
xdata$z.trial <- as.vector(scale(xdata$Trial_within_step))

contr <- lmerControl(optCtrl = list(maxfun = 10000000))

full <- lmer(log.Latency ~ z.step + z.trial +
  (1 + z.step + z.trial || Subject),
REML = F, data = xdata, control = contr
)

ranef.diagn.plot(full)

null <- lmer(log.Latency ~ 1 +
  (1 + z.step + z.trial || Subject),
REML = F, data = xdata, control = contr
)

wt.txt(c.tab(anova(null, full, test = "Chisq"), 3))

#       npar      AIC     BIC logLik deviance Chisq    Df Pr(>Chisq)
# null 5.000  -98.292 -83.043 54.146 -108.292    NA    NA         NA
# full 7.000 -102.430 -81.081 58.215 -116.430 8.137 2.000      0.017



diagnostics.plot(full)


tests <- as.data.frame(drop1(full, test = "Chisq", control = contr))
wt.txt(c.tab(tests, 3))
#          npar      AIC   LRT Pr(Chi)
# <none>     NA -102.430    NA      NA
# z.step  1.000 -104.382 0.047   0.828
# z.trial 1.000  -96.310 8.120   0.004


wt.txt(c.tab(summary(full)$coefficients, 3))
#             Estimate Std. Error t value
# (Intercept)    1.150      0.052  22.181
# z.step        -0.004      0.019  -0.220
# z.trial       -0.169      0.035  -4.813

wt.txt(c.tab(as.data.frame(summary(full)$varcor), 3))
# grp              var1 var2  vcov sdcor
# Subject   (Intercept)   NA 0.010 0.100
# Subject.1      z.step   NA 0.001 0.025
# Subject.2     z.trial   NA 0.004 0.060
# Residual           NA   NA 0.024 0.156

## CI
CI.full <- confint.merMod(object = full)

wt.txt(c.tab(CI.full, 3))

#              2.5 % 97.5 %
# .sig01       0.053  0.251
# .sig02       0.000  0.092
# .sig03       0.022  0.166
# .sigma       0.140  0.176
# (Intercept)  1.018  1.280
# z.step      -0.059  0.041
# z.trial     -0.264 -0.087

# Model stability

m.stab <- glmm.model.stab(model.res = full, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))
#                          orig    min    max
# (Intercept)             1.150  1.093  1.176
# z.step                 -0.004 -0.020  0.005
# z.trial                -0.169 -0.213 -0.148
# Subject@(Intercept)@NA  0.100  0.010  0.115
# Subject@z.step@NA       0.025  0.000  0.034
# Subject@z.trial@NA      0.060  0.029  0.071
# Residual                0.156  0.150  0.161

# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])

xres <- lm(log.Latency ~ z.trial + z.step, data = xdata)
vif(xres)

#z.trial  z.step 
#1.11617 1.11617 