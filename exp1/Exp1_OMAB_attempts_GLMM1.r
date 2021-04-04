library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp1/data/160628 Otter multiaccess box_data 2016attempts.txt", header = T, sep = "\t")

str(all.data)

xdata <- subset(all.data, Step != 4)
xdata <- subset(xdata, Success != 0)
summary(xdata)

hist(xdata$transitions)
hist(xdata$Step)

xx.fe.re <- fe.re.tab(
  fe.model = "transitions~Step+Trial_within_step", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary

ydata <- xx.fe.re$data

ydata$z.step <- as.vector(scale(ydata$Step))
ydata$z.trial <- as.vector(scale(ydata$Trial_within_step))


contr <- glmerControl(optCtrl = list(maxfun = 10000000))

full <- glmer(transitions ~ z.step + z.trial +
  (1 + z.step + z.trial || Subject),
family = poisson, data = ydata, control = contr
)

overdisp.test(full)



null <- glmer(transitions ~ 1 +
  (1 + z.step + z.trial || Subject),
family = poisson, data = ydata, control = contr
)

wt.txt(c.tab(anova(null, full, test = "Chisq"), 3))

#         Df     AIC     BIC   logLik deviance Chisq Chi Df Pr(>Chisq)
# null 4.000 532.038 544.238 -262.019  524.038    NA     NA         NA
# full 6.000 526.729 545.028 -257.364  514.729 9.310  2.000      0.010

tests <- as.data.frame(drop1(full, test = "Chisq", control = contr))
wt.txt(c.tab(tests, 3))

#            Df     AIC   LRT Pr(Chi)
# <none>     NA 526.729    NA      NA
# z.step  1.000 529.744 5.015   0.025
# z.trial 1.000 529.083 4.354   0.037


wt.txt(c.tab(summary(full)$coefficients, 3))
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)    0.535      0.110   4.876    0.000
# z.step         0.224      0.069   3.254    0.001
# z.trial       -0.452      0.171  -2.647    0.008


wt.txt(c.tab(as.data.frame(summary(full)$varcor), 3))
# grp              var1 var2  vcov sdcor
# Subject   (Intercept)   NA 0.025 0.158
# Subject.1      z.step   NA 0.005 0.069
# Subject.2     z.trial   NA 0.085 0.292


#Confidence intervals 
boot.res <- boot.glmm(
  model.res = full, excl.warnings = T,
  nboots = 1000, para = T
)
wt.txt(c.tab(boot.res))

#                    orig       X2.5.      X97.5.
# (Intercept)  0.53500107  0.30585315  0.73467319
# z.step       0.22356986  0.08652201  0.35613092
# z.trial     -0.45171083 -0.76793017 -0.11814461


#Model stability
m.stab <- glmm.model.stab(model.res = full, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))
#                       orig    min    max
# (Intercept)          0.535  0.398  0.595
# z.step               0.224  0.158  0.294
# z.trial             -0.452 -0.667 -0.317
# Subject@(Intercept)  0.158  0.000  0.411
# Subject.1@z.step     0.069  0.000  0.319
# Subject.2@z.trial    0.292  0.491  0.552


# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])

#Colinearity
xres <- lm(transitions ~ z.trial + z.step, data = ydata)

vif(xres)

# z.trial   z.step
# 1.11617 1.11617
