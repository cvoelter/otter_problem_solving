library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp1/data/160628 Otter multiaccess box_data 2016attempts.txt", header = T, sep = "\t")

str(all.data)

xdata <- subset(all.data, Subject != "Twoface") # did not reach phase 3/4
xdata <- subset(xdata, Step == 3 | Step == 4)
xdata <- subset(xdata, trials_step34 > 0)
summary(xdata)

hist(xdata$Latency)
hist(xdata$Step)

xx.fe.re <- fe.re.tab(
  fe.model = "transitions~flexible+trials_step34+Method", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary
ydata <- xx.fe.re$data

ydata$z.flexible <- as.vector(scale(ydata$flexible))
ydata$z.trials_step34 <- as.vector(scale(ydata$trials_step34))


#GLMM:
contr <- glmerControl(optCtrl = list(maxfun = 10000000))

full <- glmer(transitions ~ z.flexible + z.trials_step34 +
  (1 + z.flexible + z.trials_step34 || Subject),
family = poisson, data = ydata, control = contr
)

overdisp.test(full)
# 1.941738

full.nb <- glmer.nb(transitions ~ z.flexible + z.trials_step34 +
  (1 + z.flexible + z.trials_step34 || Subject),
data = ydata, control = contr
)

overdisp.test(full.nb)
#1.480513

null.nb <- glmer.nb(transitions ~ 1 +
                (1 + z.flexible + z.trials_step34 || Subject),
              family = poisson, data = ydata, control = contr
)

wt.txt(c.tab(anova(null.nb, full.nb, test = "Chisq"), 3))
#          npar     AIC     BIC  logLik deviance Chisq    Df Pr(>Chisq)
# null.nb 5.000 116.808 122.699 -53.404  106.808    NA    NA         NA
# full.nb 7.000 113.388 121.635 -49.694   99.388 7.420 2.000      0.024


tests <- as.data.frame(drop1(full.nb, test = "Chisq", control = contr))
wt.txt(c.tab(tests, 3))
#                  npar     AIC   LRT Pr(Chi)
# <none>             NA 113.388    NA      NA
# z.flexible      1.000 118.373 6.985   0.008
# z.trials_step34 1.000 112.066 0.678   0.410


wt.txt(c.tab(summary(full.nb)$coefficients, 3))
#                 Estimate Std. Error z value Pr(>|z|)
# (Intercept)        1.032      0.157   6.588    0.000
# z.flexible        -0.508      0.156  -3.265    0.001
# z.trials_step34   -0.189      0.214  -0.881    0.378


wt.txt(c.tab(as.data.frame(summary(full.nb)$varcor), 3))
# grp                  var1 var2  vcov sdcor
# Subject       (Intercept)   NA 0.000 0.000
# Subject.1      z.flexible   NA 0.000 0.000
# Subject.2 z.trials_step34   NA 0.076 0.276


## CI

boot.res <- boot.glmm(
  model.res = full.nb, excl.warnings = T,
  nboots = 1000, para = T
)
wt.txt(c.tab(boot.res))
#                       orig      X2.5.     X97.5.
# (Intercept)      1.0322058  0.6434752  1.2900732
# z.flexible      -0.5082345 -0.8309759 -0.2257811
# z.trials_step34 -0.1886333 -0.6540498  0.2687990



#Model stability

m.stab <- glmm.model.stab(model.res = full.nb, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))
#                              orig    min    max
# (Intercept)                 1.032  0.936  1.178
# z.flexible                 -0.508 -0.506 -0.501
# z.trials_step34            -0.189 -0.307  0.095
# Subject@(Intercept)@NA      0.000  0.000  0.042
# Subject@z.flexible@NA       0.000  0.000  0.000
# Subject@z.trials_step34@NA  0.276  0.000  0.346



# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])

#Colinearity
xres <- lm(transitions ~ z.flexible + z.trials_step34, data = ydata)

vif(xres)

# z.flexible z.trials_step34
#           1              1
