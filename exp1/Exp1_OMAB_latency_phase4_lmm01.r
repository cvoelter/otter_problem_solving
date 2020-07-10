library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp1/data/160628 Otter multiaccess box_data 2016attempts.txt", header = T, sep = "\t")

str(all.data)

xdata <- subset(all.data, Subject != "Twoface")
xdata <- subset(xdata, Step == 4)
summary(xdata)

hist(xdata$Latency)

# box-cox power transformation using library car
trans <- powerTransform(xdata$Latency)$lambda
xdata$log.Latency <- ((xdata$Latency^trans - 1) / trans)
hist(xdata$log.Latency)

xx.fe.re <- fe.re.tab(
  fe.model = "log.Latency~preexperience+Trial_within_step+Method", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary


xdata$z.preexperience <- as.vector(scale(xdata$preexperience))
xdata$z.Trial_within_step <- as.vector(scale(xdata$Trial_within_step))


contr <- lmerControl(optCtrl = list(maxfun = 10000000))

full <- lmer(log.Latency ~ z.preexperience + z.Trial_within_step +
  (1 + z.preexperience + z.Trial_within_step || Subject),
REML = F, data = xdata, control = contr
)

ranef.diagn.plot(full)


null <- lmer(log.Latency ~ 1 +
  (1 + z.preexperience + z.Trial_within_step || Subject),
REML = F, data = xdata, control = contr
)

wt.txt(c.tab(anova(null, full, test = "Chisq"), 3))

#       npar     AIC     BIC  logLik deviance Chisq    Df Pr(>Chisq)
# null 5.000 107.349 115.267 -48.675   97.349    NA    NA         NA
# full 7.000 106.419 117.504 -46.210   92.419 4.930 2.000      0.085

diagnostics.plot(full)

tests <- as.data.frame(drop1(full, test = "Chisq", control = contr))
wt.txt(c.tab(tests, 3))
#                      npar     AIC   LRT Pr(Chi)
# <none>                 NA 106.419    NA      NA
# z.preexperience     1.000 108.487 4.068   0.044
# z.Trial_within_step 1.000 105.309 0.889   0.346


wt.txt(c.tab(summary(full)$coefficients, 3))
#                     Estimate Std. Error t value
# (Intercept)            2.766      0.219  12.628
# z.preexperience       -0.481      0.164  -2.939
# z.Trial_within_step   -0.146      0.144  -1.017

wt.txt(c.tab(as.data.frame(summary(full)$varcor), 3))
# grp                      var1 var2  vcov sdcor
# Subject           (Intercept)   NA 0.087 0.295
# Subject.1     z.preexperience   NA 0.021 0.146
# Subject.2 z.Trial_within_step   NA 0.003 0.055
# Residual                   NA   NA 0.685 0.828



## CI
CI.full <- confint.merMod(object = full)

wt.txt(c.tab(CI.full, 3))

#                      2.5 % 97.5 %
# .sig01               0.000  1.095
# .sig02               0.000  0.800
# .sig03               0.000  0.693
# .sigma               0.649  1.091
# (Intercept)          2.155  3.378
# z.preexperience     -0.937 -0.024
# z.Trial_within_step -0.548  0.255

source("C:/Users/cjv3/R/R scripts/Roger/glmm_stability.r")
m.stab <- glmm.model.stab(model.res = full, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))
#                                  orig    min    max
# (Intercept)                     2.766  2.504  2.946
# z.preexperience                -0.481 -0.639 -0.311
# z.Trial_within_step            -0.146 -0.311 -0.061
# Subject@(Intercept)@NA          0.295  0.000  0.373
# Subject@z.preexperience@NA      0.146  0.000  0.290
# Subject@z.Trial_within_step@NA  0.055  0.000  0.196
# Residual                        0.828  0.618  0.882


# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])

#Colinearity
xres <- lm(log.Latency ~ z.preexperience + z.Trial_within_step, data = xdata)

vif(xres)

#    z.preexperience z.Trial_within_step
#          1.003509            1.003509
