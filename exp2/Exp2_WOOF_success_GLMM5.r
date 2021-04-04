library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp2/data/WOOF_data 2016.txt", header = T, sep = "\t")
str(all.data)

xdata <- all.data
xdata <- subset(xdata, Phase > 4)
xdata <- subset(xdata, Number_of_turns > 0)
summary(xdata)


xx.fe.re <- fe.re.tab(
  fe.model = "Number_of_turns~con2+Trial+Session_cont", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary


ydata <- xx.fe.re$data


# centering
ydata$con2.response_window_large.c <- ydata$con2.response_window_large - mean(ydata$con2.response_window_large)
ydata$z.Session_cont <- as.vector(scale(ydata$Session_cont))
ydata$z.Trial <- as.vector(scale(ydata$Trial))

contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000))

full <- glmer(Number_of_turns ~ con2 + z.Trial + z.Session_cont +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
family = poisson, data = ydata, control = contr
)

overdisp.test(full) #--> 2.649811


full.nb <- glmer.nb(Number_of_turns ~ con2 + z.Trial + z.Session_cont +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
data = ydata, control = contr
)

overdisp.test(full.nb) #-->0.9501975


null.nb <- glmer.nb(Number_of_turns ~ 1 +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
data = ydata, control = contr
)

wt.txt(c.tab(anova(null.nb, full.nb, test = "Chisq"), 3))
#            Df     AIC     BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)
# null.nb 6.000 715.260 732.736 -351.630  703.260     NA     NA         NA
# full.nb 9.000 704.049 730.263 -343.025  686.049 17.211  3.000      0.001

tests <- as.data.frame(drop1(full.nb, test = "Chisq", control = contr))
wt.txt(c.tab(tests, 3))
#                   Df     AIC    LRT Pr(Chi)
# <none>            NA 704.049     NA      NA
# con2           1.000 712.741 10.692   0.001
# z.Trial        1.000 706.370  4.321   0.038
# z.Session_cont 1.000 703.198  1.149   0.284

wt.txt(c.tab(summary(full.nb)$coefficients, 3))
#                           Estimate Std. Error z value Pr(>|z|)
# (Intercept)                  2.054      0.170  12.066    0.000
# con2response_window_large   -1.051      0.201  -5.237    0.000
# z.Trial                     -0.160      0.077  -2.091    0.037
# z.Session_cont              -0.218      0.191  -1.141    0.254

wt.txt(c.tab(as.data.frame(summary(full.nb)$varcor), 3))
# grp                               var1 var2  vcov sdcor
# Subject                    (Intercept)   NA 0.060 0.245
# Subject.1               z.Session_cont   NA 0.097 0.312
# Subject.2                      z.Trial   NA 0.000 0.000
# Subject.3 con2.response_window_large.c   NA 0.000 0.000

## CI
boot.res <- boot.glmm(
  model.res = full.nb, excl.warnings = T,
  nboots = 1000, para = T
)
wt.txt(c.tab(boot.res))
#                                   orig        X2.5.       X97.5.
# (Intercept)                2.054273664  1.716907763  2.390948152
# con2response_window_large -1.051108413 -1.481060505 -0.703857928
# z.Trial                   -0.160238238 -0.305993087 -0.002441655
# z.Session_cont            -0.217662068 -0.643597667  0.102947364



# Model stability
m.stab <- glmm.model.stab(model.res = full.nb, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))

#                                          orig    min    max
# (Intercept)                             2.054  1.927  2.198
# con2response_window_large              -1.051 -1.106 -0.952
# z.Trial                                -0.160 -0.185 -0.154
# z.Session_cont                         -0.218 -0.397 -0.104
# Subject@(Intercept)                     0.245  0.402  0.527
# Subject.1@z.Session_cont                0.312  0.000  0.603
# Subject.2@z.Trial                       0.000  0.000  0.000
# Subject.3@con2.response_window_large.c  0.000  0.000  0.000

# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])

# Colinearity
xres <- lm(Number_of_turns ~ con2 + z.Trial + z.Session_cont, data = ydata)
vif(xres)