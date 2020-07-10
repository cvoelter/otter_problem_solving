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
summary(xdata)

xx.fe.re <- fe.re.tab(
  fe.model = "Success~con2+Trial+Session_cont", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary
ydata <- xx.fe.re$data

# centering
ydata$con2.response_window_large.c <- ydata$con2.response_window_large - mean(ydata$con2.response_window_large)

ydata$z.Session_cont <- as.vector(scale(ydata$Session_cont))
ydata$z.Trial <- as.vector(scale(ydata$Trial))

contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000))

full <- glmer(Success ~ con2 + z.Trial + z.Session_cont +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
family = binomial, data = ydata, control = contr
)

overdisp.test(full) #-->0.7055027

null <- glmer(Success ~ 1 +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
family = binomial, data = ydata, control = contr
)


wt.txt(c.tab(anova(null, full, test = "Chisq"), 3))
#         Df     AIC     BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# null 5.000 158.353 173.202 -74.177  148.353    NA     NA         NA
# full 8.000 157.568 181.327 -70.784  141.568 6.785  3.000      0.079

tests <- as.data.frame(drop1(full, test = "Chisq", control = contr))
wt.txt(c.tab(tests, 3))
#                   Df     AIC   LRT Pr(Chi)
# <none>            NA 157.568    NA      NA
# con2           1.000 161.005 5.436   0.020
# z.Trial        1.000 155.981 0.412   0.521
# z.Session_cont 1.000 156.328 0.760   0.383


wt.txt(c.tab(summary(full)$coefficients, 3))
#                           Estimate Std. Error z value Pr(>|z|)
# (Intercept)                 -1.264      0.626  -2.017    0.044
# con2response_window_large    2.354      0.797   2.955    0.003
# z.Trial                      0.151      0.227   0.665    0.506
# z.Session_cont              -0.745      0.850  -0.876    0.381


wt.txt(c.tab(as.data.frame(summary(full)$varcor), 3))
# grp                               var1 var2  vcov sdcor
# Subject                    (Intercept)   NA 0.403 0.635
# Subject.1               z.Session_cont   NA 2.393 1.547
# Subject.2                      z.Trial   NA 0.000 0.000
# Subject.3 con2.response_window_large.c   NA 0.932 0.965


# CIs
boot.res <- boot.glmm(
  model.res = full, excl.warnings = T,
  nboots = 1000, para = T
)
wt.txt(c.tab(boot.res))

#                                  orig       X2.5.      X97.5.
# (Intercept)               -1.26378218 -2.77262921 -0.04534593
# con2response_window_large  2.35431221  0.68314033  4.51419628
# z.Trial                    0.15117485 -0.41195249  0.69740626
# z.Session_cont            -0.74520012 -3.02608672  1.01418880


# Model stability
m.stab <- glmm.model.stab(model.res = full, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))
#                                           orig    min    max
# (Intercept)                             -1.264 -2.074 -0.440
# con2response_window_large                2.354  1.759  3.277
# z.Trial                                  0.151  0.037  0.297
# z.Session_cont                          -0.745 -1.472  0.018
# Subject@(Intercept)@NA                   0.635  0.000  0.831
# Subject@z.Session_cont@NA                1.547  0.436  2.558
# Subject@z.Trial@NA                       0.000  0.000  0.122
# Subject@con2.response_window_large.c@NA  0.965  0.507  1.600


# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])

xres <- lm(Success ~ con2 + z.Trial + z.Session_cont, data = ydata)
vif(xres)