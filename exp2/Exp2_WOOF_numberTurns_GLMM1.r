library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp2/data/WOOF_data 2016.txt", header = T, sep = "\t")

str(all.data)

xdata <- subset(all.data, Phase > 3 & Phase < 6)
xdata <- subset(xdata, Number_of_turns > 0)
summary(xdata)

xx.fe.re <- fe.re.tab(
  fe.model = "Number_of_turns~food_level+Trial", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary

ydata <- xx.fe.re$data


# centering factors for random slopes
ydata$food_level.high.c <- ydata$food_level.high - mean(ydata$food_level.high)

# z-transforming covariate
ydata$z.Trial <- as.vector(scale(ydata$Trial))

# GLMM
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000))

full <- glmer(Number_of_turns ~ food_level + z.Trial +
  (1 | Subject) + (0 + z.Trial | Subject) + (0 + food_level.high.c | Subject),
family = poisson, data = ydata, control = contr
)

overdisp.test(full) #--> 2.703451

full.nb <- glmer.nb(Number_of_turns ~ food_level + z.Trial +
  (1 | Subject) + (0 + z.Trial | Subject) + (0 + food_level.high.c | Subject),
data = ydata, control = contr
)

overdisp.test(full.nb) #-->1.240891

null.nb <- glmer.nb(Number_of_turns ~ 1 +
  (1 | Subject) + (0 + z.Trial | Subject) + (0 + food_level.high.c | Subject),
data = ydata, control = contr
)



wt.txt(c.tab(anova(null.nb, full.nb, test = "Chisq"), 3))
#            Df     AIC     BIC   logLik deviance Chisq Chi Df Pr(>Chisq)
# null.nb 5.000 433.280 446.049 -211.640  423.280    NA     NA         NA
# full.nb 7.000 428.136 446.013 -207.068  414.136 9.144  2.000      0.010


tests <- as.data.frame(drop1(full.nb, test = "Chisq"))
wt.txt(c.tab(tests, 3))
Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.00436262 (tol = 0.002, component 1)





wt.txt(c.tab(summary(full.nb)$coefficients, 3))

#                Estimate Std. Error z value Pr(>|z|)
# (Intercept)       1.279      0.154   8.297    0.000
# food_levelhigh   -0.576      0.209  -2.760    0.006
# z.Trial          -0.237      0.103  -2.290    0.022






wt.txt(c.tab(as.data.frame(summary(full.nb)$varcor), 3))

# grp                    var1 var2 vcov sdcor
# Subject         (Intercept)   NA 0.05 0.224
# Subject.1           z.Trial   NA 0.00 0.000
# Subject.2 food_level.high.c   NA 0.00 0.000


## CI
boot.res <- boot.glmm(
  model.res = full.nb, excl.warnings = T,
  nboots = 1000, para = T
)
wt.txt(c.tab(boot.res))
#                      orig      X2.5.     X97.5.
# (Intercept)     1.2792440  0.9600645  1.5570776
# food_levelhigh -0.5758354 -1.0002324 -0.1631235
# z.Trial        -0.2369752 -0.4363084 -0.0508792


# Model stability
m.stab <- glmm.model.stab(model.res = full.nb, contr = contr)
wt.txt(c.tab(round(m.stab$summary[, -1], 3)))
#                               orig    min    max
# (Intercept)                  1.279  1.080  1.360
# food_levelhigh              -0.576 -0.669 -0.437
# z.Trial                     -0.237 -0.280 -0.170
# Subject@(Intercept)          0.224  0.002  0.519
# Subject.1@z.Trial            0.000  0.001  0.001
# Subject.2@food_level.high.c  0.000  0.001  0.003



# visualising the variance of each variable
m.stab.plot(m.stab$summary[, -1])
## looks good

xres <- lm(Number_of_turns ~ food_level + z.Trial, data = ydata)

library(car)
vif(xres)