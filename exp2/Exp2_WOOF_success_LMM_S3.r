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
xdata <- subset(xdata, Success == 1)
summary(xdata)

hist(xdata$Latency)
hist(xdata$Phase)

# box-cox power transformation using library car
trans <- powerTransform(xdata$Latency)$lambda
xdata$log.Latency <- ((xdata$Latency^trans - 1) / trans)
hist(xdata$log.Latency)


xx.fe.re <- fe.re.tab(
  fe.model = "log.Latency~con2+Trial+Session_cont", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary

ydata <- xx.fe.re$data


# centering
ydata$con2.response_window_large.c <- ydata$con2.response_window_large - mean(ydata$con2.response_window_large)
ydata$z.Session_cont <- as.vector(scale(ydata$Session_cont))
ydata$z.Trial <- as.vector(scale(ydata$Trial))

contr <- lmerControl(optCtrl = list(maxfun = 10000000))

full <- lmer(log.Latency ~ con2 + z.Trial + z.Session_cont +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
REML = F, data = ydata, control = contr
)

diagnostics.plot(full)

null <- lmer(log.Latency ~ 1 +
  (1 | Subject) + (0 + z.Session_cont | Subject) + (0 + z.Trial | Subject) + (0 + con2.response_window_large.c | Subject),
REML = F, data = ydata, control = contr
)

wt.txt(c.tab(anova(null, full, test = "Chisq"), 3))
#       npar     AIC     BIC  logLik deviance Chisq    Df Pr(>Chisq)
# null 6.000 121.580 135.404 -54.790  109.580    NA    NA         NA
# full 9.000 126.978 147.715 -54.489  108.978 0.601 3.000      0.896


# Colinearity
xres <- lm(log.Latency ~ con2 + z.Trial + z.Session_cont, data = ydata)
vif(xres)