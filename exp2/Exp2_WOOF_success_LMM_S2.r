library(lme4)
library(car)
source("functions/diagnostic_fcns.r")
source("functions/helpers.r")
source("functions/boot_glmm.r")
source("functions/glmm_stability.r")

all.data <- read.table(file = "exp2/data/WOOF_data 2016.txt", header = T, sep = "\t")
str(all.data)

xdata <- subset(all.data, Phase > 3 & Phase < 6)
xdata <- subset(xdata, Success == 1)
summary(xdata)

hist(xdata$Latency)


# box-cox power transformation using library car

trans<-powerTransform(xdata$Latency)$lambda
xdata$log.Latency<-((xdata$Latency^trans-1)/trans)
hist(xdata$log.Latency)

xx.fe.re <- fe.re.tab(
  fe.model = "log.Latency~food_level+Trial", re = "(1|Subject)",
  data = data.frame(xdata)
)
xx.fe.re$summary

ydata <- xx.fe.re$data

# centering
ydata$food_level.high.c <- ydata$food_level.high - mean(ydata$food_level.high)
ydata$z.Trial <- as.vector(scale(ydata$Trial))

contr <- lmerControl(optCtrl = list(maxfun = 10000000))

full <- lmer(log.Latency ~ food_level + z.Trial +
  (1 | Subject) + (0 + z.Trial | Subject) + (0 + food_level.high.c | Subject),
REML = F, data = ydata, control = contr
)

diagnostics.plot(full)


null <- lmer(log.Latency ~ 1 +
  (1 | Subject) + (0 + z.Trial | Subject) + (0 + food_level.high.c | Subject),
REML = F, data = ydata, control = contr
)


wt.txt(c.tab(anova(null, full, test = "Chisq"), 3))
#       npar     AIC     BIC  logLik deviance Chisq    Df Pr(>Chisq)
# null 5.000 115.444 127.416 -52.722  105.444    NA    NA         NA
# full 7.000 115.170 131.931 -50.585  101.170 4.274 2.000      0.118

#Colinearity
xres <- lm(log.Latency ~ food_level + z.Trial, data = ydata)

vif(xres)