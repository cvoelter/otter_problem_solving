all.data <- read.table(file = "exp2/data/WOOF_data 2016.txt", header = T, sep = "\t")

str(all.data)


png(filename="exp2/graphics/Exp2_WOOF_plot.png", height = 600, width=480)
par(mfrow = c(3, 1), mar = c(3, 3, 1.5, 1.5), mgp = c(1.7, 0.5, 0), tcl = -0.25)

#### success###

xdata <- all.data

xx <- aggregate(
  x = xdata$Success,
  by = xdata[, c("Phase", "Subject")], FUN = mean
)

medians <- tapply(X = xx$x, INDEX = xx$Phase, FUN = median)


xx[3] <- round(xx[3], digit = 2)


plot(
  x = as.numeric(xx$Phase), y = xx$x, xlim = c(0.5, 7.5), ylim = c(0, 1.1), las = 1, pch = 19, xaxt = "n",
  tcl = -0.2,
  xlab = "", cex = 0, ylab = "Success", col = grey(level = 0.25, alpha = 0.5), main = "a) Success"
)




xx1 <- subset(xx, Phase == 1)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 1, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))




xx1 <- subset(xx, Phase == 2)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 2, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 3)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 3, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 4)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 4, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 5)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 5, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 6)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 6, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))

xx1 <- subset(xx, Phase == 7)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 7, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))



hll <- 0.25 # create scalar indicating half the line length
segments(x0 = (1:7) - hll, x1 = (1:7) + hll, y0 = medians, y1 = medians, lwd = 2)
axis(side = 1, at = 3.5, col = "darkgray", line = 0, tick = T, labels = F, lwd = 2, lwd.ticks = 1, tck = 1, lty = 2)

############ latency

xx <- aggregate(
  x = xdata$Latency,
  by = xdata[, c("Phase", "Subject")], FUN = mean
)

medians <- tapply(X = xx$x, INDEX = xx$Phase, FUN = median)


xx[3] <- round(xx[3], digit = 0)


plot(
  x = as.numeric(xx$Phase), y = xx$x, xlim = c(0.5, 7.5), ylim = c(0, 105), las = 1, pch = 19, xaxt = "n",
  tcl = -0.2,
  xlab = "", cex = 0, ylab = "Latency (in sec)", col = grey(level = 0.25, alpha = 0.5), main = "b) Latency"
)




xx1 <- subset(xx, Phase == 1)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 1, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))




xx1 <- subset(xx, Phase == 2)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 2, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 3)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 3, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 4)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 4, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 5)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 5, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 6)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 6, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))

xx1 <- subset(xx, Phase == 7)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 7, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))






hll <- 0.25 # create scalar indicating half the line length
segments(x0 = (1:7) - hll, x1 = (1:7) + hll, y0 = medians, y1 = medians, lwd = 2)
axis(side = 1, at = 3.5, col = "darkgray", line = 0, tick = T, labels = F, lwd = 2, lwd.ticks = 1, tck = 1, lty = 2)

########### Number of turns##########

zdata <- subset(all.data, Number_of_turns > 0)

xx <- aggregate(
  x = zdata$Number_of_turns,
  by = zdata[, c("Phase", "Subject")], FUN = mean
)

medians <- tapply(X = xx$x, INDEX = xx$Phase, FUN = median)


xx[3] <- round(xx[3], digit = 0)


# 	par(mfrow=c(1,3), mar=c(3,3,1.5,1.5), mgp=c(1.7,0.5,0), tcl=-0.25)


# par(mfrow=c(1,3), mar=c(3,3,1.5,1.5), mgp=c(1.7,0.5,0), tcl=-0.25)


plot(
  x = as.numeric(xx$Phase), y = xx$x, xlim = c(0.5, 7.5), ylim = c(0, 15), las = 1, pch = 19, xaxt = "n",
  tcl = -0.2,
  xlab = "", cex = 0, ylab = "Number of turns", col = grey(level = 0.25, alpha = 0.5), main = "c) Number of turns"
)




xx1 <- subset(xx, Phase == 1)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 1, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))




xx1 <- subset(xx, Phase == 2)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 2, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 3)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 3, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 4)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 4, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 5)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 5, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))


xx1 <- subset(xx, Phase == 6)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 6, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))

xx1 <- subset(xx, Phase == 7)
px <- table(xx1$x)
# step 2, add points:
points(x = rep(x = 7, times = length(px)), y = as.numeric(names(px)), cex = 0.8 * (px), pch = 19, col = grey(level = 0.25, alpha = 0.75))

hll <- 0.25 # create scalar indicating half the line length
segments(x0 = (1:7) - hll, x1 = (1:7) + hll, y0 = medians, y1 = medians, lwd = 2)

axis(side = 1, at = 3.5, col = "darkgray", line = 3, tick = T, labels = F, lwd = 2, lwd.ticks = 1, tck = 1.25, lty = 2)

mtext(text = "low", at = 1, line = 0, side = 1, cex = 0.75)
mtext(text = "high", at = 2, line = 0, side = 1, cex = 0.75)
mtext(text = "high", at = 3, line = 0, side = 1, cex = 0.75)
mtext(text = "high", at = 4, line = 0, side = 1, cex = 0.75)
mtext(text = "extra-high", at = 5, line = 0, side = 1, cex = 0.75)
mtext(text = "extra-high", at = 6, line = 0, side = 1, cex = 0.75)
mtext(text = "extra-high", at = 7, line = 0, side = 1, cex = 0.75)

mtext(text = "Training (1-3)", at = 2, line = 1.7, side = 1, cex = 0.8)

mtext(text = "visible", at = 4, line = 1.7, side = 1, cex = 0.8)
mtext(text = "visible", at = 5, line = 1.7, side = 1, cex = 0.8)
mtext(text = "occluded", at = 6, line = 1.7, side = 1, cex = 0.8)
mtext(text = "visible", at = 7, line = 1.7, side = 1, cex = 0.8)


dev.off()