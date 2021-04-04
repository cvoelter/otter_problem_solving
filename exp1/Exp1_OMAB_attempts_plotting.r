
all.data <- read.table(file = "exp1/data/160628 Otter multiaccess box_data 2016attempts.txt", header = T, sep = "\t")

zdata <- subset(all.data, Method != "-")
zdata <- subset(zdata, Success != 0)
zdata$Method <- factor(zdata$Method)

png(filename = "exp1/graphics/OMAB_attempts_individual data.png", width =500, height =500)

par(mfrow=c(2,2))

##Blacky
xdata=subset(zdata, Subject=="Blacky")
par(mar=c(3.2, 3.2, 1.2, 0.5), mgp=c(2, 0.8, 0))

plot(x=xdata$Trial, y=xdata$transitions, las=1, xlab="", ylab="Method transitions", xaxt="n",
type="b", pch=c(1, 8, 17)[as.numeric(xdata$Method)], main="Blacky", ylim=range(1:12), lty=3)


mtext(text=1:4, at=c(14,35,47,59), line=1, side=1)
 axis(side=1, at=c(1:65), labels=F) 

 
  axis(side=1,at=29.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
   axis(side=1,at=41.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
  axis(side=1,at=53.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)   

  legend("topleft", c("lid", "stick", "swing"), pch=c(1, 8, 17))


##James
xdata=subset(zdata, Subject=="James")
par(mar=c(3.2, 3.2, 1.2, 0.5), mgp=c(2, 0.8, 0))

plot(x=xdata$Trial, y=xdata$transitions, las=1, xlab="", ylab="", xaxt="n",
type="b", pch=c(1, 8, 17)[as.numeric(xdata$Method)], main="James", ylim=range(1:12),lty=3)


mtext(text=1:4, at=c(6,18,30,42), line=1, side=1)
 axis(side=1, at=c(1:48), labels=F) 
  axis(side=1,at=12.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
  axis(side=1,at=24.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
    axis(side=1,at=36.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
	
	
##Rudi
xdata=subset(zdata, Subject=="Rudi")
par(mar=c(3.2, 3.2, 1.2, 0.5), mgp=c(2, 0.8, 0))

plot(x=xdata$Trial, y=xdata$transitions, las=1, xlab="", ylab="Method transitions", xaxt="n",
type="b", pch=c(1, 8, 17)[as.numeric(xdata$Method)], main="Rudi", ylim=range(1:12),lty=3)


mtext(text=1:4, at=c(6,18,30,42), line=1, side=1)
mtext(text="Phase", at=24, line=2, side=1)
 axis(side=1, at=c(1:48), labels=F) 
 
axis(side=1,at=12.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
  axis(side=1,at=24.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
    axis(side=1,at=36.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
	

##Twoface
xdata=subset(zdata, Subject=="Twoface")
par(mar=c(3.2, 3.2, 1.2, 0.5), mgp=c(2, 0.8, 0))

plot(x=xdata$Trial, y=xdata$transitions, las=1, xlab="", ylab="", xaxt="n",
type="b", pch=c(1, 8, 17)[as.numeric(xdata$Method)], main="Twoface", ylim=range(1:12),lty=3)


mtext(text=1:2, at=c(9.5, 25), line=1, side=1)
mtext(text="Phase", at=15.5, line=2, side=1)
 axis(side=1, at=c(1:31), labels=F) 
 
axis(side=1,at=19.5,col="darkgray",line=2,tick=T,labels=F,lwd=2,lwd.ticks=1, tck=1.15,lty=2)
  
dev.off()
