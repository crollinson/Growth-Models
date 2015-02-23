###############################
# QURU
###############################
load("QURU_Full_Ann_Results.Rdata")
results.quru <- results

# quru.run <- read.csv("QURU_AllSites_1990-2011.csv")
# summary(quru.run)

# Need to generate residuals for QURU to do ACF
plot(results.quru$source_data$predicted,results.quru$source_data$BAI,xlab="Predicted",ylab="Observed")
abline(0,1,lwd=2,col="red")

# generate resids
results.quru$source_data$resid <- results.quru$source_data$BAI - results.quru$source_data$predicted
summary(results.quru$source_data)

hist(results.quru$source_data$resid)
acf(results.quru$source_data$resid, lag.max=20)
boxplot(resid ~ TreeID, data=results.quru$source_data)

length(unique(results.quru$source_data$TreeID))
par(mfrow=c(6,6), mar=c(5, 5, 1, 1)+.1)
for(i in unique(results.quru$source_data$TreeID)){
	acf(results.quru$source_data[results.quru$source_data$TreeID==i, "resid"], lag.max=20)
	text(x=20, y=0.85, labels=i, pos=2, cex=1.5, font=2)
	}

quru1 <- lm(resid ~ Year-1, data=results.quru$source_data)
summary(quru1)

quru1b <- lme(resid ~ Year-1, random=list(TreeID=~1), data=results.quru$source_data)
summary(quru1b)

quru2 <- aov(resid ~ TreeID-1, data=results.quru$source_data)
summary(quru2)

quru3 <- aov(resid ~ PlotID-1, data=results.quru$source_data)
summary(quru3)

quru4 <- aov(resid ~ Site.Trans-1, data=results.quru$source_data)
summary(quru4)

quru5 <- aov(resid ~ Site-1, data=results.quru$source_data)
summary(quru5)
