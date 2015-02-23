####################################################################
# Checking autocorrelaton of trees from models
####################################################################
rm(list=ls())
library(nlme)
###############################
# ACRU
###############################
load("ACRU_Full_Ann_Results.Rdata")
results.acru <- results

# acru.run <- read.csv("ACRU_AllSites_1990-2011.csv")
# summary(acru.run)

# Need to generate residuals for ACRU to do ACF
plot(results.acru$source_data$predicted,results.acru$source_data$BAI,xlab="Predicted",ylab="Observed")
abline(0,1,lwd=2,col="red")

# generate resids
results.acru$source_data$resid <- results.acru$source_data$BAI - results.acru$source_data$predicted
summary(results.acru$source_data)

hist(results.acru$source_data$resid)
acf(results.acru$source_data$resid, lag.max=20)
boxplot(resid ~ TreeID, data=results.acru$source_data)

length(unique(results.acru$source_data$TreeID))
par(mfrow=c(6,6), mar=c(5, 5, 1, 1)+.1)
for(i in unique(results.acru$source_data$TreeID)){
	acf(results.acru$source_data[results.acru$source_data$TreeID==i, "resid"], lag.max=20)
	text(x=20, y=0.85, labels=i, pos=2, cex=1.5, font=2)
	}

acru1 <- lm(resid ~ Year -1, data=results.acru$source_data)
summary(acru1)

acru1b <- lme(resid ~ Year -1 , random=list(TreeID=~1), data=results.acru$source_data)
summary(acru1b)

acru2 <- aov(resid ~ TreeID-1, data=results.acru$source_data)
summary(acru2)

acru2b <- lme(resid ~ TreeID-1, random=list(Year=~1), data=results.acru$source_data)
summary(acru2b)

acru3 <- aov(resid ~ PlotID, data=results.acru$source_data)
summary(acru3)

acru4 <- aov(resid ~ Site, data=results.acru$source_data)
summary(acru4)


###############################
# BELE
###############################
load("BELE_Full_Ann_Results.Rdata")
results.bele <- results

# bele.run <- read.csv("BELE_AllSites_1990-2011.csv")
# summary(bele.run)

# Need to generate residuals for BELE to do ACF
plot(results.bele$source_data$predicted,results.bele$source_data$BAI,xlab="Predicted",ylab="Observed")
abline(0,1,lwd=2,col="red")

# generate resids
results.bele$source_data$resid <- results.bele$source_data$BAI - results.bele$source_data$predicted
summary(results.bele$source_data)

hist(results.bele$source_data$resid)
acf(results.bele$source_data$resid, lag.max=20)
boxplot(resid ~ TreeID, data=results.bele$source_data)

length(unique(results.bele$source_data$TreeID))
par(mfrow=c(6,6), mar=c(5, 5, 1, 1)+.1)
for(i in unique(results.bele$source_data$TreeID)){
	acf(results.bele$source_data[results.bele$source_data$TreeID==i, "resid"], lag.max=20)
	text(x=20, y=0.85, labels=i, pos=2, cex=1.5, font=2)
	}

bele1 <- lm(resid ~ Year-1, data=results.bele$source_data)
summary(bele1)

bele1b <- lme(resid ~ Year -1 , random=list(TreeID=~1), data=results.bele$source_data)
summary(bele1b)

bele2 <- aov(resid ~ TreeID-1, data=results.bele$source_data)
summary(bele2)

bele3 <- aov(resid ~ PlotID, data=results.bele$source_data)
summary(bele3)

bele4 <- aov(resid ~ Site, data=results.bele$source_data)
summary(bele4)

###############################
# NYSY
###############################
load("NYSY_Full_Ann_Results.Rdata")
results.nysy <- results

# nysy.run <- read.csv("NYSY_AllSites_1990-2011.csv")
# summary(nysy.run)

# Need to generate residuals for NYSY to do ACF
plot(results.nysy$source_data$predicted,results.nysy$source_data$BAI,xlab="Predicted",ylab="Observed")
abline(0,1,lwd=2,col="red")

# generate resids
results.nysy$source_data$resid <- results.nysy$source_data$BAI - results.nysy$source_data$predicted
summary(results.nysy$source_data)

hist(results.nysy$source_data$resid)
acf(results.nysy$source_data$resid, lag.max=20)
boxplot(resid ~ TreeID, data=results.nysy$source_data)

nysy1 <- lm(resid ~ Year, data=results.nysy$source_data)
summary(nysy1)

nysy2 <- aov(resid ~ TreeID, data=results.nysy$source_data)
summary(nysy2)

nysy3 <- aov(resid ~ PlotID, data=results.nysy$source_data)
summary(nysy3)

nysy4 <- aov(resid ~ Site, data=results.nysy$source_data)
summary(nysy4)


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


###############################
# QUPR
###############################
load("QUPR_Full_Ann_Results.Rdata")
results.qupr <- results

# qupr.run <- read.csv("QUPR_AllSites_1990-2011.csv")
# summary(qupr.run)

# Need to generate residuals for QUPR to do ACF
plot(results.qupr$source_data$predicted,results.qupr$source_data$BAI,xlab="Predicted",ylab="Observed")
abline(0,1,lwd=2,col="red")

# generate resids
results.qupr$source_data$resid <- results.qupr$source_data$BAI - results.qupr$source_data$predicted
summary(results.qupr$source_data)

hist(results.qupr$source_data$resid)
acf(results.qupr$source_data$resid, lag.max=20)
boxplot(resid ~ TreeID, data=results.qupr$source_data)

qupr1 <- lm(resid ~ Year, data=results.qupr$source_data)
summary(qupr1)

qupr2 <- aov(resid ~ TreeID, data=results.qupr$source_data)
summary(qupr2)

qupr3 <- aov(resid ~ PlotID, data=results.qupr$source_data)
summary(qupr3)

qupr4 <- aov(resid ~ Site, data=results.qupr$source_data)
summary(qupr4)


########################################################################
# Summarizing the ACF graphs
########################################################################
par(mfrow=c(3,2), mar=c(5, 5, 1, 1)+.1)
acf(results.acru$source_data$resid, lag.max=20, lwd=2, main="", font.lab=2, cex.axis=1.5, cex.lab=1.5)
text(x=20, y=0.85, labels="ACRU, Annual", pos=2, cex=1.5, font=2)
acf(results.bele$source_data$resid, lag.max=20, lwd=2, main="", font.lab=2, cex.axis=1.5, cex.lab=1.5)
text(x=20, y=0.85, labels="BELE, Annual", pos=2, cex=1.5, font=2)
acf(results.nysy$source_data$resid, lag.max=20, lwd=2, main="", font.lab=2, cex.axis=1.5, cex.lab=1.5)
text(x=20, y=0.85, labels="NYSY, Annual", pos=2, cex=1.5, font=2)
acf(results.quru$source_data$resid, lag.max=20, lwd=2, main="", font.lab=2, cex.axis=1.5, cex.lab=1.5)
text(x=20, y=0.85, labels="QURU, Annual", pos=2, cex=1.5, font=2)
acf(results.qupr$source_data$resid, lag.max=20, lwd=2, main="", font.lab=2, cex.axis=1.5, cex.lab=1.5)
text(x=20, y=0.85, labels="QUPR, Annual", pos=2, cex=1.5, font=2)
