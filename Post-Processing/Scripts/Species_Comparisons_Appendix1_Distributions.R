####################################################################
# Appendix figures: Distributions of predictor data plotted along size response curves for each species
####################################################################
rm(list=ls())
setwd("..")

# Load already bootstrapped runs
load("Inputs/Scalars_Annual_Bootstrapped.Rdata")
# model.data <- read.csv("Inputs/TargetSpecies_AllSites_Ann_1990-2011.csv")
model.data <- read.csv("../RawInputs/CARCA_CoreData_Climate_Annual.csv")
model.data$BA.tree.cm2 <- model.data$BA.tree/100

# Subsetting the data to make our lives easier
species <- c("ACRU", "QURU", "QUPR", "BELE", "NYSY")
model.data <- model.data[(model.data$Spp %in% species) & model.data$Year>=1990 & model.data$Year<=2011,]

library(likelihood)
library(ggplot2)
library(raster)
library(grid)
library(kobe)


q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

q.blank2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1)), axis.text.y=element_text(color="black", size=rel(1)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))


large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))



# Number of trees
length(unique(model.data[model.data$Spp=="ACRU","TreeID"]))
length(unique(model.data[model.data$Spp=="BELE","TreeID"]))
length(unique(model.data[model.data$Spp=="NYSY","TreeID"]))
length(unique(model.data[model.data$Spp=="QUPR","TreeID"]))
length(unique(model.data[model.data$Spp=="QURU","TreeID"]))

# Number of Rings
length(model.data[model.data$Spp=="ACRU","TreeID"])
length(model.data[model.data$Spp=="BELE","TreeID"])
length(model.data[model.data$Spp=="NYSY","TreeID"])
length(model.data[model.data$Spp=="QUPR","TreeID"])
length(model.data[model.data$Spp=="QURU","TreeID"])

# Summary of competition
summary(scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Competition",]) # MLE range: 0.19 - 0.60
summary(scalars.annual[scalars.annual$Species=="BELE" & scalars.annual$Factor=="Competition",]) # MLE range: 0.4433 - 1.000
summary(scalars.annual[scalars.annual$Species=="NYSY" & scalars.annual$Factor=="Competition",]) # MLE range: 0.4775 - 0.7870
summary(scalars.annual[scalars.annual$Species=="QUPR" & scalars.annual$Factor=="Competition",]) # MLE range: 0.9246 - 0.9938
summary(scalars.annual[scalars.annual$Species=="QURU" & scalars.annual$Factor=="Competition",]) # MLE range: 0.6572 - 1.000

# # ---------------------------
# # NOTE: This all needs to be updated to make sense now
# # ---------------------------
# # Looking at change in competition over the range(?)
# acru.comp <- 1-min(scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Competition","MLE"])/max(scalars.annual[scalars.annual$Species=="ACRU","MLE"])
# bele.comp <- 1-min(scalars.annual[scalars.annual$Species=="BELE" & scalars.annual$Factor=="Competition","MLE"])/max(scalars.annual[scalars.annual$Species=="BELE","MLE"])
# nysy.comp <- 1-min(scalars.annual[scalars.annual$Species=="NYSY" & scalars.annual$Factor=="Competition","MLE"])/max(scalars.annual[scalars.annual$Species=="NYSY","MLE"])
# qupr.comp <- 1-min(scalars.annual[scalars.annual$Species=="QUPR" & scalars.annual$Factor=="Competition","MLE"])/max(scalars.annual[scalars.annual$Species=="QUPR","MLE"])
# quru.comp <- 1-min(scalars.annual[scalars.annual$Species=="QURU" & scalars.annual$Factor=="Competition","MLE"])/max(scalars.annual[scalars.annual$Species=="QURU","MLE"])

# comp <- c(acru.comp, bele.comp, nysy.comp, qupr.comp, quru.comp)
# mean(comp); sd(comp)

# # Doing something with temperature (not sure what)
# acru.temp <- 1-min(scalars.annual[scalars.annual$Species=="ACRU","MLE"])/max(scalars.annual[scalars.annual$Species=="ACRU","MLE"])
# bele.temp <- 1-min(scalars.annual[scalars.annual$Species=="BELE","MLE"])/max(scalars.annual[scalars.annual$Species=="BELE","MLE"])
# nysy.temp <- 1-min(scalars.annual[scalars.annual$Species=="NYSY","MLE"])/max(scalars.annual[scalars.annual$Species=="NYSY","MLE"])
# qupr.temp <- 1-min(scalars.annual[scalars.annual$Species=="QUPR","MLE"])/max(scalars.annual[scalars.annual$Species=="QUPR","MLE"])
# quru.temp <- 1-min(scalars.annual[scalars.annual$Species=="QURU","MLE"])/max(scalars.annual[scalars.annual$Species=="QURU","MLE"])

# temp <- c(acru.temp, bele.temp, nysy.temp, qupr.temp, quru.temp)
# mean(temp); sd(temp)

# acru.precip <- 1-min(scalars.annual[scalars.annual$Species=="ACRU","MLE"])/max(scalars.annual[scalars.annual$Species=="ACRU","MLE"])
# bele.precip <- 1-min(scalars.annual[scalars.annual$Species=="BELE","MLE"])/max(scalars.annual[scalars.annual$Species=="BELE","MLE"])
# nysy.precip <- 1-min(scalars.annual[scalars.annual$Species=="NYSY","MLE"])/max(scalars.annual[scalars.annual$Species=="NYSY","MLE"])
# qupr.precip <- 1-min(scalars.annual[scalars.annual$Species=="QUPR","MLE"])/max(scalars.annual[scalars.annual$Species=="QUPR","MLE"])
# quru.precip <- 1-min(scalars.annual[scalars.annual$Species=="QURU","MLE"])/max(scalars.annual[scalars.annual$Species=="QURU","MLE"])

# precip <- c(acru.precip, bele.precip, nysy.precip, qupr.precip, quru.precip)
# mean(precip); sd(precip)

# acru.temp; acru.precip
# bele.temp; bele.precip
# nysy.temp; nysy.precip
# qupr.temp; qupr.precip
# quru.temp; quru.precip

##################################################################################
# ACRU data distributions
##################################################################################
summary(model.data[model.data$Spp=="ACRU",])


# Autogenic
summary(scalars.annual)
acru.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x= BA.tree.cm2, weight=1/length(BA.tree.cm2)), color="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Size", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Size", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(scalars.annual)
acru.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x=RelBA, weight=1/length(RelBA)), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Competition", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Competition", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(scalars.annual)
acru.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x=Tavg, weight=1/length(Tavg)), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Temperature", ], aes(x=X-273, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Temperature", ], aes(x=X-273, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Tmean (C)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(scalars.annual)
acru.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x= Precip.PRISM.sum, weight=1/length(Precip.PRISM.sum)), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Precipitation", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Precipitation", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - ACRU.pdf")
multiplot(acru.aut, acru.temp, acru.comp,  acru.precip, cols=2)
dev.off()




##################################################################################
# BELE data distributions
##################################################################################
summary(model.data[model.data$Spp=="BELE",])

# Autogenic
summary(scalars.annual)
bele.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(scalars.annual)
bele.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(scalars.annual)
bele.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=Tavg, weight=1/length(Tavg)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x= X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(scalars.annual)
bele.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="BELE", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - BELE.pdf")
multiplot(bele.aut, bele.temp, bele.comp,  bele.precip, cols=2)
dev.off()




##################################################################################
# NYSY data distributions
##################################################################################
summary(model.data[model.data$Spp=="NYSY",])

# Autogenic
summary(scalars.annual)
nysy.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(scalars.annual)
nysy.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(scalars.annual)
nysy.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=Tavg, weight=1/length(Tavg)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x= X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(scalars.annual)
nysy.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="NYSY", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - NYSY.pdf")
multiplot(nysy.aut, nysy.temp, nysy.comp,  nysy.precip, cols=2)
dev.off()





##################################################################################
# QUPR data distributions
##################################################################################
summary(model.data[model.data$Spp=="QUPR",])

# Autogenic
summary(scalars.annual)
qupr.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(scalars.annual)
qupr.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(scalars.annual)
qupr.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=Tavg, weight=1/length(Tavg)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x= X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(scalars.annual)
qupr.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QUPR", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - QUPR.pdf")
multiplot(qupr.aut, qupr.temp, qupr.comp,  qupr.precip, cols=2)
dev.off()





##################################################################################
# QURU data distributions
##################################################################################
summary(model.data[model.data$Spp=="QURU",])

# Autogenic
summary(scalars.annual)
quru.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(scalars.annual)
quru.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(scalars.annual)
quru.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=Tavg, weight=1/length(Tavg)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x= X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(scalars.annual)
quru.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	geom_line(data=scalars.annual[scalars.annual$Species=="QURU", ], aes(x=X, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - QURU.pdf")
multiplot(quru.aut, quru.temp, quru.comp,  quru.precip, cols=2)
dev.off()





##################################################################################
##################################################################################