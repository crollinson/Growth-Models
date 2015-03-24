####################################################################
# Appendix figures: Distributions of predictor data plotted along size response curves for each species
####################################################################
rm(list=ls())
# Load already bootstrapped runs
load("Species_Comparisons_FullModel_Ann.RData")

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

summary(comp.stack[comp.stack$Species=="ACRU",]) # MLE range: 0.19 - 0.60
summary(comp.stack[comp.stack$Species=="BELE",]) # MLE range: 0.4433 - 1.000
summary(comp.stack[comp.stack$Species=="NYSY",]) # MLE range: 0.4775 - 0.7870
summary(comp.stack[comp.stack$Species=="QUPR",]) # MLE range: 0.9246 - 0.9938
summary(comp.stack[comp.stack$Species=="QURU",]) # MLE range: 0.6572 - 1.000


acru.comp <- 1-min(comp.stack[comp.stack$Species=="ACRU","MLE"])/max(comp.stack[comp.stack$Species=="ACRU","MLE"])
bele.comp <- 1-min(comp.stack[comp.stack$Species=="BELE","MLE"])/max(comp.stack[comp.stack$Species=="BELE","MLE"])
nysy.comp <- 1-min(comp.stack[comp.stack$Species=="NYSY","MLE"])/max(comp.stack[comp.stack$Species=="NYSY","MLE"])
qupr.comp <- 1-min(comp.stack[comp.stack$Species=="QUPR","MLE"])/max(comp.stack[comp.stack$Species=="QUPR","MLE"])
quru.comp <- 1-min(comp.stack[comp.stack$Species=="QURU","MLE"])/max(comp.stack[comp.stack$Species=="QURU","MLE"])

comp <- c(acru.comp, bele.comp, nysy.comp, qupr.comp, quru.comp)
mean(comp); sd(comp)


acru.temp <- 1-min(temp.stack[temp.stack$Species=="ACRU","MLE"])/max(temp.stack[temp.stack$Species=="ACRU","MLE"])
bele.temp <- 1-min(temp.stack[temp.stack$Species=="BELE","MLE"])/max(temp.stack[temp.stack$Species=="BELE","MLE"])
nysy.temp <- 1-min(temp.stack[temp.stack$Species=="NYSY","MLE"])/max(temp.stack[temp.stack$Species=="NYSY","MLE"])
qupr.temp <- 1-min(temp.stack[temp.stack$Species=="QUPR","MLE"])/max(temp.stack[temp.stack$Species=="QUPR","MLE"])
quru.temp <- 1-min(temp.stack[temp.stack$Species=="QURU","MLE"])/max(temp.stack[temp.stack$Species=="QURU","MLE"])

temp <- c(acru.temp, bele.temp, nysy.temp, qupr.temp, quru.temp)
mean(temp); sd(temp)

acru.precip <- 1-min(precip.stack[precip.stack$Species=="ACRU","MLE"])/max(precip.stack[precip.stack$Species=="ACRU","MLE"])
bele.precip <- 1-min(precip.stack[precip.stack$Species=="BELE","MLE"])/max(precip.stack[precip.stack$Species=="BELE","MLE"])
nysy.precip <- 1-min(precip.stack[precip.stack$Species=="NYSY","MLE"])/max(precip.stack[precip.stack$Species=="NYSY","MLE"])
qupr.precip <- 1-min(precip.stack[precip.stack$Species=="QUPR","MLE"])/max(precip.stack[precip.stack$Species=="QUPR","MLE"])
quru.precip <- 1-min(precip.stack[precip.stack$Species=="QURU","MLE"])/max(precip.stack[precip.stack$Species=="QURU","MLE"])

precip <- c(acru.precip, bele.precip, nysy.precip, qupr.precip, quru.precip)
mean(precip); sd(precip)

acru.temp; acru.precip
bele.temp; bele.precip
nysy.temp; nysy.precip
qupr.temp; qupr.precip
quru.temp; quru.precip

##################################################################################
# ACRU data distributions
##################################################################################
summary(model.data[model.data$Spp=="ACRU",])


# Autogenic
summary(aut.stack)
acru.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=aut.stack[aut.stack$Species=="ACRU", ], aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=aut.stack[aut.stack$Species=="ACRU", ], aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(comp.stack)
acru.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=comp.stack[comp.stack$Species=="ACRU", ], aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=comp.stack[comp.stack$Species=="ACRU", ], aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(temp.stack)
acru.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x=temp.yr, weight=1/length(temp.yr)*2), col="gray50") +
	geom_ribbon(data=temp.stack[temp.stack$Species=="ACRU", ], aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=temp.stack[temp.stack$Species=="ACRU", ], aes(x= x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(precip.stack)
acru.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="ACRU",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=precip.stack[precip.stack$Species=="ACRU", ], aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=precip.stack[precip.stack$Species=="ACRU", ], aes(x=x.precip, y=MLE, color=Species), size=2) +
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
summary(aut.stack)
bele.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=aut.stack[aut.stack$Species=="BELE", ], aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=aut.stack[aut.stack$Species=="BELE", ], aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(comp.stack)
bele.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=comp.stack[comp.stack$Species=="BELE", ], aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=comp.stack[comp.stack$Species=="BELE", ], aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(temp.stack)
bele.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=temp.yr, weight=1/length(temp.yr)*2), col="gray50") +
	geom_ribbon(data=temp.stack[temp.stack$Species=="BELE", ], aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=temp.stack[temp.stack$Species=="BELE", ], aes(x= x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(precip.stack)
bele.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="BELE",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=precip.stack[precip.stack$Species=="BELE", ], aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=precip.stack[precip.stack$Species=="BELE", ], aes(x=x.precip, y=MLE, color=Species), size=2) +
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
summary(aut.stack)
nysy.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=aut.stack[aut.stack$Species=="NYSY", ], aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=aut.stack[aut.stack$Species=="NYSY", ], aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(comp.stack)
nysy.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=comp.stack[comp.stack$Species=="NYSY", ], aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=comp.stack[comp.stack$Species=="NYSY", ], aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(temp.stack)
nysy.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=temp.yr, weight=1/length(temp.yr)*2), col="gray50") +
	geom_ribbon(data=temp.stack[temp.stack$Species=="NYSY", ], aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=temp.stack[temp.stack$Species=="NYSY", ], aes(x= x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(precip.stack)
nysy.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="NYSY",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=precip.stack[precip.stack$Species=="NYSY", ], aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=precip.stack[precip.stack$Species=="NYSY", ], aes(x=x.precip, y=MLE, color=Species), size=2) +
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
summary(aut.stack)
qupr.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=aut.stack[aut.stack$Species=="QUPR", ], aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=aut.stack[aut.stack$Species=="QUPR", ], aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(comp.stack)
qupr.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=comp.stack[comp.stack$Species=="QUPR", ], aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=comp.stack[comp.stack$Species=="QUPR", ], aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(temp.stack)
qupr.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=temp.yr, weight=1/length(temp.yr)*2), col="gray50") +
	geom_ribbon(data=temp.stack[temp.stack$Species=="QUPR", ], aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=temp.stack[temp.stack$Species=="QUPR", ], aes(x= x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(precip.stack)
qupr.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QUPR",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=precip.stack[precip.stack$Species=="QUPR", ], aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=precip.stack[precip.stack$Species=="QUPR", ], aes(x=x.precip, y=MLE, color=Species), size=2) +
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
summary(aut.stack)
quru.aut <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=aut.stack[aut.stack$Species=="QURU", ], aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=aut.stack[aut.stack$Species=="QURU", ], aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(comp.stack)
quru.comp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=comp.stack[comp.stack$Species=="QURU", ], aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=comp.stack[comp.stack$Species=="QURU", ], aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(temp.stack)
quru.temp <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=temp.yr, weight=1/length(temp.yr)*2), col="gray50") +
	geom_ribbon(data=temp.stack[temp.stack$Species=="QURU", ], aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=temp.stack[temp.stack$Species=="QURU", ], aes(x= x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(precip.stack)
quru.precip <- ggplot() + q.blank2 +
	geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=precip.stack[precip.stack$Species=="QURU", ], aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=precip.stack[precip.stack$Species=="QURU", ], aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Percent", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - QURU.pdf")
multiplot(quru.aut, quru.temp, quru.comp,  quru.precip, cols=2)
dev.off()





##################################################################################
##################################################################################