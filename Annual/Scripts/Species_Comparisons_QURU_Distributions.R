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

q.blank3 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(0)), axis.text.y=element_text(color="black", size=rel(0)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))


large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))


##################################################################################
# QURU data distributions
##################################################################################
summary(model.data[model.data$Spp=="QURU",])

# Autogenic
summary(aut.stack)
quru.aut <- ggplot() + q.blank3 +
	# geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=BA.tree.cm2, weight=1/length(BA.tree.cm2)*2), col="gray50") +
	geom_ribbon(data=aut.stack[aut.stack$Species=="QURU", ], aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=aut.stack[aut.stack$Species=="QURU", ], aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Tree Basal Area (cm2)", limit=range(model.data$BA.tree.cm2, na.rm=T)) + scale_y_continuous(name="Growth", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Relative BA
summary(comp.stack)
quru.comp <- ggplot() + q.blank3 +
	# geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=RelBA, weight=1/length(RelBA)*2), col="gray50") +
	geom_ribbon(data=comp.stack[comp.stack$Species=="QURU", ], aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=comp.stack[comp.stack$Species=="QURU", ], aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Relative Basal Area", limit=c(0,1)) + scale_y_continuous(name="Growth", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Temperature
summary(temp.stack)
quru.temp <- ggplot() + q.blank3 +
	# geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=temp.yr, weight=1/length(temp.yr)*2), col="gray50") +
	geom_ribbon(data=temp.stack[temp.stack$Species=="QURU", ], aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=temp.stack[temp.stack$Species=="QURU", ], aes(x= x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Tmean (K)") + scale_y_continuous(name="Growth", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")

# Precipitation
summary(precip.stack)
quru.precip <- ggplot() + q.blank3 +
	# geom_histogram(data=model.data[model.data$Spp=="QURU",], aes(x=ppt.yr, weight=1/length(ppt.yr)*2), col="gray50") +
	geom_ribbon(data=precip.stack[precip.stack$Species=="QURU", ], aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(data=precip.stack[precip.stack$Species=="QURU", ], aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="Precipitation (mm)") + scale_y_continuous(name="Growth", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	guides(fill=F, color=F)
	# theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")


pdf("Predictor Distributions - QURU2.pdf", height=6, width=2.5)
multiplot(quru.comp, quru.temp, quru.precip, cols=1)
dev.off()


