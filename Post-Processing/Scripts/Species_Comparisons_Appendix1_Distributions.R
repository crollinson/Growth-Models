####################################################################
# Appendix figures: Distributions of predictor data plotted along size response curves for each species
####################################################################
rm(list=ls())
setwd("..")



# -------------------------
# Load Libraries & some custom ggplot themes
# -------------------------
library(likelihood)
library(ggplot2)
# library(raster)
library(grid)
library(car)
# library(kobe)


q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

q.blank2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1)), axis.text.y=element_text(color="black", size=rel(1)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))


large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))
# -------------------------


# -------------------------
# Load data
# -------------------------
# Load already bootstrapped runs
load("Inputs/Scalars_Annual_Bootstrapped.Rdata") # loads object "scalars.annual"
# change Temp to Celcius
scalars.annual[scalars.annual$Factor=="Temperature","X"] <- scalars.annual[scalars.annual$Factor=="Temperature","X"]-273.15 
summary(scalars.annual)

# Loading in the raw tree ring data
model.data <- read.csv("../RawInputs/CARCA_CoreData_Climate_Annual.csv")
model.data$BA.tree.cm2 <- model.data$BA.tree/100

# Subsetting the data to make our lives easier
species <- c("ACRU", "QURU", "QUPR", "BELE", "NYSY")
model.data <- model.data[(model.data$Spp %in% species) & model.data$Year>=1990 & model.data$Year<=2011,]

# Translating model.data into something that more closely represents the scalars.annual to 
#   make figures a lot easier
data.x <- stack(model.data[,c("BA.tree.cm2", "RelBA", "Tavg", "Precip.PRISM.sum")])[,c(2,1)]
names(data.x)  <- c("Factor", "X")
data.x$Factor  <- recode(data.x$Factor, "'BA.tree.cm2'='Size'; 'RelBA'='Competition'; 'Tavg'='Temperature'; 'Precip.PRISM.sum'='Precipitation'")
data.x$Species <- model.data$Spp
data.x$Site    <- model.data$Site 
data.x$PlotID  <- model.data$PlotID
data.x$Trans   <- model.data$Trans
data.x$TreeID  <- model.data$TreeID
data.x$Year    <- model.data$Year
summary(data.x)
# -------------------------



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

# # Summary of competition
# summary(scalars.annual[scalars.annual$Species=="ACRU" & scalars.annual$Factor=="Competition",]) # MLE range: 0.19 - 0.60
# summary(scalars.annual[scalars.annual$Species=="BELE" & scalars.annual$Factor=="Competition",]) # MLE range: 0.4433 - 1.000
# summary(scalars.annual[scalars.annual$Species=="NYSY" & scalars.annual$Factor=="Competition",]) # MLE range: 0.4775 - 0.7870
# summary(scalars.annual[scalars.annual$Species=="QUPR" & scalars.annual$Factor=="Competition",]) # MLE range: 0.9246 - 0.9938
# summary(scalars.annual[scalars.annual$Species=="QURU" & scalars.annual$Factor=="Competition",]) # MLE range: 0.6572 - 1.000

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
# Species data distributions
##################################################################################
summary(model.data[model.data$Spp=="ACRU",])

n.acru <- length(data.x[data.x$Species=="ACRU" & data.x$Factor=="Size", "X"])
n.bele <- length(data.x[data.x$Species=="BELE" & data.x$Factor=="Size", "X"])
n.nysy <- length(data.x[data.x$Species=="NYSY" & data.x$Factor=="Size", "X"])
n.qupr <- length(data.x[data.x$Species=="QUPR" & data.x$Factor=="Size", "X"])
n.quru <- length(data.x[data.x$Species=="QURU" & data.x$Factor=="Size", "X"])

# levels(scalars.annual$Factor) <- c("Size (cm2)", "Competition (Rel. BA)", "Precipitation (mm yr-1)", "Temperature ˚C")
# levels(data.x$Factor) <- c("Size (cm2)", "Competition (Rel. BA)", "Precipitation (mm yr-1)", "Temperature ˚C")

pdf("Figures/Species_Distributions_Appendix_ACRU.pdf")
ggplot(data=scalars.annual[scalars.annual$Species=="ACRU",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species=="ACRU",], aes(x=X, weight=(1/n.acru)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values="purple") + scale_fill_manual(values="purple") +
	scale_x_continuous(name="") + scale_y_continuous(name="Percent", limits=c(0,101)) +
	guides(fill=F, color=F) +
	theme(axis.line=element_line(color="black", size=0.5), 
		panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
		panel.border= element_blank(), panel.background= element_blank(), 
		axis.text.x=element_text(color="black", size=rel(1.5), vjust=0.5, angle=45), 
		axis.text.y=element_text(color="black", size=rel(1.5)), 
		axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  
		axis.title.y=element_text(face="bold", size=rel(1.5), vjust=0.5), 
		plot.margin=unit(c(0.1,0.1,0.1,0.1), "lines")) +
	theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()

pdf("Figures/Species_Distributions_Appendix_BELE.pdf")
ggplot(data=scalars.annual[scalars.annual$Species=="BELE",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species=="BELE",], aes(x=X, weight=(1/n.bele)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values="blue") + scale_fill_manual(values="blue") +
	scale_x_continuous(name="") + scale_y_continuous(name="Percent", limits=c(0,101)) +
	guides(fill=F, color=F) +
	theme(axis.line=element_line(color="black", size=0.5), 
		panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
		panel.border= element_blank(), panel.background= element_blank(), 
		axis.text.x=element_text(color="black", size=rel(1.5), vjust=0.5, angle=45), 
		axis.text.y=element_text(color="black", size=rel(1.5)), 
		axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  
		axis.title.y=element_text(face="bold", size=rel(1.5), vjust=0.5), 
		plot.margin=unit(c(0.1,0.1,0.1,0.1), "lines")) +
	theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()

pdf("Figures/Species_Distributions_Appendix_NYSY.pdf")
ggplot(data=scalars.annual[scalars.annual$Species=="NYSY",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species=="NYSY",], aes(x=X, weight=(1/n.nysy)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values="green3") + scale_fill_manual(values="green3") +
	scale_x_continuous(name="") + scale_y_continuous(name="Percent", limits=c(0,101)) +
	guides(fill=F, color=F) +
	theme(axis.line=element_line(color="black", size=0.5), 
		panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
		panel.border= element_blank(), panel.background= element_blank(), 
		axis.text.x=element_text(color="black", size=rel(1.5), vjust=0.5, angle=45), 
		axis.text.y=element_text(color="black", size=rel(1.5)), 
		axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  
		axis.title.y=element_text(face="bold", size=rel(1.5), vjust=0.5), 
		plot.margin=unit(c(0.1,0.1,0.1,0.1), "lines")) +
	theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()

pdf("Figures/Species_Distributions_Appendix_QUPR.pdf")
ggplot(data=scalars.annual[scalars.annual$Species=="QUPR",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species=="QUPR",], aes(x=X, weight=(1/n.qupr)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values="orange") + scale_fill_manual(values="orange") +
	scale_x_continuous(name="") + scale_y_continuous(name="Percent", limits=c(0,101)) +
	guides(fill=F, color=F) +
	theme(axis.line=element_line(color="black", size=0.5), 
		panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
		panel.border= element_blank(), panel.background= element_blank(), 
		axis.text.x=element_text(color="black", size=rel(1.5), vjust=0.5, angle=45), 
		axis.text.y=element_text(color="black", size=rel(1.5)), 
		axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  
		axis.title.y=element_text(face="bold", size=rel(1.5), vjust=0.5), 
		plot.margin=unit(c(0.1,0.1,0.1,0.1), "lines")) +
	theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()

pdf("Figures/Species_Distributions_Appendix_QURU.pdf")
ggplot(data=scalars.annual[scalars.annual$Species=="QURU",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species=="QURU",], aes(x=X, weight=(1/n.quru)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values="red") + scale_fill_manual(values="red") +
	scale_x_continuous(name="") + scale_y_continuous(name="Percent", limits=c(0,101)) +
	guides(fill=F, color=F) +
	theme(axis.line=element_line(color="black", size=0.5), 
		panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
		panel.border= element_blank(), panel.background= element_blank(), 
		axis.text.x=element_text(color="black", size=rel(1.5), vjust=0.5, angle=45), 
		axis.text.y=element_text(color="black", size=rel(1.5)), 
		axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  
		axis.title.y=element_text(face="bold", size=rel(1.5), vjust=0.5), 
		plot.margin=unit(c(0.1,0.1,0.1,0.1), "lines")) +
	theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()