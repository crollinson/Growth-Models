####################################################################
# Comparing Niches from full model for different species
# Climate is from year of BAI measurement (mean Tavg, total Precip)
####################################################################
rm(list=ls())
# load("Species_Comparisons_FullModel_Ann.RData")
setwd("..")
source("Scripts/GrowthEffect_Functions.R")
source("Scripts/GrowthModel_Functions.R")

library(likelihood)
library(ggplot2)
library(raster)
library(grid)
library(kobe)

# min(model.data$temp.yr)-273.15;max(model.data$temp.yr)-273.15
# min(model.data$ppt.yr);max(model.data$ppt.yr)

# mean(model.data$temp.yr)-273.15; sd(model.data$temp.yr)
# mean(model.data$ppt.yr); sd(model.data$ppt.yr)

# names(model.data)
# unique(model.data[model.data$temp.yr==min(model.data$temp.yr), c("PlotID", "Year")])
# unique(model.data[model.data$temp.yr==min(model.data$temp.yr), c("PlotID", "Year")])

# (max(model.data$temp.yr)-273.15)-(min(model.data$temp.yr)-273.15)

# # all.data <- read.csv("../RawInputs/CARCA_CoreData_Climate_Annual.csv")
# all.data$BA.tree.cm2 <- all.data$BA.tree/100
# summary(all.data)

# species <- c("QURU", "QUPR", "NYSY", "BELE", "ACRU")

# model.data <- all.data[all.data$Spp %in% species & all.data$Year>=1990 & all.data$Year<=2011,]
# model.data <- model.data[complete.cases(model.data),]
# summary(model.data)
# write.csv(model.data, "Inputs/TargetSpecies_AllSites_Ann_1990-2011.csv", row.names=F)

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.5), plot.margin=unit(c(2,2,2,2), "lines"))


####################################################################

model.data <- read.csv("Inputs/TargetSpecies_AllSites_Ann_1990-2011.csv")
summary(model.data)
length(unique(model.data$TreeID))

# months <- c("X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09", "X10", "X11", "X12")

# months.use <- c(months[1:12]) # current March through October
# length(months.use)

# # Making vectors with the column names of the temp & precip months of interest
# temp.col <- paste("Tavg", months.use, sep=".")
# precip.col <- paste("Precip", months.use, sep=".")

# # Column numbers of temp & precip months of interest
# temp.col.ind <- which(names(model.data) %in% c(temp.col))
# precip.col.ind <- which(names(model.data) %in% c(precip.col))
# summary(model.data[model.data,precip.col.ind])
# length(precip.col.ind)

# # Making Temperature in Kelvin
# model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]

# # Creating columns to summarize the year
# model.data$ppt.yr <- rowSums(model.data[,precip.col])
# model.data$temp.yr <- rowMeans(model.data[,temp.col])

# summary(model.data)


# # # Plotting climate space of data
# climate.data <- aggregate(model.data[,c("Tavg", "Precip.PRISM.sum")], by=list(model.data$PlotID, model.data$Year), FUN=mean)
# names(climate.data) <- c("PlotID", "Year", "temp.yr", "ppt.yr")
# climate.data$temp.yr.round <- round(climate.data$temp.yr, digits=1)
# climate.data$ppt.yr.round <- round(climate.data$ppt.yr, digits=-1)
# summary(climate.data)

# climate.data2 <- aggregate(climate.data[,"temp.yr.round"], by=list(climate.data$ppt.yr.round, climate.data$temp.yr.round), FUN=length)
# names(climate.data2) <- c("Precip.yr.round", "Temp.yr.round", "Frequency")
# summary(climate.data2)

# # pdf("Figures/ClimateSpace_ModelData.pdf")
# ggplot(climate.data2, aes(x= Precip.yr.round, y=Temp.yr.round, fill=Frequency)) + large.axes +
	# geom_tile() + scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Annual Mean Min Temp (C)") + scale_fill_gradientn(colours=bpy.colors(500), "Frequency") + large.axes + theme(legend.position=c(0.97,0.85), legend.text=element_text(size=14), legend.title=element_text(size=14)) + guides(fill=guide_colorbar(barwidth=2, barheight=5))
# # dev.off()

####################################################################

# ####################################################################
# # Making a csv with parameter estimates
# ####################################################################
# load("../Annual/Outputs/ACRU_Full_Ann_Results.Rdata")
# acru.results <- results
# load("../Annual/Outputs/BELE_Full_Ann_Results.Rdata")
# bele.results <- results
# load("../Annual/Outputs/NYSY_Full_Ann_Results.Rdata")
# nysy.results <- results
# load("../Annual/Outputs/QUPR_Full_Ann_Results.Rdata")
# qupr.results <- results
# load("../Annual/Outputs/QURU_Full_Ann_Results.Rdata")
# quru.results <- results

# #-----------------------------------
# # Looking at autocorrelation quickly
# #-----------------------------------
# # Calculating residuals
# acru.results$source_data$resid <- acru.results$source_data$BAI - acru.results$source_data$predicted
# bele.results$source_data$resid <- bele.results$source_data$BAI - bele.results$source_data$predicted
# nysy.results$source_data$resid <- nysy.results$source_data$BAI - nysy.results$source_data$predicted
# qupr.results$source_data$resid <- qupr.results$source_data$BAI - qupr.results$source_data$predicted
# quru.results$source_data$resid <- quru.results$source_data$BAI - quru.results$source_data$predicted

# acf(acru.results$source_data$resid, lag.max=20)
# acf(bele.results$source_data$resid, lag.max=20)
# acf(nysy.results$source_data$resid, lag.max=20)
# acf(qupr.results$source_data$resid, lag.max=20)
# acf(quru.results$source_data$resid, lag.max=20)
# #-----------------------------------


# acru.pars <- data.frame(Species="ACRU", Parameter=names(unlist(acru.results$best_pars)), MLE=unlist(acru.results$best_pars), Lower.Lim=unlist(acru.results$lower_limits), Upper.Lim=unlist(acru.results$upper_limits), Lower.Bound=unlist(acru.results$par_lo), Upper.Bound=unlist(acru.results$par_hi))

# bele.pars <- data.frame(Species="BELE", Parameter=names(unlist(bele.results$best_pars)), MLE=unlist(bele.results$best_pars), Lower.Lim=unlist(bele.results$lower_limits), Upper.Lim=unlist(bele.results$upper_limits), Lower.Bound=unlist(bele.results$par_lo), Upper.Bound=unlist(bele.results$par_hi))

# nysy.pars <- data.frame(Species="NYSY", Parameter=names(unlist(nysy.results$best_pars)), MLE=unlist(nysy.results$best_pars), Lower.Lim=unlist(nysy.results$lower_limits), Upper.Lim=unlist(nysy.results$upper_limits), Lower.Bound=unlist(nysy.results$par_lo), Upper.Bound=unlist(nysy.results$par_hi))

# qupr.pars <- data.frame(Species="QUPR", Parameter=names(unlist(qupr.results$best_pars)), MLE=unlist(qupr.results$best_pars), Lower.Lim=unlist(qupr.results$lower_limits), Upper.Lim=unlist(qupr.results$upper_limits), Lower.Bound=unlist(qupr.results$par_lo), Upper.Bound=unlist(qupr.results$par_hi))

# quru.pars <- data.frame(Species="QURU", Parameter=names(unlist(quru.results$best_pars)), MLE=unlist(quru.results$best_pars), Lower.Lim=unlist(quru.results$lower_limits), Upper.Lim=unlist(quru.results$upper_limits), Lower.Bound=unlist(quru.results$par_lo), Upper.Bound=unlist(quru.results$par_hi))

# param.est <- rbind(acru.pars, bele.pars, nysy.pars, qupr.pars, quru.pars)
# write.csv(param.est, "Inputs/SpeciesModelParameters_Annual.csv", row.names=F)

####################################################################
param.est <- read.csv("Inputs/SpeciesModelParameters_Annual.csv")
summary(param.est)
####################################################################


####################################################################
# Creating quasi-bootstrapped distributions for each parameter
# Sampling from the range (lower_lim to upper_lim) for each parameter
####################################################################
# param.est[param.est$Species=="QURU",]
# n = number of draws to do
n <- 500

param.distrib <- data.frame(array(dim=c(nrow(param.est),n+2))) # ncol= num runs + parameter + species
names(param.distrib) <- c("Species", "Parameter", paste("X", 1:n, sep=""))
names(param.distrib)
param.distrib[,c("Species", "Parameter")] <- param.est[,c("Species", "Parameter")]
param.distrib[1:30,1:4]


for(s in unique(param.distrib$Species)){
	for(j in unique(param.distrib$Parameter)){
		p.range <- seq(from=param.est[param.est$Species==s & param.est$Parameter==j, "Lower.Lim"], to=param.est[param.est$Species==s & param.est$Parameter==j, "Upper.Lim"], length.out=n*3)

		temp <- sample(p.range, n)
		param.distrib[param.distrib$Species==s & param.distrib$Parameter==j, 3:(3+n-1)] <- temp
	}
}

param.distrib[1:30,1:10]
summary(param.distrib[,1:20])

param.means <- rowMeans(param.distrib[,3:ncol(param.distrib)])


##################################################################################
# Autogenic: Size = Basal Area of tree (cm2); BootStrapped
##################################################################################
x.auto <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)

##########################
# Some Initial Data Frames
##########################
# Data frame where each run will be placed
# aut.temp <- data.frame(array(dim=c(length(x.auto),1)))
# row.names(aut.temp) <- x.auto

# Data frame where the summary (mean & SD) of the runs will be placed
aut.response <- data.frame(array(dim=c(length(x.auto),1)))
names(aut.response) <- "x.auto"
# aut.response[,1] <- x.auto


for(s in unique(param.distrib$Species)){
	aut.response <- aut.predict(s, x.auto, aut.response, param.est, param.distrib, n=250)
	}

summary(aut.response)

# ---------------------

aut.stack <- stack(aut.response[,substr(names(aut.response),6,11)=="x.auto"])
names(aut.stack) <- c("x.auto", "Species")
aut.stack$Species <- as.factor(substr(aut.stack$Species, 1, 4))
summary(aut.stack)

aut.stack1 <- stack(aut.response[,substr(names(aut.response),6,8)=="MLE"])
summary(aut.stack1)

aut.stack2 <- stack(aut.response[,substr(names(aut.response),6,11)=="CI.low"])
summary(aut.stack2)

aut.stack3 <- stack(aut.response[,substr(names(aut.response),6,12)=="CI.high"])
summary(aut.stack3)

aut.stack4 <- stack(aut.response[,substr(names(aut.response),6,8)=="Min"])
summary(aut.stack4)

aut.stack5 <- stack(aut.response[,substr(names(aut.response),6,8)=="Max"])
summary(aut.stack5)

aut.stack$MLE <- aut.stack1[,1]
aut.stack$CI.low <- aut.stack2[,1]
aut.stack$CI.hi <- aut.stack3[,1]
aut.stack$Min <- aut.stack4[,1]
aut.stack$Max <- aut.stack5[,1]

summary(aut.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")


# pdf("Figures/Species Comparisons Full Model Annual - Autogenic.pdf")
# ggplot(data=aut.stack) + large.axes +
# #	geom_ribbon(aes(x=x.auto, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	# geom_ribbon(aes(x=x.auto, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.auto, y=MLE*100, color=Species, linetype=Species), size=1) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	# scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,100), breaks=seq(0, 100, 25)) +
	# theme(legend.position=c(0.85,0.75), legend.text=element_text(size=14), legend.title=element_text(size=16)) + 
	 # theme(axis.text.x=element_text(angle=0, color="black", size=rel(2.25)), axis.text.y=element_text(color="black", size=rel(2.25)),  axis.title.y=element_text(face="bold", size=rel(2), vjust=0.5), axis.title.x=element_text(face="bold", size=rel(2), vjust=0), plot.margin=unit(c(0.1,0.1,0.2,0.1), "lines")) +
	 # labs(fill="Species")
# dev.off()

# # pdf("Figures/Species Comparisons Full Model Annual - Autogenic Gmax.pdf")
# # ggplot(data=aut.stack) + large.axes +
	# # # geom_ribbon(aes(x=x.auto, ymin=CI.low.gmax, ymax=CI.high.gmax, fill=Species), alpha=0.3) +
	# # geom_ribbon(aes(x=x.auto, ymin=Min.gmax, ymax=Max.gmax, fill=Species), alpha=0.3) +
	# # geom_line(aes(x=x.auto, y=MLE.gmax, color=Species), size=1) +
	# # scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# # xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	# # scale_y_continuous(name="Basal Area Increment (mm2)") +
	# # theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
# # dev.off()




##################################################################################
# Temperature; BootStrapped
##################################################################################
x.temp <- seq(min(model.data$Tavg), max(model.data$Tavg), length=250)+273.15
summary(x.temp)

##########################
# Some Initial Data Frames
##########################

# Data frame where the summary (mean & SD) of the runs will be placed
temp.response <- data.frame(array(dim=c(length(x.temp),1)))
names(temp.response) <- "x.temp"
temp.response[,1] <- x.temp

for(s in unique(param.distrib$Species)){
	temp.response <- temp.ann(s, x.temp, temp.response, param.est, param.distrib, n=250)	
}

summary(temp.response)

temp.stack <- stack(temp.response[,substr(names(temp.response),6,8)=="MLE"])
names(temp.stack) <- c("MLE", "Species")
temp.stack$Species <- as.factor(substr(temp.stack$Species, 1, 4))
temp.stack$x.temp <- temp.response$x.temp
summary(temp.stack)

temp.stack2 <- stack(temp.response[,substr(names(temp.response),6,11)=="CI.low"])
summary(temp.stack2)

temp.stack3 <- stack(temp.response[,substr(names(temp.response),6,12)=="CI.high"])
summary(temp.stack3)

temp.stack4 <- stack(temp.response[,substr(names(temp.response),6,8)=="Min"])
summary(temp.stack4)

temp.stack5 <- stack(temp.response[,substr(names(temp.response),6,8)=="Max"])
summary(temp.stack5)


temp.stack$CI.low <- temp.stack2[,1]
temp.stack$CI.hi <- temp.stack3[,1]
temp.stack$Min <- temp.stack4[,1]
temp.stack$Max <- temp.stack5[,1]


species.colors <- c("purple", "blue", "green3", "orange", "red")

# pdf("Figures/Species Comparisons Full Model Annual - Temperature.pdf")
# ggplot(data=temp.stack) + large.axes +
	# geom_ribbon(aes(x=x.temp-273, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.temp-273, y=MLE*100, color=Species, linetype=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name=expression(bold(paste("Mean Annual Temperature ("^"o","C)")))) + 
	# scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,100)) +
	# theme(legend.position=c(0.8,0.4), legend.text=element_text(size=14), legend.title=element_text(size=16)) + 
	 # theme(axis.text.x=element_text(angle=0, color="black", size=rel(2.25)), axis.text.y=element_text(color="black", size=rel(2.25)),  axis.title.y=element_text(face="bold", size=rel(2), vjust=0.5), axis.title.x=element_text(face="bold", size=rel(2), vjust=0), plot.margin=unit(c(0.1,0.1,0.5,0.1), "lines")) +
	# labs(fill="Species")
# dev.off()




##################################################################################
# Precipitation; BootStrapped
##################################################################################
x.precip <- seq(min(model.data$Precip.PRISM.sum), max(model.data$Precip.PRISM.sum), length=250)
summary(x.precip)

x.flow <- mean(model.data$flow)

##########################
# Some Initial Data Frames
##########################
# Data frame where each run will be placed
# Data frame where the summary (mean & SD) of the runs will be placed
precip.response <- data.frame(array(dim=c(length(x.auto),1)))
names(precip.response) <- "x.precip"
precip.response[,1] <- x.precip

for(s in unique(param.distrib$Species)){
	flow <- mean(model.data$flow)
	precip.response <- precip.ann(s, x.precip, flow, precip.response, param.est, param.distrib, n=250)
	}

summary(precip.response)

precip.stack <- stack(precip.response[,substr(names(precip.response),6,8)=="MLE"])
names(precip.stack) <- c("MLE", "Species")
precip.stack$Species <- as.factor(substr(precip.stack$Species, 1, 4))
precip.stack$x.precip <- precip.response$x.precip
summary(precip.stack)

precip.stack2 <- stack(precip.response[,substr(names(precip.response),6,11)=="CI.low"])
summary(precip.stack2)

precip.stack3 <- stack(precip.response[,substr(names(precip.response),6,12)=="CI.high"])
summary(precip.stack3)

precip.stack4 <- stack(precip.response[,substr(names(precip.response),6,8)=="Min"])
summary(precip.stack4)

precip.stack5 <- stack(precip.response[,substr(names(precip.response),6,8)=="Max"])
summary(precip.stack5)

precip.stack$CI.low <- precip.stack2[,1]
precip.stack$CI.hi <- precip.stack3[,1]
precip.stack$Min <- precip.stack4[,1]
precip.stack$Max <- precip.stack5[,1]

species.colors <- c("purple", "blue", "green3", "orange", "red")

# pdf("Figures/Species Comparisons Full Model Annual - Precipitation.pdf")
# ggplot(data=precip.stack) + large.axes +
	# geom_ribbon(aes(x=x.precip, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.precip, y=MLE*100, color=Species, linetype=Species), size=2) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Annual Precip (mm)", breaks=seq(800, 1600, 200)) + 
	# scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,100)) +
	# theme(legend.position=c(0.2,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + 
	# theme(axis.text.x=element_text(angle=0, color="black", size=rel(2.25)), axis.text.y=element_text(color="black", size=rel(2.25)),  axis.title.y=element_text(face="bold", size=rel(2), vjust=0.5), axis.title.x=element_text(face="bold", size=rel(2), vjust=0), plot.margin=unit(c(0.1,0.1,1.1,0.1), "lines")) +
	# labs(fill="Species")
# dev.off()



##################################################################################
# Competition; BootStrapped
##################################################################################
x.relba <- seq(0,1, length.out=250)
summary(x.relba)
plot.BA <- mean(model.data$BA.m2ha.plot.live)

##########################
# Some Initial Data Frames
##########################
# Data frame where each run will be placed
comp.temp <- data.frame(array(dim=c(length(x.relba),1)))
row.names(comp.temp) <- x.relba

# Data frame where the summary (mean & SD) of the runs will be placed
comp.response <- data.frame(array(dim=c(length(x.relba),1)))
names(comp.response) <- "x.relba"
comp.response[,1] <- x.relba

for(s in unique(param.distrib$Species)){
	x.relba <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=250)
	plot.ba <- mean(model.data$BA.m2ha)
	comp.response <- comp.predict(s, x.relba, plot.ba, comp.response, param.est, param.distrib, n=250)
	}
summary(comp.response)

comp.stack <- stack(comp.response[,substr(names(comp.response),6,10)=="relba"])
names(comp.stack) <- c("x.relba", "Species")
comp.stack$Species <- as.factor(substr(comp.stack$Species, 1, 4))
summary(comp.stack)

comp.stack1 <- stack(comp.response[,substr(names(comp.response),6,8)=="MLE"])

comp.stack2 <- stack(comp.response[,substr(names(comp.response),6,11)=="CI.low"])
summary(comp.stack2)

comp.stack3 <- stack(comp.response[,substr(names(comp.response),6,12)=="CI.high"])
summary(comp.stack3)

comp.stack4 <- stack(comp.response[,substr(names(comp.response),6,8)=="Min"])
summary(comp.stack4)

comp.stack5 <- stack(comp.response[,substr(names(comp.response),6,8)=="Max"])
summary(comp.stack5)


comp.stack$MLE <- comp.stack1[,1]
comp.stack$CI.low <- comp.stack2[,1]
comp.stack$CI.hi <- comp.stack3[,1]
comp.stack$Min <- comp.stack4[,1]
comp.stack$Max <- comp.stack5[,1]

summary(comp.stack)


species.colors <- c("purple", "blue", "green3", "orange", "red")

# pdf("Figures/Species Comparisons Full Model Annual - Competition.pdf")
# ggplot(data=comp.stack) + large.axes +
	# geom_ribbon(aes(x=x.relba, ymin=Max*100, ymax=Min*100, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.relba, y=MLE*100, color=Species, linetype=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Relative Basal Area") + 
	# scale_y_continuous(name="Growth Rate (% Max)", limits=c(0, 100)) +
	# theme(legend.position=c(0.85,0.25), legend.text=element_text(size=14), legend.title=element_text(size=16)) + 
	 # theme(axis.text.x=element_text(angle=0, color="black", size=rel(2.25)), axis.text.y=element_text(color="black", size=rel(2.25)),  axis.title.y=element_text(face="bold", size=rel(2), vjust=0.5), axis.title.x=element_text(face="bold", size=rel(2), vjust=0), plot.margin=unit(c(0.1,0.1,1.1,0.1), "lines")) +
	# labs(fill="Species")
# dev.off()

# # pdf("Figures/Species Comparisons Full Model Annual - Competition Gmax.pdf")
# # ggplot(data=comp.stack) + large.axes +
	# # # geom_ribbon(aes(x=x.relba, ymin=CI.low.gmax, ymax=CI.hi.gmax, fill=Species), alpha=0.4) +	
	# # geom_ribbon(aes(x=x.relba, ymin=Min.gmax, ymax=Max.gmax, fill=Species), alpha=0.4) +
	# # geom_line(aes(x=x.relba, y=MLE.gmax, color=Species), size=2) +
	# # scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# # scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Growth (Basal Area Increment, mm2)") +
	# # theme(legend.position=c(0.8,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
# # dev.off()


##################################################################################
# Multiplot of all Scalars
##################################################################################
q.blank2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1)), axis.text.y=element_text(color="black", size=rel(1)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))

# making all the effects a single data frame
summary(aut.stack)
summary(comp.stack)
summary(precip.stack)
summary(temp.stack)

aut.stack2 <- aut.stack[,1:7]
names(aut.stack2)[1] <- "X"
aut.stack2$Factor <- as.factor("Auto")
levels(aut.stack2$Factor) <- "Size"
summary(aut.stack2)

comp.stack2 <- comp.stack[,1:7]
names(comp.stack2)[1] <- "X"
comp.stack2$Factor <- as.factor("Competition")
summary(comp.stack2)

precip.stack <- precip.stack[,c("x.precip", "Species", "MLE", "CI.low", "CI.hi", "Min", "Max")]
names(precip.stack)[1] <- "X"
precip.stack$Factor <- as.factor("Precipitation")
summary(precip.stack)

temp.stack <- temp.stack[,c("x.temp", "Species", "MLE", "CI.low", "CI.hi", "Min", "Max")]
names(temp.stack)[1] <- "X"
temp.stack$Factor <- as.factor("Temperature")
summary(temp.stack)

scalars.annual <- rbind(aut.stack2, comp.stack2, precip.stack, temp.stack)
summary(scalars.annual)

species.colors <- c("purple", "blue", "green3", "orange", "red")

# ggplot(data=scalars.annual) + facet_wrap(~Factor, scales="free") + q.blank2 +
	# geom_ribbon(aes(x=X, ymin=CI.low, ymax=CI.hi, fill=Species), alpha=0.3) +
	# geom_line(aes(x=X, y=MLE, color=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
    # theme(plot.title=element_text(face="bold")) + 
	# guides(fill=F, color=F)

# # Autogenic
# plot.auto <- ggplot(data=aut.stack) + q.blank2 +
	# geom_ribbon(aes(x=x.auto, ymin= CI.low, ymax= CI.hi, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.auto, y=MLE, color=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	# scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	# ggtitle("Size") + theme(plot.title=element_text(face="bold")) + 
	# guides(fill=F, color=F)

# # Competition
# plot.comp <- ggplot(data=comp.stack) + q.blank2 +
	# geom_ribbon(aes(x=x.relba, ymin= CI.low, ymax= CI.hi, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.relba, y=MLE, color=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0, 1.05)) +
	# ggtitle("Competition") + theme(plot.title=element_text(face="bold")) +
	# guides(fill=F, color=F)

# # Temperature
# plot.temp <- ggplot(data=temp.stack) + q.blank2 +
	# geom_ribbon(aes(x=x.temp, ymin= CI.low, ymax= CI.hi, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.temp, y=MLE, color=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Annual Mean Temp (K)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.05)) + theme(plot.title=element_text(face="bold")) +
	# ggtitle("Temperature") +
	# guides(fill=F, color=F)

# # Precipitation
# plot.precip <- ggplot(data=precip.stack) + q.blank2 +
	# geom_ribbon(aes(x=x.precip, ymin= CI.low, ymax= CI.hi, fill=Species), alpha=0.3) +
	# geom_line(aes(x=x.precip, y=MLE, color=Species), size=1.5) +
	# scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.15), breaks=c(0,0.25,0.5,0.75,1)) +
	# ggtitle("Precipitation") + theme(plot.title=element_text(face="bold")) +
	# theme(legend.position=c(0.2,0.3), legend.text=element_text(size=rel(0.8)), legend.title=element_text(size=rel(1))) + labs(fill="Species")



# # pdf("Species Comparisons Full Model - All Scalars.pdf")
# multiplot(plot.auto, plot.temp, plot.comp,  plot.precip, cols=2)
# # dev.off()

##################################################################################
# Save everything for use elsewhere
##################################################################################
save(scalars.annual, file="Inputs/Scalars_Annual_Bootstrapped.Rdata")









