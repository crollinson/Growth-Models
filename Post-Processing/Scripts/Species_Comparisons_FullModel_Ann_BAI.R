####################################################################
# Comparing Niches from full model for different species.conds
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

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.5), plot.margin=unit(c(2,2,2,2), "lines"))



model.data <- read.csv("Inputs/TargetSpecies_AllSites_Ann_1990-2011.csv")
summary(model.data)
length(unique(model.data$TreeID))
summary(model.data)

tree.data <- read.csv("../RawInputs/TreeData.csv")
tree.data$BA.tree.cm2 <- tree.data$BA.tree/100
summary(tree.data)

param.est <- read.csv("Inputs/SpeciesModelParameters_Annual.csv")
summary(param.est)

####################################################################
# Creating quasi-bootstrapped distributions for each parameter
# Sampling from the range (lower_lim to upper_lim) for each parameter
####################################################################
# param.est[param.est$Species=="QURU",]

param.distrib <- data.frame(array(dim=c(nrow(param.est),202))) # ncol= num runs + parameter + species.conds
names(param.distrib) <- c("Species", "Parameter", paste("X", 1:200, sep=""))
names(param.distrib)
param.distrib[,c("Species", "Parameter")] <- param.est[,c("Species", "Parameter")]
param.distrib[1:30,1:4]

# n = number of draws to do
n <- 200

for(s in unique(param.distrib$Species)){
	for(j in unique(param.distrib$Parameter)){
		p.range <- seq(from=param.est[param.est$Species==s & param.est$Parameter==j, "Lower.Lim"], to=param.est[param.est$Species==s & param.est$Parameter==j, "Upper.Lim"], length.out=500)

		temp <- sample(p.range, n)
		param.distrib[param.distrib$Species==s & param.distrib$Parameter==j, 3:(3+n-1)] <- temp
	}
}

param.distrib[1:30,1:10]
summary(param.distrib[,1:20])

param.means <- rowMeans(param.distrib[,3:202])


##################################################################################
# Creating X-vectors & finding the mean conditions for a tree
##################################################################################
# Getting the X conditions
x.temp <- seq(min(model.data$Tavg)+273, max(model.data$Tavg)+273, length.out=250)
x.precip <- seq(min(model.data$Precip.PRISM.sum), max(model.data$Precip.PRISM.sum), length.out=250)
x.flow <- mean(min(model.data$flow), max(model.data$flow), length.out=250)
x.comp <- seq(0, 1, length.out=250)
x.ba <- seq(0,max(model.data$BA.m2ha.plot.live), length=200)

species.conds <- as.data.frame(c("ACRU", "BELE", "NYSY", "QUPR", "QURU"))
names(species.conds) <- "Species"
species.conds

# Finding the mean condition for each species.conds
for(i in unique(species.conds$Species)){
	species.conds[species.conds$Species==i,"Temp"] <- mean(model.data[model.data$TreeID, "Tavg"], na.rm=T)
	species.conds[species.conds$Species==i,"Precip"] <- mean(model.data[model.data$TreeID, "Precip.PRISM.sum"], na.rm=T)
	species.conds[species.conds$Species==i,"Flow"] <- mean(model.data[model.data$TreeID, "flow"], na.rm=T)

	trees.all <-  tree.data[tree.data$Spp==i, "TreeID"]
	trees.dom <- tree.data[tree.data$Spp==i & (tree.data$Canopy=="C" | tree.data$Canopy=="D"), "TreeID"]
	trees.under<- tree.data[tree.data$Spp==i & (tree.data$Canopy=="U" | tree.data$Canopy=="I"), "TreeID"]

	species.conds[species.conds$Species==i,"BA.All"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.tree.cm2"], na.rm=T)
	species.conds[species.conds$Species==i,"RelBA.All"] <- mean(model.data[model.data$TreeID %in% trees.dom, "RelBA"], na.rm=T)
	species.conds[species.conds$Species==i,"PlotBA.All"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.m2ha.plot.live"], na.rm=T)

	species.conds[species.conds$Species==i,"BA.Dom"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.tree.cm2"], na.rm=T)
	species.conds[species.conds$Species==i,"RelBA.Dom"] <- mean(model.data[model.data$TreeID %in% trees.dom, "RelBA"], na.rm=T)
	species.conds[species.conds$Species==i,"PlotBA.Dom"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.m2ha.plot.live"], na.rm=T)


	species.conds[species.conds$Species==i,"BA.Under"] <- mean(model.data[model.data$TreeID %in% trees.under, "BA.tree.cm2"], na.rm=T)
	species.conds[species.conds$Species==i,"RelBA.Under"] <- mean(model.data[model.data$TreeID %in% trees.under, "RelBA"], na.rm=T)
	species.conds[species.conds$Species==i,"PlotBA.Under"] <- mean(model.data[model.data$TreeID %in% trees.under, "BA.m2ha.plot.live"], na.rm=T)
}
summary(species.conds)



##################################################################################
# Temp - All Trees
##################################################################################
# Data frame where each run will be placed
full.temp <- data.frame(array(dim=c(length(x.temp),1)))
row.names(full.temp) <- x.temp

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.all.temp <- data.frame(array(dim=c(length(x.temp),1)))
names(full.response.all.temp) <- "x.temp"
full.response.all.temp[,1] <- x.temp


for(s in unique(param.distrib$Species)){
	SIZE= species.conds[species.conds$Species==s, "BA.All"]
	RS=species.conds[species.conds$Species==s, "RelBA.All"] 
	BA.PLOT=species.conds[species.conds$Species==s, "PlotBA.All"]
	TEMP=x.temp
	PRECIP= species.conds[species.conds$Species==s, "Precip"]
	FLOW=species.conds[species.conds$Species==s, "Flow"]

	full.response.all.temp <- full.model.annual.bai(var=TEMP, s, SIZE, RS, BA.PLOT, TEMP, PRECIP, FLOW, full.response.all.temp, param.est, param.distrib)	
	}

summary(full.response.all.temp)

full.temp.stack <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,8)=="MLE"])
names(full.temp.stack) <- c("MLE", "Species")
full.temp.stack$Species <- as.factor(substr(full.temp.stack$Species, 1, 4))
full.temp.stack$x.temp <- full.response.all.temp$x.temp
summary(full.temp.stack)

full.temp.stack2 <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,11)=="CI.low"])
summary(full.temp.stack2)

full.temp.stack3 <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,12)=="CI.high"])
summary(full.temp.stack3)

full.temp.stack4 <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,8)=="Min"])
summary(full.temp.stack4)

full.temp.stack5 <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,8)=="Max"])
summary(full.temp.stack5)

full.temp.stack$CI.low <- full.temp.stack2[,1]
full.temp.stack$CI.hi <- full.temp.stack3[,1]
full.temp.stack$Min <- full.temp.stack4[,1]
full.temp.stack$Max <- full.temp.stack5[,1]

summary(full.temp.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Temperature All Sizes.pdf")
ggplot(data=full.temp.stack) + large.axes +
	geom_ribbon(aes(x=x.temp, ymin=Min, ymax=Max, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors, guide=guide_legend(ncol=2)) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Mean Temp (K)") + 
	# scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.94), legend.text=element_text(size=12), legend.title=element_text(size=12)) + labs(fill="Species")
dev.off()



##################################################################################
# Precip - All Trees
##################################################################################
# Data frame where each run will be placed
full.precip <- data.frame(array(dim=c(length(x.precip),1)))
row.names(full.precip) <- x.precip

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.all.precip <- data.frame(array(dim=c(length(x.precip),1)))
names(full.response.all.precip) <- "x.precip"
full.response.all.precip[,1] <- x.precip


for(s in unique(param.distrib$Species)){
	SIZE= species.conds[species.conds$Species==s, "BA.All"]
	RS=species.conds[species.conds$Species==s, "RelBA.All"] 
	BA.PLOT=species.conds[species.conds$Species==s, "PlotBA.All"]
	TEMP=species.conds[species.conds$Species==s, "Temp"]+273.15
	PRECIP= x.precip
	FLOW= species.conds[species.conds$Species==s, "Flow"]

	full.response.all.precip <- full.model.annual.bai(var=PRECIP, s, SIZE, RS, BA.PLOT, TEMP, PRECIP, FLOW, full.response.all.precip, param.est, param.distrib)	

	}

summary(full.response.all.precip)

full.precip.stack <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,8)=="MLE"])
names(full.precip.stack) <- c("MLE", "Species")
full.precip.stack$Species <- as.factor(substr(full.precip.stack$Species, 1, 4))
full.precip.stack$x.precip <- full.response.all.precip$x.precip
summary(full.precip.stack)

full.precip.stack2 <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,11)=="CI.low"])
summary(full.precip.stack2)

full.precip.stack3 <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,12)=="CI.high"])
summary(full.precip.stack3)

full.precip.stack4 <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,8)=="Min"])
summary(full.precip.stack4)

full.precip.stack5 <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,8)=="Max"])
summary(full.precip.stack5)

full.precip.stack$CI.low <- full.precip.stack2[,1]
full.precip.stack$CI.hi <- full.precip.stack3[,1]
full.precip.stack$Min <- full.precip.stack4[,1]
full.precip.stack$Max <- full.precip.stack5[,1]

summary(full.precip.stack)


species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Precipitation All Sizes.pdf")
ggplot(data=full.precip.stack) + large.axes +
	geom_ribbon(aes(x=x.precip, ymin=Min, ymax=Max, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors, guide=guide_legend(ncol=2)) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Total Precip (mm)") + #scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.91), legend.text=element_text(size=14), legend.title=element_text(size=14)) + labs(fill="Species")
dev.off()





##################################################################################
# Competition - All Trees
##################################################################################
x.relba <- seq(0, 1, length=250)
summary(x.relba)
plot.BA <- mean(model.data$BA.m2ha.plot.live)


summary(species.conds)

# Data frame where each run will be placed
full.comp <- data.frame(array(dim=c(length(x.relba),1)))
row.names(full.comp) <- x.relba

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.all.comp <- data.frame(array(dim=c(length(x.relba),1)))
names(full.response.all.comp) <- "x.relba"
full.response.all.comp[,1] <- x.relba


for(s in unique(param.distrib$Species)){
	SIZE= species.conds[species.conds$Species==s, "BA.All"]
	RS= x.relba
	BA.PLOT=species.conds[species.conds$Species==s, "PlotBA.All"]
	TEMP=species.conds[species.conds$Species==s, "Temp"]+273.15
	PRECIP=species.conds[species.conds$Species==s, "Precip"]
	FLOW= species.conds[species.conds$Species==s, "Flow"]

	full.response.all.comp <- full.model.annual.bai(var=RS, s, SIZE, RS, BA.PLOT, TEMP, PRECIP, FLOW, full.response.all.comp, param.est, param.distrib)	
	}

summary(full.response.all.comp)

full.comp.stack <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,8)=="MLE"])
names(full.comp.stack) <- c("MLE", "Species")
full.comp.stack$Species <- as.factor(substr(full.comp.stack$Species, 1, 4))
full.comp.stack$x.relba <- full.response.all.comp$x.relba
summary(full.comp.stack)

full.comp.stack2 <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,11)=="CI.low"])
summary(full.comp.stack2)

full.comp.stack3 <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,12)=="CI.high"])
summary(full.comp.stack3)

full.comp.stack4 <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,8)=="Min"])
summary(full.comp.stack4)

full.comp.stack5 <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,8)=="Max"])
summary(full.comp.stack5)

full.comp.stack$CI.low <- full.comp.stack2[,1]
full.comp.stack$CI.hi <- full.comp.stack3[,1]
full.comp.stack$Min <- full.comp.stack4[,1]
full.comp.stack$Max <- full.comp.stack5[,1]

summary(full.comp.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Competition RelBA All Sizes.pdf")
ggplot(data=full.comp.stack) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=Min, ymax=Max, fill=Species), alpha=0.3) +
	geom_line(aes(x= x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + 	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()


##################################################################################
# Autogenic - All Trees
##################################################################################
x.auto <- seq(0, max(model.data$BA.tree.cm2), length=250)
summary(x.auto)
plot.BA <- mean(model.data$BA.m2ha.plot.live)

summary(species.conds)

# Data frame where each run will be placed
full.auto <- data.frame(array(dim=c(length(x.auto),1)))
row.names(full.auto) <- x.auto

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.all.auto <- data.frame(array(dim=c(length(x.auto),1)))
names(full.response.all.auto) <- "x.auto"
full.response.all.auto[,1] <- x.auto


for(s in unique(param.distrib$Species)){
	
	SIZE= x.auto
	RS= species.conds[species.conds$Species==s, "RelBA.All"]
	BA.PLOT=species.conds[species.conds$Species==s, "PlotBA.All"]
	TEMP=species.conds[species.conds$Species==s, "Temp"]+273.15
	PRECIP=species.conds[species.conds$Species==s, "Precip"]
	FLOW= species.conds[species.conds$Species==s, "Flow"]

	full.response.all.auto <- full.model.annual.bai(var=SIZE, s, SIZE, RS, BA.PLOT, TEMP, PRECIP, FLOW, full.response.all.auto, param.est, param.distrib)	
	}

summary(full.response.all.auto)

full.auto.stack <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,8)=="MLE"])
names(full.auto.stack) <- c("MLE", "Species")
full.auto.stack$Species <- as.factor(substr(full.auto.stack$Species, 1, 4))
full.auto.stack$x.auto <- full.response.all.auto$x.auto
summary(full.auto.stack)

full.auto.stack2 <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,11)=="CI.low"])
summary(full.auto.stack2)

full.auto.stack3 <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,12)=="CI.high"])
summary(full.auto.stack3)

full.auto.stack4 <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,8)=="Min"])
summary(full.auto.stack4)

full.auto.stack5 <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,8)=="Max"])
summary(full.auto.stack5)

full.auto.stack$CI.low <- full.auto.stack2[,1]
full.auto.stack$CI.hi <- full.auto.stack3[,1]
full.auto.stack$Min <- full.auto.stack4[,1]
full.auto.stack$Max <- full.auto.stack5[,1]

summary(full.auto.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Autogenic All Sizes.pdf")
ggplot(data=full.auto.stack) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=Min, ymax=Max, fill=Species), alpha=0.3) +
	geom_line(aes(x= x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(breaks=c(0,2500,5000)) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()




##################################################################################
# Temperature with different Precip
##################################################################################
# Data frame where each run will be placed
full.temp <- data.frame(array(dim=c(length(x.temp),1)))
row.names(full.temp) <- x.temp

# Data frame where the summary (mean & SD) of the runs will be placed
temp.response.mean <- temp.response.min <- temp.response.max <- data.frame(array(dim=c(length(x.temp),1)))
names(temp.response.mean) <- names(temp.response.max) <- names(temp.response.min) <- "x.temp"
temp.response.mean[,1] <- temp.response.max[,1] <- temp.response.min[,1] <- x.temp


for(s in unique(param.distrib$Species)){

	SIZE= species.conds[species.conds$Species==s, "BA.All"]
	RS=species.conds[species.conds$Species==s, "RelBA.All"] 
	BA.PLOT=species.conds[species.conds$Species==s, "PlotBA.All"]
	TEMP=x.temp
	FLOW=species.conds[species.conds$Species==s, "Flow"]

	PRECIP.mean=mean(model.data$Precip.PRISM.sum)
	PRECIP.min= min(model.data$Precip.PRISM.sum)
	PRECIP.max= max(model.data$Precip.PRISM.sum)


	temp.response.mean <- full.model.annual.bai(var=TEMP, s, SIZE, RS, BA.PLOT, TEMP, PRECIP.mean, FLOW, temp.response.mean, param.est, param.distrib)	

	temp.response.min <- full.model.annual.bai(var=TEMP, s, SIZE, RS, BA.PLOT, TEMP, PRECIP.min, FLOW, temp.response.min, param.est, param.distrib)	

	temp.response.max <- full.model.annual.bai(var=TEMP, s, SIZE, RS, BA.PLOT, TEMP, PRECIP.max, FLOW, temp.response.max, param.est, param.distrib)	
	}

summary(temp.response.mean)
summary(temp.response.min)
summary(temp.response.max)

# -----------------------
temp.mean.stack <- stack(temp.response.mean[,substr(names(temp.response.mean),6,8)=="MLE"])
names(temp.mean.stack) <- c("MLE", "Species")
temp.mean.stack$Species <- as.factor(substr(temp.mean.stack$Species, 1, 4))
temp.mean.stack$x.temp <- temp.response.mean$x.temp
summary(temp.mean.stack)

temp.mean.stack2 <- stack(temp.response.mean[,substr(names(temp.response.mean),6,11)=="CI.low"])
summary(temp.mean.stack2)

temp.mean.stack3 <- stack(temp.response.mean[,substr(names(temp.response.mean),6,12)=="CI.high"])
summary(temp.mean.stack3)

temp.mean.stack4 <- stack(temp.response.mean[,substr(names(temp.response.mean),6,8)=="Min"])
summary(temp.mean.stack4)

temp.mean.stack5 <- stack(temp.response.mean[,substr(names(temp.response.mean),6,8)=="Max"])
summary(temp.mean.stack5)

temp.mean.stack$CI.low <- temp.mean.stack2[,1]
temp.mean.stack$CI.hi <- temp.mean.stack3[,1]
temp.mean.stack$Min <- temp.mean.stack4[,1]
temp.mean.stack$Max <- temp.mean.stack5[,1]
temp.mean.stack$Precip <- as.factor("Mean")

summary(temp.mean.stack)
# -----------------------
# -----------------------
temp.min.stack <- stack(temp.response.min[,substr(names(temp.response.min),6,8)=="MLE"])
names(temp.min.stack) <- c("MLE", "Species")
temp.min.stack$Species <- as.factor(substr(temp.min.stack$Species, 1, 4))
temp.min.stack$x.temp <- temp.response.min$x.temp
summary(temp.min.stack)

temp.min.stack2 <- stack(temp.response.min[,substr(names(temp.response.min),6,11)=="CI.low"])
summary(temp.min.stack2)

temp.min.stack3 <- stack(temp.response.min[,substr(names(temp.response.min),6,12)=="CI.high"])
summary(temp.min.stack3)

temp.min.stack4 <- stack(temp.response.min[,substr(names(temp.response.min),6,8)=="Min"])
summary(temp.min.stack4)

temp.min.stack5 <- stack(temp.response.min[,substr(names(temp.response.min),6,8)=="Max"])
summary(temp.min.stack5)

temp.min.stack$CI.low <- temp.min.stack2[,1]
temp.min.stack$CI.hi <- temp.min.stack3[,1]
temp.min.stack$Min <- temp.min.stack4[,1]
temp.min.stack$Max <- temp.min.stack5[,1]
temp.min.stack$Precip <- as.factor("Minimum")

summary(temp.min.stack)
# -----------------------
# -----------------------
temp.max.stack <- stack(temp.response.max[,substr(names(temp.response.max),6,8)=="MLE"])
names(temp.max.stack) <- c("MLE", "Species")
temp.max.stack$Species <- as.factor(substr(temp.max.stack$Species, 1, 4))
temp.max.stack$x.temp <- temp.response.max$x.temp
summary(temp.max.stack)

temp.max.stack2 <- stack(temp.response.max[,substr(names(temp.response.max),6,11)=="CI.low"])
summary(temp.max.stack2)

temp.max.stack3 <- stack(temp.response.max[,substr(names(temp.response.max),6,12)=="CI.high"])
summary(temp.max.stack3)

temp.max.stack4 <- stack(temp.response.max[,substr(names(temp.response.max),6,8)=="Min"])
summary(temp.max.stack4)

temp.max.stack5 <- stack(temp.response.max[,substr(names(temp.response.max),6,8)=="Max"])
summary(temp.max.stack5)

temp.max.stack$CI.low <- temp.max.stack2[,1]
temp.max.stack$CI.hi <- temp.max.stack3[,1]
temp.max.stack$Min <- temp.max.stack4[,1]
temp.max.stack$Max <- temp.max.stack5[,1]
temp.max.stack$Precip <- as.factor("Maximum")

summary(temp.mean.stack)
summary(temp.min.stack)
summary(temp.max.stack)
# -----------------------

temp.stack <- rbind(temp.mean.stack, temp.min.stack, temp.max.stack)
summary(temp.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Figures/Annual Precipitation and Temperature.pdf", width=11, height=8.5)
ggplot(data=temp.stack) + large.axes + facet_wrap(~ Species) +
	geom_ribbon(aes(x=x.temp-273.15, ymin=Min, ymax=Max, fill=Precip), alpha=0.2) +
	geom_line(aes(x=x.temp-273.15, y=MLE, color=Precip, linetype=Precip), size=1) +
	scale_color_manual(values=c("black", "red", "blue")) + 
	scale_fill_manual(values=c("black", "red", "blue")) +
#	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	scale_x_continuous(name=expression(bold(paste("Mean Annual Temperature ("^"o","C)")))) + 
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.85,0.25), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Precipitation", color="Precipitation", linetype="Precipitation")
dev.off()




##################################################################################
# Precipitation with different Temp
##################################################################################
# Data frame where each run will be placed
full.precip <- data.frame(array(dim=c(length(x.precip),1)))
row.names(full.precip) <- x.precip

# Data frame where the summary (mean & SD) of the runs will be placed
precip.response.mean <- precip.response.min <- precip.response.max <- data.frame(array(dim=c(length(x.precip),1)))
names(precip.response.mean) <- names(precip.response.max) <- names(precip.response.min) <- "x.precip"
precip.response.mean[,1] <- precip.response.max[,1] <- precip.response.min[,1] <- x.precip


for(s in unique(param.distrib$Species)){

	SIZE= species.conds[species.conds$Species==s, "BA.All"]
	RS=species.conds[species.conds$Species==s, "RelBA.All"] 
	BA.PLOT=species.conds[species.conds$Species==s, "PlotBA.All"]
	PRECIP=x.precip
	FLOW=species.conds[species.conds$Species==s, "Flow"]

	TEMP.mean=mean(model.data$Tavg)+273.15
	TEMP.min= min(model.data$Tavg)+273.15
	TEMP.max= max(model.data$Tavg)+273.15


	precip.response.mean <- full.model.annual.bai(var=PRECIP, s, SIZE, RS, BA.PLOT, TEMP.mean, PRECIP, FLOW, precip.response.mean, param.est, param.distrib)	

	precip.response.min <- full.model.annual.bai(var=PRECIP, s, SIZE, RS, BA.PLOT, TEMP.min, PRECIP, FLOW, precip.response.min, param.est, param.distrib)	

	precip.response.max <- full.model.annual.bai(var=PRECIP, s, SIZE, RS, BA.PLOT, TEMP.max, PRECIP, FLOW, precip.response.max, param.est, param.distrib)	
	}

summary(precip.response.mean)
summary(precip.response.min)
summary(precip.response.max)

# -----------------------
precip.mean.stack <- stack(precip.response.mean[,substr(names(precip.response.mean),6,8)=="MLE"])
names(precip.mean.stack) <- c("MLE", "Species")
precip.mean.stack$Species <- as.factor(substr(precip.mean.stack$Species, 1, 4))
precip.mean.stack$x.precip <- precip.response.mean$x.precip
summary(precip.mean.stack)

precip.mean.stack2 <- stack(precip.response.mean[,substr(names(precip.response.mean),6,11)=="CI.low"])
summary(precip.mean.stack2)

precip.mean.stack3 <- stack(precip.response.mean[,substr(names(precip.response.mean),6,12)=="CI.high"])
summary(precip.mean.stack3)

precip.mean.stack4 <- stack(precip.response.mean[,substr(names(precip.response.mean),6,8)=="Min"])
summary(precip.mean.stack4)

precip.mean.stack5 <- stack(precip.response.mean[,substr(names(precip.response.mean),6,8)=="Max"])
summary(precip.mean.stack5)

precip.mean.stack$CI.low <- precip.mean.stack2[,1]
precip.mean.stack$CI.hi <- precip.mean.stack3[,1]
precip.mean.stack$Min <- precip.mean.stack4[,1]
precip.mean.stack$Max <- precip.mean.stack5[,1]
precip.mean.stack$Precip <- as.factor("Mean")

summary(precip.mean.stack)
# -----------------------
# -----------------------
precip.min.stack <- stack(precip.response.min[,substr(names(precip.response.min),6,8)=="MLE"])
names(precip.min.stack) <- c("MLE", "Species")
precip.min.stack$Species <- as.factor(substr(precip.min.stack$Species, 1, 4))
precip.min.stack$x.precip <- precip.response.min$x.precip
summary(precip.min.stack)

precip.min.stack2 <- stack(precip.response.min[,substr(names(precip.response.min),6,11)=="CI.low"])
summary(precip.min.stack2)

precip.min.stack3 <- stack(precip.response.min[,substr(names(precip.response.min),6,12)=="CI.high"])
summary(precip.min.stack3)

precip.min.stack4 <- stack(precip.response.min[,substr(names(precip.response.min),6,8)=="Min"])
summary(precip.min.stack4)

precip.min.stack5 <- stack(precip.response.min[,substr(names(precip.response.min),6,8)=="Max"])
summary(precip.min.stack5)

precip.min.stack$CI.low <- precip.min.stack2[,1]
precip.min.stack$CI.hi <- precip.min.stack3[,1]
precip.min.stack$Min <- precip.min.stack4[,1]
precip.min.stack$Max <- precip.min.stack5[,1]
precip.min.stack$Precip <- as.factor("Minimum")

summary(precip.min.stack)
# -----------------------
# -----------------------
precip.max.stack <- stack(precip.response.max[,substr(names(precip.response.max),6,8)=="MLE"])
names(precip.max.stack) <- c("MLE", "Species")
precip.max.stack$Species <- as.factor(substr(precip.max.stack$Species, 1, 4))
precip.max.stack$x.precip <- precip.response.max$x.precip
summary(precip.max.stack)

precip.max.stack2 <- stack(precip.response.max[,substr(names(precip.response.max),6,11)=="CI.low"])
summary(precip.max.stack2)

precip.max.stack3 <- stack(precip.response.max[,substr(names(precip.response.max),6,12)=="CI.high"])
summary(precip.max.stack3)

precip.max.stack4 <- stack(precip.response.max[,substr(names(precip.response.max),6,8)=="Min"])
summary(precip.max.stack4)

precip.max.stack5 <- stack(precip.response.max[,substr(names(precip.response.max),6,8)=="Max"])
summary(precip.max.stack5)

precip.max.stack$CI.low <- precip.max.stack2[,1]
precip.max.stack$CI.hi <- precip.max.stack3[,1]
precip.max.stack$Min <- precip.max.stack4[,1]
precip.max.stack$Max <- precip.max.stack5[,1]
precip.max.stack$Precip <- as.factor("Maximum")

summary(precip.mean.stack)
summary(precip.min.stack)
summary(precip.max.stack)
# -----------------------

precip.stack <- rbind(precip.mean.stack, precip.min.stack, precip.max.stack)
summary(precip.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Figures/Annual Temperature and Precipitation.pdf", width=11, height=8.5)
ggplot(data=precip.stack) + large.axes + facet_wrap(~ Species) +
	geom_ribbon(aes(x=x.precip, ymin=Min, ymax=Max, fill=Precip), alpha=0.2) +
	geom_line(aes(x=x.precip, y=MLE, color=Precip, linetype=Precip), size=1) +
	scale_color_manual(values=c("black", "blue", "red")) + 
	scale_fill_manual(values=c("black", "blue", "red")) +
#	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	scale_x_continuous(name="Total Annual Precipitation (mm)", breaks=c(800,1200,1600)) + 
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.85,0.25), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Temperature", color="Temperature", linetype="Temperature")
dev.off()