####################################################################
# Comparing Niches from full model for different species
# Climate is from year of BAI measurement (mean Tavg, total Precip)
####################################################################
rm(list=ls())
load("Species_Comparisons_FullModel_Ann.RData")

library(likelihood)
library(ggplot2)
library(raster)
library(grid)
library(kobe)

min(model.data$temp.yr)-273.15;max(model.data$temp.yr)-273.15
min(model.data$ppt.yr);max(model.data$ppt.yr)

mean(model.data$temp.yr)-273.15; sd(model.data$temp.yr)
mean(model.data$ppt.yr); sd(model.data$ppt.yr)

names(model.data)
unique(model.data[model.data$temp.yr==min(model.data$temp.yr), c("PlotID", "Year")])
unique(model.data[model.data$temp.yr==min(model.data$temp.yr), c("PlotID", "Year")])

(max(model.data$temp.yr)-273.15)-(min(model.data$temp.yr)-273.15)

# all.data <- read.csv("CARCA_CoreData_Tree_Plot_Tavg_Precip_WideFormat.csv")
# all.data$BA.tree.cm2 <- all.data$BA.tree/100
# summary(all.data)

# species <- c("QURU", "QUPR", "NYSY", "BELE", "ACRU")

# model.data <- all.data[all.data$Spp %in% species & all.data$Year>=1990 & all.data$Year<=2011,]
# model.data <- model.data[complete.cases(model.data),]
# summary(model.data)
# write.csv(model.data, "TargetSpecies_AllSites_1990-2011.csv", row.names=F)

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))


####################################################################
# Full Model: BAI ~ Gmax*Autogenic*Competition*Climate

autogenic.effect <- function(SIZE,aa,ab) 
	{ (1 - aa*exp(-ab*SIZE))  }

size.effect <- function(RS,ca,cb,cc) 
	{ ca*exp(-cb*(RS^cc)) }

ba.effect <- function(BA.PLOT,ce,cf,cg) 
	{ ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg)) }

comp.effect <- function(RS,BA.PLOT,ca,cb,cc,cd,ce,cf,cg)
	{ size.effect <- ca*exp(-cb*(RS^cc))
      plot.effect <- (ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg)))
      comp.resp <- exp(-size.effect*(plot.effect^cd)) 
      return(comp.resp)
      }

temp.effect <- function(TEMP,ta1,tb1)
	{ (exp(-0.5*(((TEMP)-ta1)/tb1)^2)) }

precip.effect <- function(PRECIP,FLOW,pa1,pb1,pc1)
	{ (exp(-0.5*(((PRECIP + pc1*FLOW*PRECIP)-pa1)/pb1)^2)) }

bai.model <- function(SIZE,RS,BA.PLOT,TEMP,PRECIP,FLOW,aa,ab,ca,cb,cc,cd,ce,cf,cg,ta1,tb1,pa1,pb1,pc1,gmax)		
	{ aut <- (1 - aa*exp(-ab*SIZE))  
	  size <- ca*exp(-cb*(RS^cc))
	  plot <- ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg))
	  comp <- exp(-size*(plot^cd))
	  temp <- exp(-0.5*(((TEMP)-ta1)/tb1)^2)
	  precip <- exp(-0.5*(((PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2)
	  
	  gmax*aut*comp*temp*precip
	}

####################################################################

model.data <- read.csv("TargetSpecies_AllSites_1990-2011.csv")
summary(model.data)
length(unique(model.data$TreeID))

months <- c("X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09", "X10", "X11", "X12")

months.use <- c(months[1:12]) # current March through October
length(months.use)

# Making vectors with the column names of the temp & precip months of interest
temp.col <- paste("Tavg", months.use, sep=".")
precip.col <- paste("Precip", months.use, sep=".")

# Column numbers of temp & precip months of interest
temp.col.ind <- which(names(model.data) %in% c(temp.col))
precip.col.ind <- which(names(model.data) %in% c(precip.col))
summary(model.data[model.data,precip.col.ind])
length(precip.col.ind)

# Making Temperature in Kelvin
model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]

# Creating columns to summarize the year
model.data$ppt.yr <- rowSums(model.data[,precip.col])
model.data$temp.yr <- rowMeans(model.data[,temp.col])

summary(model.data)


# Plotting climate space of data
climate.data <- aggregate(model.data[,c("ppt.yr", "temp.yr")], by=list(model.data$PlotID, model.data$Year), FUN=mean)
names(climate.data) <- c("PlotID", "Year", "ppt.yr", "temp.yr")
climate.data$temp.yr.round <- round(climate.data$temp.yr, digits=1)
climate.data$ppt.yr.round <- round(climate.data$ppt.yr, digits=-1)
summary(climate.data)

climate.data2 <- aggregate(climate.data[,"temp.yr.round"], by=list(climate.data$ppt.yr.round, climate.data$temp.yr.round), FUN=length)
names(climate.data2) <- c("Precip.yr.round", "Temp.yr.round", "Frequency")
summary(climate.data2)

pdf("ClimateSpace_ModelData.pdf")
ggplot(climate.data2, aes(x= Precip.yr.round, y=Temp.yr.round-273, fill=Frequency)) + large.axes +
	geom_tile() + scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Annual Mean Min Temp (C)") + scale_fill_gradientn(colours=bpy.colors(500), "Frequency") + large.axes + theme(legend.position=c(0.97,0.85), legend.text=element_text(size=14), legend.title=element_text(size=14)) + guides(fill=guide_colorbar(barwidth=2, barheight=5))
dev.off()

####################################################################

param.est <- read.csv("SpeciesModelParameters_Ann.csv")
summary(param.est)

param.est$StdDev <- ((param.est$Upper.SI - param.est$Lower.SI)/3.92)
param.est$MLE.StdDev <- param.est$MLE - param.est$StdDev
summary(param.est)
param.est[1:30,c("Species", "Parameter", "MLE","StdDev", "Lower.SI", "MLE.StdDev")]

unique(param.est$Parameter)

####################################################################
param.est[param.est$Species=="QURU",]

param.distrib <- data.frame(array(dim=c(nrow(param.est),202))) # ncol= num runs + parameter + species
names(param.distrib) <- c("Species", "Parameter", paste("X", 1:200, sep=""))
names(param.distrib)
param.distrib[,c("Species", "Parameter")] <- param.est[,c("Species", "Parameter")]
param.distrib[1:30,1:4]

for(i in unique(param.distrib$Species)){
	for(j in unique(param.distrib$Parameter)){
		temp <- rnorm(200, mean=param.est[param.est$Species==i & param.est$Parameter==j, "MLE"], sd=param.est[param.est$Species==i & param.est$Parameter==j, "StdDev"])

		temp[temp < param.est[param.est$Species==i & param.est$Parameter==j, "Lower.SI"]] <- param.est[param.est$Species==i & param.est$Parameter==j, "Lower.SI"]
		
		temp[temp > param.est[param.est$Species==i & param.est$Parameter==j, "Upper.SI"]] <- param.est[param.est$Species==i & param.est$Parameter==j, "Upper.SI"]

		param.distrib[param.distrib$Species==i & param.distrib$Parameter==j, 3:202] <- temp
	}
}

param.distrib[1:30,1:10]
summary(param.distrib[,1:20])

param.means <- rowMeans(param.distrib[,3:202])
test <- param.est$MLE - param.means
summary(test)








##################################################################################
# Autogenic: Size = Basal Area of tree (cm2); BootStrapped
##################################################################################
x.auto <- seq(0, max(model.data$BA.tree.cm2, na.rm=T), length=250)
summary(x.auto)

##########################
# Some Initial Data Frames
##########################
# Data frame where each run will be placed
aut.temp <- data.frame(array(dim=c(length(x.auto),1)))
row.names(aut.temp) <- x.auto

# Data frame where the summary (mean & SD) of the runs will be placed
aut.response <- data.frame(array(dim=c(length(x.auto),1)))
names(aut.response) <- "x.auto"
# aut.response[,1] <- x.auto

for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		aa.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="aa", 3:202], size=1, replace=T))
		ab.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ab", 3:202], size=1, replace=T))
		aut.temp[,i] <- autogenic.effect(x.auto, aa=aa.samp, ab=ab.samp)
		}

	aut.response[,paste(s, "x.auto", sep=".")] <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)

	aut.response[,paste(s, "MLE", sep=".")] <- autogenic.effect(
			SIZE=aut.response[,paste(s, "x.auto", sep=".")], 
			aa=param.est[param.est$Species==s & param.est$Parameter=="aa", "MLE"], 
			ab=param.est[param.est$Species==s & param.est$Parameter=="ab", "MLE"])
	
	
	aut.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(aut.temp)
	aut.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(aut.temp)
	aut.response[,paste(s, "boot.sd", sep=".")] <- apply(aut.temp, 1, FUN=sd)
	}

summary(aut.response)

################
# Autogenic effect with gmax
aut.response.gmax <- data.frame(array(dim=c(length(x.auto),1)))
names(aut.response.gmax) <- "x.auto"
aut.response.gmax[,1] <- x.auto

for(s in unique(param.distrib$Species)){
	for(i in 1:2000){
		aa.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="aa", 3:202], size=1, replace=T))
		ab.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ab", 3:202], size=1, replace=T))
		gmax.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="gmax", 3:202], size=1, replace=T))
		aut.temp[,i] <- gmax.samp*autogenic.effect(x.auto, aa=aa.samp, ab=ab.samp)
		}

	aut.response.gmax[,paste(s, "x.auto", sep=".")] <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)
	
	aut.response.gmax[,paste(s, "MLE", sep=".")] <- param.est[param.est$Species==s & param.est$Parameter=="gmax", "MLE"] * autogenic.effect(SIZE=aut.response.gmax[,paste(s, "x.auto", sep=".")], aa=param.est[param.est$Species==s & param.est$Parameter=="aa", "MLE"], ab=param.est[param.est$Species==s & param.est$Parameter=="ab", "MLE"])

	aut.response.gmax[,paste(s, "boot.mean", sep=".")] <- rowMeans(aut.temp)
	aut.response.gmax[,paste(s, "boot.sd", sep=".")] <- apply(aut.temp, 1, FUN=sd)
	}

summary(aut.response.gmax)

aut.stack <- stack(aut.response[,substr(names(aut.response),6,11)=="x.auto"])
names(aut.stack) <- c("x.auto", "Species")
aut.stack$Species <- as.factor(substr(aut.stack$Species, 1, 4))
summary(aut.stack)

aut.stack1 <- stack(aut.response[,substr(names(aut.response),6,8)=="MLE"])
summary(aut.stack1)

aut.stack2 <- stack(aut.response[,substr(names(aut.response),6,12)=="boot.sd"])
summary(aut.stack2)

aut.stack3 <- stack(aut.response.gmax[,substr(names(aut.response.gmax),6,8)=="MLE"])
summary(aut.stack3)

aut.stack4 <- stack(aut.response.gmax[,substr(names(aut.response.gmax),6,12)=="boot.sd"])
summary(aut.stack4)

aut.stack$MLE <- aut.stack1[,1]
aut.stack$boot.sd <- aut.stack2[,1]
aut.stack$MLE.gmax <- aut.stack3[,1]
aut.stack$boot.sd.gmax <- aut.stack4[,1]
summary(aut.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Autogenic.pdf")
ggplot(data=aut.stack) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

pdf("Species Comparisons Full Model - Autogenic Gmax.pdf")
ggplot(data=aut.stack) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=MLE.gmax-1.96*boot.sd.gmax, ymax=MLE.gmax+1.96*boot.sd.gmax, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.auto, y=MLE.gmax, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	scale_y_continuous(name="Basal Area Increment (mm2)") +
	theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()




##################################################################################
# Temperature; BootStrapped
##################################################################################
x.temp <- seq(min(model.data$temp.yr), max(model.data$temp.yr), length=250)
summary(x.temp)

##########################
# Some Initial Data Frames
##########################
# Data frame where each run will be placed
temp.temp <- data.frame(array(dim=c(length(x.auto),1)))
row.names(temp.temp) <- x.temp

# Data frame where the summary (mean & SD) of the runs will be placed
temp.response <- data.frame(array(dim=c(length(x.auto),1)))
names(temp.response) <- "x.temp"
temp.response[,1] <- x.temp

for(s in unique(param.distrib$Species)){
	for(i in 1:2000){
		ta1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ta1", 3:202], size=1, replace=T))
		tb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="tb1", 3:202], size=1, replace=T))
		temp.temp[,i] <- temp.effect(x.temp, ta1=ta1.samp, tb1=tb1.samp)
		}

	temp.response[,paste(s, "MLE", sep=".")] <- temp.effect(x.temp, ta1=param.est[param.est$Species==s & param.est$Parameter=="ta1", "MLE"], tb1=param.est[param.est$Species==s & param.est$Parameter=="tb1", "MLE"])

	temp.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(temp.temp)
	temp.response[,paste(s, "boot.sd", sep=".")] <- apply(temp.temp, 1, FUN=sd)
	}

summary(temp.response)

temp.stack <- stack(temp.response[,substr(names(temp.response),6,8)=="MLE"])
names(temp.stack) <- c("MLE", "Species")
temp.stack$Species <- as.factor(substr(temp.stack$Species, 1, 4))
temp.stack$x.temp <- temp.response$x.temp
summary(temp.stack)

temp.stack2 <- stack(temp.response[,substr(names(temp.response),6,12)=="boot.sd"])
summary(temp.stack2)

temp.stack$boot.sd <- temp.stack2[,1]
summary(temp.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Temperature.pdf")
ggplot(data=temp.stack) + large.axes +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Mean Temp (K)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.05)) +
	theme(legend.position=c(0.85,0.15), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()




##################################################################################
# Precipitation; BootStrapped
##################################################################################
x.precip <- seq(min(model.data$ppt.yr), max(model.data$ppt.yr), length=250)
summary(x.precip)

x.flow <- mean(model.data$flow)

##########################
# Some Initial Data Frames
##########################
# Data frame where each run will be placed
precip.temp <- data.frame(array(dim=c(length(x.auto),1)))
row.names(precip.temp) <- x.precip

# Data frame where the summary (mean & SD) of the runs will be placed
precip.response <- data.frame(array(dim=c(length(x.auto),1)))
names(precip.response) <- "x.precip"
precip.response[,1] <- x.precip

for(s in unique(param.distrib$Species)){
	for(i in 1:2000){
		pa1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pa1", 3:202], size=1, replace=T))
		pb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pb1", 3:202], size=1, replace=T))
		pc1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pc1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.samp, pb1=pb1.samp, pc1=pc1.samp)
		}

	precip.response[,paste(s, "MLE", sep=".")] <- precip.effect(x.precip, x.flow, pa1=param.est[param.est$Species==s & param.est$Parameter=="pa1", "MLE"], pb1=param.est[param.est$Species==s & param.est$Parameter=="pb1", "MLE"], pc1=param.est[param.est$Species==s & param.est$Parameter=="pc1", "MLE"])

	precip.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(precip.temp)
	precip.response[,paste(s, "boot.sd", sep=".")] <- apply(precip.temp, 1, FUN=sd)
	}

summary(precip.response)
precip.stack <- stack(precip.response[,substr(names(precip.response),6,8)=="MLE"])
names(precip.stack) <- c("MLE", "Species")
precip.stack$Species <- as.factor(substr(precip.stack$Species, 1, 4))
precip.stack$x.precip <- precip.response$x.precip
summary(precip.stack)

precip.stack2 <- stack(precip.response[,substr(names(precip.response),6,12)=="boot.sd"])
summary(precip.stack2)

precip.stack$boot.sd <- precip.stack2[,1]
summary(precip.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Precipitation.pdf")
ggplot(data=precip.stack) + large.axes +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.15), breaks=c(0,0.25,0.5,0.75,1)) +
	theme(legend.position=c(0.2,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()




##################################################################################
# Competition; BootStrapped
##################################################################################
x.relba <- seq(0, 1, length=250)
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
	for(i in 1:1000){
		for(p in unique(param.est[substr(param.est$Parameter,1,1)=="c","Parameter"])){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
			}

		comp.temp[,i] <- comp.effect(RS=x.relba, BA.PLOT=plot.BA, ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp)
		}

	for(p in unique(param.est[substr(param.est$Parameter,1,1)=="c","Parameter"]))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}

	comp.response[,paste(s, "relba", sep=".")] <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=250)
		
	comp.response[,paste(s, "MLE", sep=".")] <-  comp.effect(
												RS=comp.response[,paste(s, "relba", sep=".")], 
												BA.PLOT=plot.BA, 
												ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
												cd=cd.MLE,
												ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)

	comp.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(comp.temp)
	comp.response[,paste(s, "boot.sd", sep=".")] <- apply(comp.temp, 1, FUN=sd)
	}


################
# Competition effect with gmax
comp.response.gmax <- data.frame(array(dim=c(length(x.relba),1)))
names(comp.response.gmax) <- "x.relba"
comp.response.gmax[,1] <- x.relba

for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est[substr(param.est$Parameter,1,1)=="c","Parameter"])){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
			}

		gmax.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="gmax", 3:202], size=1, replace=T))

		comp.temp[,i] <- gmax.samp*comp.effect(RS=x.relba, BA.PLOT=plot.BA, ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp)
		}

	for(p in unique(param.est[substr(param.est$Parameter,1,1)=="c","Parameter"]))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}

		gmax.MLE <- param.est[param.est$Species==s & param.est$Parameter=="gmax", "MLE"]
		
		comp.response.gmax[,paste(s, "relba", sep=".")] <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=250)

	comp.response.gmax[,paste(s, "MLE", sep=".")] <-  gmax.MLE* comp.effect(
										RS=comp.response.gmax[,paste(s, "relba", sep=".")], 
										BA.PLOT=plot.BA, 
										ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE,
										ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)

	comp.response.gmax[,paste(s, "boot.mean", sep=".")] <- rowMeans(comp.temp)
	comp.response.gmax[,paste(s, "boot.sd", sep=".")] <- apply(comp.temp, 1, FUN=sd)
	}


summary(comp.response.gmax)
summary(comp.response)


comp.stack <- stack(comp.response[,substr(names(comp.response),6,10)=="relba"])
names(comp.stack) <- c("x.relba", "Species")
comp.stack$Species <- as.factor(substr(comp.stack$Species, 1, 4))
summary(comp.stack)


comp.stack1 <- stack(comp.response[,substr(names(comp.response),6,8)=="MLE"])

comp.stack2 <- stack(comp.response[,substr(names(comp.response),6,12)=="boot.sd"])
summary(comp.stack2)

comp.stack3 <- stack(comp.response.gmax[,substr(names(comp.response.gmax),6,8)=="MLE"])
summary(comp.stack3)

comp.stack4 <- stack(comp.response.gmax[,substr(names(comp.response.gmax),6,12)=="boot.sd"])
summary(comp.stack4)

comp.stack$MLE <- comp.stack1[,1]
comp.stack$boot.sd <- comp.stack2[,1]
comp.stack$MLE.gmax <- comp.stack3[,1]
comp.stack$boot.sd.gmax <- comp.stack4[,1]
summary(comp.stack)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Competition.pdf")
ggplot(data=comp.stack) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0, 1.05)) +
	theme(legend.position=c(0.85,0.25), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

pdf("Species Comparisons Full Model - Competition Gmax.pdf")
ggplot(data=comp.stack) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=MLE.gmax-1.96*boot.sd.gmax, ymax=MLE.gmax+1.96*boot.sd.gmax, fill=Species), alpha=0.4) +
	geom_line(aes(x=x.relba, y=MLE.gmax, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Growth (Basal Area Increment, mm2)", limits=c(0,12000)) +
	theme(legend.position=c(0.15,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()







##################################################################################
# Multiplot of all Scalars
##################################################################################
library(kobe)

q.blank2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1)), axis.text.y=element_text(color="black", size=rel(1)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))



# Autogenic
plot.auto <- ggplot(data=aut.stack) + q.blank2 +
	geom_ribbon(aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.auto, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	ggtitle("Size") + theme(plot.title=element_text(face="bold")) + 
	guides(fill=F, color=F)

# Competition
plot.comp <- ggplot(data=comp.stack) + q.blank2 +
	geom_ribbon(aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.relba, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0, 1.05)) +
	ggtitle("Competition") + theme(plot.title=element_text(face="bold")) +
	guides(fill=F, color=F)

# Temperature
plot.temp <- ggplot(data=temp.stack) + q.blank2 +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Mean Temp (K)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.05)) + theme(plot.title=element_text(face="bold")) +
	ggtitle("Temperature") +
	guides(fill=F, color=F)

# Precipitation
plot.precip <- ggplot(data=precip.stack) + q.blank2 +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.15), breaks=c(0,0.25,0.5,0.75,1)) +
	ggtitle("Precipitation") + theme(plot.title=element_text(face="bold")) +
	theme(legend.position=c(0.2,0.3), legend.text=element_text(size=rel(0.8)), legend.title=element_text(size=rel(1))) + labs(fill="Species")



# pdf("Species Comparisons Full Model - All Scalars.pdf")
multiplot(plot.auto, plot.temp, plot.comp,  plot.precip, cols=2)
# dev.off()











##################################################################################
# Full Model of Tree Growth; BootStrapped
##################################################################################
bai.model <- function(SIZE,RS,BA.PLOT,TEMP,PRECIP,FLOW,aa,ab,ca,cb,cc,cd,ce,cf,cg,ta1,tb1,pa1,pb1,pc1,gmax)		
	{ aut <- (1 - aa*exp(-ab*SIZE))  
	  size <- ca*exp(-cb*(RS^cc))
	  plot <- ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg))
	  comp <- exp(-size*(plot^cd))
	  temp <- exp(-0.5*(((TEMP)-ta1)/tb1)^2)
	  precip <- exp(-0.5*(((PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2)
	  
	  gmax*aut*comp*temp*precip
	}

x.temp <- seq(min(model.data$temp.yr), max(model.data$temp.yr), length.out=250)
x.precip <- seq(min(model.data$ppt.yr), max(model.data$ppt.yr), length.out=250)
x.flow <- mean(min(model.data$flow), max(model.data$flow), length.out=250)
x.comp <- seq(0, 1, length.out=250)
x.ba <- seq(0,max(model.data$BA.m2ha.plot.live), length=200)

tree.data <- read.csv("TreeData.csv")
tree.data$BA.tree.cm2 <- tree.data$BA.tree/100
summary(tree.data)

summary(model.data)

species <- as.data.frame(c("ACRU", "BELE", "NYSY", "QUPR", "QURU"))
names(species) <- "Species"
species

for(i in unique(species$Species)){
	species[species$Species==i,"Temp"] <- mean(model.data[model.data$TreeID, "temp.yr"], na.rm=T)
	species[species$Species==i,"Precip"] <- mean(model.data[model.data$TreeID, "ppt.yr"], na.rm=T)
	species[species$Species==i,"Flow"] <- mean(model.data[model.data$TreeID, "flow"], na.rm=T)

	trees.all <-  tree.data[tree.data$Spp==i, "TreeID"]
	trees.dom <- tree.data[tree.data$Spp==i & (tree.data$Canopy=="C" | tree.data$Canopy=="D"), "TreeID"]
	trees.under<- tree.data[tree.data$Spp==i & (tree.data$Canopy=="U" | tree.data$Canopy=="I"), "TreeID"]

	species[species$Species==i,"BA.All"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.tree.cm2"], na.rm=T)
	species[species$Species==i,"RelBA.All"] <- mean(model.data[model.data$TreeID %in% trees.dom, "RelBA"], na.rm=T)
	species[species$Species==i,"PlotBA.All"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.m2ha.plot.live"], na.rm=T)

	species[species$Species==i,"BA.Dom"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.tree.cm2"], na.rm=T)
	species[species$Species==i,"RelBA.Dom"] <- mean(model.data[model.data$TreeID %in% trees.dom, "RelBA"], na.rm=T)
	species[species$Species==i,"PlotBA.Dom"] <- mean(model.data[model.data$TreeID %in% trees.dom, "BA.m2ha.plot.live"], na.rm=T)


	species[species$Species==i,"BA.Under"] <- mean(model.data[model.data$TreeID %in% trees.under, "BA.tree.cm2"], na.rm=T)
	species[species$Species==i,"RelBA.Under"] <- mean(model.data[model.data$TreeID %in% trees.under, "RelBA"], na.rm=T)
	species[species$Species==i,"PlotBA.Under"] <- mean(model.data[model.data$TreeID %in% trees.under, "BA.m2ha.plot.live"], na.rm=T)
}
summary(species)



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
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}

		#############
		full.temp[,i] <- bai.model(SIZE= species[species$Species==s, "BA.All"],
									RS=species[species$Species==s, "RelBA.All"], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=x.temp,
									PRECIP= species[species$Species==s, "Precip"],
									FLOW=species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	

	full.response.all.temp[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.All"],
									RS=species[species$Species==s, "RelBA.All"], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=x.temp,
									PRECIP=species[species$Species==s, "Precip"],
									FLOW=species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.all.temp[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.temp)
	full.response.all.temp[,paste(s, "boot.sd", sep=".")] <- apply(full.temp, 1, FUN=sd)
	}

summary(full.response.all.temp)

full.stack.all.temp <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,8)=="MLE"])
names(full.stack.all.temp) <- c("MLE", "Species")
full.stack.all.temp $Species <- as.factor(substr(full.stack.all.temp$Species, 1, 4))
full.stack.all.temp $x.temp <- full.response.all.temp$x.temp
summary(full.stack.all.temp)

full.stack.all.temp2 <- stack(full.response.all.temp[,substr(names(full.response.all.temp),6,12)=="boot.sd"])
summary(full.stack.all.temp2)

full.stack.all.temp$boot.sd <- full.stack.all.temp2[,1]
summary(full.stack.all.temp)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Temperature All Sizes.pdf")
ggplot(data= full.stack.all.temp) + large.axes +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Mean Temp (K)") + scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.15), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
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
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}

		#############
		full.precip[,i] <- bai.model(SIZE= species[species$Species==s, "BA.All"],
									RS=species[species$Species==s, "RelBA.All"], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP=x.precip,
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	

	full.response.all.precip[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.All"],
									RS=species[species$Species==s, "RelBA.All"], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP= x.precip,
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.all.precip[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.precip)
	full.response.all.precip[,paste(s, "boot.sd", sep=".")] <- apply(full.precip, 1, FUN=sd)
	}

summary(full.response.all.precip)

full.stack.all.precip <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,8)=="MLE"])
names(full.stack.all.precip) <- c("MLE", "Species")
full.stack.all.precip $Species <- as.factor(substr(full.stack.all.precip$Species, 1, 4))
full.stack.all.precip $x.precip <- full.response.all.precip$x.precip
summary(full.stack.all.precip)

full.stack.all.precip2 <- stack(full.response.all.precip[,substr(names(full.response.all.precip),6,12)=="boot.sd"])
summary(full.stack.all.precip2)

full.stack.all.precip$boot.sd <- full.stack.all.precip2[,1]
summary(full.stack.all.precip)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Precipitation All Sizes.pdf")
ggplot(data= full.stack.all.precip) + large.axes +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Total Precip (mm)") + scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()





##################################################################################
# Competition - All Trees
##################################################################################
x.relba <- seq(0, 1, length=250)
summary(x.relba)
plot.BA <- mean(model.data$BA.m2ha.plot.live)


summary(species)

# Data frame where each run will be placed
full.comp <- data.frame(array(dim=c(length(x.relba),1)))
row.names(full.comp) <- x.relba

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.all.comp <- data.frame(array(dim=c(length(x.relba),1)))
names(full.response.all.comp) <- "x.relba"
full.response.all.comp[,1] <- x.relba


for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}
		
		x.relba.temp <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=250)
		#############
		full.comp[,i] <- bai.model(SIZE= species[species$Species==s, "BA.All"],
									RS= x.relba.temp, 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP=species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	
	full.response.all.comp[,paste(s, "relba", sep=".")] <- x.relba.temp

	full.response.all.comp[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.All"],
									RS=full.response.all.comp[,paste(s, "relba", sep=".")], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP= species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.all.comp[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.comp)
	full.response.all.comp[,paste(s, "boot.sd", sep=".")] <- apply(full.comp, 1, FUN=sd)
	}

summary(full.response.all.comp)

full.stack.all.comp <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,10)=="relba"])
names(full.stack.all.comp) <- c("x.relba", "Species")
full.stack.all.comp$Species <- as.factor(substr(full.stack.all.comp$Species, 1, 4))
summary(full.stack.all.comp)

full.stack.all.comp1 <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,8)=="MLE"])
summary(full.stack.all.comp1)

full.stack.all.comp2 <- stack(full.response.all.comp[,substr(names(full.response.all.comp),6,12)=="boot.sd"])
summary(full.stack.all.comp2)

full.stack.all.comp$MLE <- full.stack.all.comp1[,1]
full.stack.all.comp$boot.sd <- full.stack.all.comp2[,1]
summary(full.stack.all.comp)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Competition RelBA All Sizes.pdf")
ggplot(data= full.stack.all.comp) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x= x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.8,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()


##################################################################################
# Autogenic - All Trees
##################################################################################
x.auto <- seq(0, max(model.data$BA.tree.cm2), length=250)
summary(x.auto)
plot.BA <- mean(model.data$BA.m2ha.plot.live)

summary(species)

# Data frame where each run will be placed
full.auto <- data.frame(array(dim=c(length(x.auto),1)))
row.names(full.auto) <- x.auto

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.all.auto <- data.frame(array(dim=c(length(x.auto),1)))
names(full.response.all.auto) <- "x.auto"
full.response.all.auto[,1] <- x.auto


for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}
		
		x.auto.temp <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)
		#############
		full.auto[,i] <- bai.model( SIZE= x.auto.temp,
									RS= species[species$Species==s, "RelBA.All"], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP=species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	

		full.response.all.auto[,paste(s, "BA.tree.cm2", sep=".")] <- x.auto.temp

		full.response.all.auto[,paste(s, "MLE", sep=".")] <- bai.model( 
									SIZE= full.response.all.auto[,paste(s, "BA.tree.cm2", sep=".")],
									RS= species[species$Species==s, "RelBA.All"], 
									BA.PLOT=species[species$Species==s, "PlotBA.All"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP= species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.all.auto[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.auto)
	full.response.all.auto[,paste(s, "boot.sd", sep=".")] <- apply(full.auto, 1, FUN=sd)
	}

summary(full.response.all.auto)

full.stack.all.auto <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,16)=="BA.tree.cm2"])
names(full.stack.all.auto) <- c("x.auto", "Species")
full.stack.all.auto$Species <- as.factor(substr(full.stack.all.auto$Species, 1, 4))
summary(full.stack.all.auto)


full.stack.all.auto1 <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,8)=="MLE"])
summary(full.stack.all.auto1)

full.stack.all.auto2 <- stack(full.response.all.auto[,substr(names(full.response.all.auto),6,12)=="boot.sd"])
summary(full.stack.all.auto2)

full.stack.all.auto$MLE <- full.stack.all.auto1[,1]
full.stack.all.auto$boot.sd <- full.stack.all.auto2[,1]
summary(full.stack.all.auto)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Autogenic All Sizes.pdf")
ggplot(data= full.stack.all.auto) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x= x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(breaks=c(0,2500,5000)) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	theme(legend.position=c(0.2,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()









##################################################################################
# Multiplot of all Scalars
##################################################################################
library(kobe)

q.blank2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1)), axis.text.y=element_text(color="black", size=rel(1)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))



# Autogenic
plot.auto2 <- ggplot(data= full.stack.all.auto) + q.blank2 +
	geom_ribbon(aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x= x.auto, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(breaks=c(0,2500,5000)) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	ggtitle("Size") + theme(plot.title=element_text(face="bold")) + 
	guides(fill=F, color=F)


# Competition
plot.comp2 <- ggplot(data= full.stack.all.comp) + q.blank2 +
	geom_ribbon(aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x= x.relba, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	ggtitle("Competition") + theme(plot.title=element_text(face="bold"))  + 
	theme(legend.position=c(0.8,0.25), legend.text=element_text(size=rel(0.8)), legend.title=element_text(size=rel(1)), legend.key.size=unit(0.8, "lines")) + labs(fill="Species")


# Temperature
plot.temp2 <- ggplot(data= full.stack.all.temp) + q.blank2 +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Mean Temp (K)") + scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	ggtitle("Temperature") + theme(plot.title=element_text(face="bold")) + 
	guides(fill=F, color=F)

# Precipitation
plot.precip2 <- ggplot(data= full.stack.all.precip) + q.blank2 +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Total Precip (mm)") + scale_y_continuous(limits=c(0,1600)) +
	ylab(expression(bold(paste(Basal~Area~Increment~~(mm^2~yr^-1))))) +
	ggtitle("Precipitation") + theme(plot.title=element_text(face="bold")) + 
	guides(fill=F, color=F)



#pdf("Species Comparisons Full Model - All Growth.pdf")
multiplot(plot.auto2, plot.temp2, plot.comp2,  plot.precip2, cols=2)
#dev.off()













##################################################################################
# Temp - Dominant Trees
##################################################################################
# Data frame where each run will be placed
full.temp <- data.frame(array(dim=c(length(x.temp),1)))
row.names(full.temp) <- x.temp

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.dom.temp <- data.frame(array(dim=c(length(x.temp),1)))
names(full.response.dom.temp) <- "x.temp"
full.response.dom.temp[,1] <- x.temp


for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}

		#############
		full.temp[,i] <- bai.model(SIZE= species[species$Species==s, "BA.Dom"],
									RS=species[species$Species==s, "RelBA.Dom"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Dom"],
									TEMP=x.temp,
									PRECIP= species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	

	full.response.dom.temp[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.Dom"],
									RS=species[species$Species==s, "RelBA.Dom"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Dom"],
									TEMP=x.temp,
									PRECIP= species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.dom.temp[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.temp)
	full.response.dom.temp[,paste(s, "boot.sd", sep=".")] <- apply(full.temp, 1, FUN=sd)
	}

summary(full.response.dom.temp)

full.stack.dom.temp <- stack(full.response.dom.temp[,substr(names(full.response.dom.temp),6,8)=="MLE"])
names(full.stack.dom.temp) <- c("MLE", "Species")
full.stack.dom.temp $Species <- as.factor(substr(full.stack.dom.temp$Species, 1, 4))
full.stack.dom.temp $x.temp <- full.response.dom.temp$x.temp
summary(full.stack.dom.temp)

full.stack.dom.temp2 <- stack(full.response.dom.temp[,substr(names(full.response.dom.temp),6,12)=="boot.sd"])
summary(full.stack.dom.temp2)

full.stack.dom.temp$boot.sd <- full.stack.dom.temp2[,1]
summary(full.stack.dom.temp)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Temperature Dominant.pdf")
ggplot(data= full.stack.dom.temp) + large.axes +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Min Temp (K)") + scale_y_continuous(name="Basal Area Increment (mm2)", limits=c(0,1600)) +
	theme(legend.position=c(0.8,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()



##################################################################################
# Precip - Dominant Trees
##################################################################################
# Data frame where each run will be placed
full.precip <- data.frame(array(dim=c(length(x.precip),1)))
row.names(full.precip) <- x.precip

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.dom.precip <- data.frame(array(dim=c(length(x.precip),1)))
names(full.response.dom.precip) <- "x.precip"
full.response.dom.precip[,1] <- x.precip

for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}

		#############
		full.precip[,i] <- bai.model(SIZE= species[species$Species==s, "BA.Dom"],
									RS=species[species$Species==s, "RelBA.Dom"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Dom"],
									TEMP=x.temp,
									PRECIP=x.precip,
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	

	full.response.dom.precip[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.Dom"],
									RS=species[species$Species==s, "RelBA.Dom"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Dom"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP= x.precip,
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.dom.precip[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.precip)
	full.response.dom.precip[,paste(s, "boot.sd", sep=".")] <- apply(full.precip, 1, FUN=sd)
	}

summary(full.response.dom.precip)

full.stack.dom.precip <- stack(full.response.dom.precip[,substr(names(full.response.dom.precip),6,8)=="MLE"])
names(full.stack.dom.precip) <- c("MLE", "Species")
full.stack.dom.precip $Species <- as.factor(substr(full.stack.dom.precip$Species, 1, 4))
full.stack.dom.precip $x.precip <- full.response.dom.precip$x.precip
summary(full.stack.dom.precip)

full.stack.dom.precip2 <- stack(full.response.dom.precip[,substr(names(full.response.dom.precip),6,12)=="boot.sd"])
summary(full.stack.dom.precip2)

full.stack.dom.precip$boot.sd <- full.stack.dom.precip2[,1]
summary(full.stack.dom.precip)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Precipitation Dominant.pdf")
ggplot(data= full.stack.dom.precip) + large.axes +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Basal Area Increment (mm2)", limits=c(0,1600)) +
	theme(legend.position=c(0.2,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()







##################################################################################
# Temp - Sub-Dominant Trees
##################################################################################
# Data frame where each run will be placed
full.temp <- data.frame(array(dim=c(length(x.temp),1)))
row.names(full.temp) <- x.temp

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.under.temp <- data.frame(array(dim=c(length(x.temp),1)))
names(full.response.under.temp) <- "x.temp"
full.response.under.temp[,1] <- x.temp


for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}

		#############
		full.temp[,i] <- bai.model(SIZE= species[species$Species==s, "BA.Under"],
									RS=species[species$Species==s, "RelBA.Under"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Under"],
									TEMP=x.temp,
									PRECIP= species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}
		
	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}

	full.response.under.temp[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.Under"],
									RS=species[species$Species==s, "RelBA.Under"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Under"],
									TEMP=x.temp,
									PRECIP= species[species$Species==s, "Precip"],
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.under.temp[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.temp)
	full.response.under.temp[,paste(s, "boot.sd", sep=".")] <- apply(full.temp, 1, FUN=sd)
	}

summary(full.response.under.temp)

full.stack.under.temp <- stack(full.response.under.temp[,substr(names(full.response.under.temp),6,8)=="MLE"])
names(full.stack.under.temp) <- c("MLE", "Species")
full.stack.under.temp $Species <- as.factor(substr(full.stack.under.temp$Species, 1, 4))
full.stack.under.temp $x.temp <- full.response.under.temp$x.temp
summary(full.stack.under.temp)

full.stack.under.temp2 <- stack(full.response.under.temp[,substr(names(full.response.under.temp),6,12)=="boot.sd"])
summary(full.stack.under.temp2)

full.stack.under.temp$boot.sd <- full.stack.under.temp2[,1]
summary(full.stack.under.temp)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Temperature Understory.pdf")
ggplot(data=full.stack.under.temp) + large.axes +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Min Temp (K)") + scale_y_continuous(name="Basal Area Increment (mm2)", limits=c(0,1600)) +
	theme(legend.position=c(0.2,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

pdf("Species Comparisons Full Model - Full Temperature Understory2.pdf")
ggplot(data=full.stack.under.temp) + large.axes +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Min Temp (K)") + scale_y_continuous(name="Basal Area Increment (mm2)") +
	theme(legend.position=c(0.13,0.875), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()






##################################################################################
# Precip - Sub-Dominant Trees
##################################################################################
# Data frame where each run will be placed
full.precip <- data.frame(array(dim=c(length(x.precip),1)))
row.names(full.precip) <- x.precip

# Data frame where the summary (mean & SD) of the runs will be placed
full.response.under.precip <- data.frame(array(dim=c(length(x.precip),1)))
names(full.response.under.precip) <- "x.precip"
full.response.under.precip[,1] <- x.precip

for(s in unique(param.distrib$Species)){
	for(i in 1:1000){
		for(p in unique(param.est$Parameter)){
			assign(paste(p, "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p, 3:202], size=1, replace=T)))
		}

		#############
		full.precip[,i] <- bai.model(SIZE= species[species$Species==s, "BA.Under"],
									RS=species[species$Species==s, "RelBA.Under"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Under"],
									TEMP=x.temp,
									PRECIP=x.precip,
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.samp, ab=ab.samp,
									ca=ca.samp, cb=cb.samp, cc=cc.samp, cd=cd.samp, ce=ce.samp, cf=cf.samp, cg=cg.samp,
									ta1=ta1.samp,
									tb1=tb1.samp,
									pa1=pa1.samp,
									pb1=pb1.samp,
									pc1=pc1.samp,
									gmax=gmax.samp
									)
		}

	for(p in unique(param.est$Parameter))
		{	assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}
	

	full.response.under.precip[,paste(s, "MLE", sep=".")] <- bai.model(SIZE= species[species$Species==s, "BA.Under"],
									RS=species[species$Species==s, "RelBA.Under"], 
									BA.PLOT=species[species$Species==s, "PlotBA.Under"],
									TEMP=species[species$Species==s, "Temp"],
									PRECIP= x.precip,
									FLOW= species[species$Species==s, "Flow"],
									aa=aa.MLE, ab=ab.MLE,
									ca=ca.MLE, cb=cb.MLE, cc=cc.MLE, cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE,
									ta1=ta1.MLE,
									tb1=tb1.MLE,
									pa1=pa1.MLE,
									pb1=pb1.MLE,
									pc1=pc1.MLE,
									gmax=gmax.MLE)
	
	full.response.under.precip[,paste(s, "boot.mean", sep=".")] <- rowMeans(full.precip)
	full.response.under.precip[,paste(s, "boot.sd", sep=".")] <- apply(full.precip, 1, FUN=sd)
	}

summary(full.response.under.precip)

full.stack.under.precip <- stack(full.response.under.precip[,substr(names(full.response.under.precip),6,8)=="MLE"])
names(full.stack.under.precip) <- c("MLE", "Species")
full.stack.under.precip $Species <- as.factor(substr(full.stack.under.precip$Species, 1, 4))
full.stack.under.precip $x.precip <- full.response.under.precip$x.precip
summary(full.stack.under.precip)

full.stack.under.precip2 <- stack(full.response.under.precip[,substr(names(full.response.under.precip),6,12)=="boot.sd"])
summary(full.stack.under.precip2)

full.stack.under.precip$boot.sd <- full.stack.under.precip2[,1]
summary(full.stack.under.precip)

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model - Full Precipitation Understory.pdf")
ggplot(data= full.stack.under.precip) + large.axes +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Basal Area Increment (mm2)", limits=c(0,1600)) +
	theme(legend.position=c(0.8,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

pdf("Species Comparisons Full Model - Full Precipitation Understory2.pdf")
ggplot(data= full.stack.under.precip) + large.axes +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Annual Precip (mm)") + scale_y_continuous(name="Basal Area Increment (mm2)") +
	theme(legend.position=c(0.9,0.75), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

