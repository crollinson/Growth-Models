####################################################################
# Comparing Niches from full model for different species
# Climate is from year of BAI measurement (mean Tmin, total Precip)
####################################################################
# rm(list=ls())
load("Species_Comparisons_FullModel_Seasonal.Rdata")

library(likelihood)
library(ggplot2)
library(raster)
library(grid)
library(car)

#######################
# # Subsetting data
# all.data <- read.csv("CARCA_CoreData_Tree_Plot_Tavg_1km_Precip_WideFormat.csv")
# all.data$BA.tree.cm2 <- all.data$BA.tree/100
# summary(all.data)

# species <- c("QURU", "QUPR", "NYSY", "BELE", "ACRU")

# model.data <- all.data[all.data$Spp %in% species & all.data$Year>=1990 & all.data$Year<=2011,]
# model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]+273.15
# model.data <- model.data[complete.cases(model.data),]
# summary(model.data)
# write.csv(model.data, "TargetSpecies_AllSites_1990-2011.csv", row.names=F)
#######################

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
       { t(exp(-0.5*((t(TEMP)-ta1)/tb1)^2))  }

precip.effect <- function(PRECIP,FLOW,pa1,pb1,pc1)
       { t(exp(-0.5*((t(PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2)) }


bai.model <- function(SIZE,RS,BA.PLOT,TEMP,PRECIP,FLOW,aa,ab,ca,cb,cc,cd,ce,cf,cg,ta1,tb1,pa1,pb1,pc1,gmax)		
	{ aut <- (1 - aa*exp(-ab*SIZE))  
	  size <- ca*exp(-cb*(RS^cc))
	  plot <- ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg))
	  comp <- exp(-size*(plot^cd))
	  temp <- t(exp(-0.5*((t(TEMP)-ta1)/tb1)^2))
	  precip <- t(exp(-0.5*((t(PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2))
	  
	  gmax*aut*comp*temp*precip
	}

####################################################################
# model.data <- read.csv("TargetSpecies_AllSites_1990-2011.csv")
# # model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]+273.15
# # model.data <- model.data[complete.cases(model.data) & model.data$Year>=1990 & model.data$Year<=2011,]
# # model.data$BA.tree.cm2 <- model.data$BA.tree/100
# summary(model.data)
# length(unique(model.data$TreeID))


# #############
# # Vector with names of Months of the year
# months <- c("X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09", "X10", "X11", "X12")

# # Selecting which previous and current year months to include in model
# months.prev <- paste(months[6:12], "prev", sep=".") # Previous June through December
# months.curr <- paste(months[1:10], sep=".") # Current Junuary through October
# months.use <- c(months.prev, months.curr)

# seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
# seasons

# model.data$Tavg.pX06.pX08 <- rowMeans(model.data[,c("Tavg.X06.prev", "Tavg.X07.prev", "Tavg.X08.prev")])
# model.data$Tavg.pX09.pX11 <- rowMeans(model.data[,c("Tavg.X09.prev", "Tavg.X10.prev", "Tavg.X11.prev")])
# model.data$Tavg.pX12.X02 <- rowMeans(model.data[,c("Tavg.X12.prev", "Tavg.X01", "Tavg.X02")])
# model.data$Tavg.X03.X05 <- rowMeans(model.data[,c("Tavg.X03", "Tavg.X04", "Tavg.X05")])
# model.data$Tavg.X06.X08 <- rowMeans(model.data[,c("Tavg.X06", "Tavg.X07", "Tavg.X08")])
# model.data$Tavg.X09.X11 <- rowMeans(model.data[,c("Tavg.X09", "Tavg.X10", "Tavg.X11")])

# model.data$Precip.pX06.pX08 <- rowMeans(model.data[,c("Precip.X06.prev", "Precip.X07.prev", "Precip.X08.prev")])
# model.data$Precip.pX09.pX11 <- rowMeans(model.data[,c("Precip.X09.prev", "Precip.X10.prev", "Precip.X11.prev")])
# model.data$Precip.pX12.X02 <- rowMeans(model.data[,c("Precip.X12.prev", "Precip.X01", "Precip.X02")])
# model.data$Precip.X03.X05 <- rowMeans(model.data[,c("Precip.X03", "Precip.X04", "Precip.X05")])
# model.data$Precip.X06.X08 <- rowMeans(model.data[,c("Precip.X06", "Precip.X07", "Precip.X08")])
# model.data$Precip.X09.X11 <- rowMeans(model.data[,c("Precip.X09", "Precip.X10", "Precip.X11")])


# # Making vectors with the column names of the temp & precip months of interest
# temp.col <- paste("Tavg", seasons, sep=".")
# precip.col <- paste("Precip", seasons, sep=".")

# # Column numbers of temp & precip months of interest
# temp.col.ind <- which(names(model.data) %in% c(temp.col))
# precip.col.ind <- which(names(model.data) %in% c(precip.col))
summary(model.data[ ,temp.col.ind])
summary(model.data[ ,precip.col.ind])
length(temp.col.ind)

min(model.data[ ,temp.col.ind])-273.15; max(model.data[ ,temp.col.ind])-273.15
min(model.data[ ,precip.col.ind]); max(model.data[ ,precip.col.ind])

####################################################################

# # param.est <- read.csv("SpeciesModelParameters_Season.csv")
# summary(param.est)

# param.est$StdDev <- (abs(param.est$Upper.SI - param.est$Lower.SI)/3.92)
# param.est$MLE.StdDev <- param.est$MLE - param.est$StdDev
summary(param.est)
param.est[1:30,c("Species", "Parameter", "MLE","StdDev", "Lower.SI", "MLE.StdDev")]

unique(param.est$Parameter)

####################################################################
# param.est[param.est$Species=="QURU",]

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

pdf("Species Comparisons Full Model Seasonal - Autogenic.pdf")
ggplot(data=aut.stack) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.auto, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Tree Basal Area (cm2)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

pdf("Species Comparisons Full Model Seasonal - Autogenic Gmax.pdf")
ggplot(data=aut.stack) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=MLE.gmax-1.96*boot.sd.gmax, ymax=MLE.gmax+1.96*boot.sd.gmax, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.auto, y=MLE.gmax, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Tree Basal Area (cm2)") + scale_y_continuous(name="Basal Area Increment (mm2)") +
	theme(legend.position=c(0.85,0.2), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
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

pdf("Species Comparisons Full Model Seasonal - Competition.pdf")
ggplot(data=comp.stack) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.relba, y=MLE, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0, 1.05)) +
	theme(legend.position=c(0.85,0.25), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

pdf("Species Comparisons Full Model Seasonal - Competition Gmax.pdf")
ggplot(data=comp.stack) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=MLE.gmax-1.96*boot.sd.gmax, ymax=MLE.gmax+1.96*boot.sd.gmax, fill=Species), alpha=0.4) +
	geom_line(aes(x=x.relba, y=MLE.gmax, color=Species), size=2) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + scale_y_continuous(name="Growth (Basal Area Increment, mm2)", limits=c(0,12000)) +
	theme(legend.position=c(0.15,0.8), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()






##################################################################################
# Temperature; BootStrapped
##################################################################################
seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
seasons

x.temp <- seq(min(model.data[,temp.col.ind]), max(model.data[,temp.col.ind]), length=250)

summary(param.est)
unique(param.est$Parameter)
##########################
# Some Initial Data Frames
##########################
# Vector needs to combine which season and x values
x.names <- vector(length=length(seasons)*length(x.temp))
for(i in 1:length(seasons)){
	start <- i*length(x.temp)-length(x.temp)+1
	x.names[start:(start+length(x.temp)-1)] <- paste(i, x.temp, sep=".")	
}

# Data frame where each run will be placed
temp.temp <- data.frame(array(dim=c(length(x.names),1)))
row.names(temp.temp) <- x.names

# Data frame where the summary (mean & SD) of the runs will be placed
# temp.response <- data.frame(array(dim=c(length(x.temp),1)))
# names(temp.response) <- "x.temp"
# temp.response[,1] <- x.temp
temp.response <- data.frame(array(dim=c(length(x.names),1)))
row.names(temp.response) <- x.names
names(temp.response) <- "x.temp"
temp.response[,1] <- x.temp
temp.response$Season <- as.factor(substr(row.names(temp.response),1,1))
summary(temp.response)


for(s in unique(param.distrib$Species)){
	for(j in 1:length(seasons)){
		start <- j*length(x.temp)-length(x.temp)+1

		p.list1 <- c(paste("ta1", j, sep=""), paste("tb1", j, sep=""))
		p.list2 <- c("ta1", "tb1")
		for(i in 1:1000){
			for(p in 1:length(p.list1)){
				assign(paste(p.list2[p], "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[p], 3:202], size=1, replace=T)))
				}

	temp.temp[start:(start+length(x.temp)-1),i] <- temp.effect(x.temp, ta1=ta1.samp, tb1=tb1.samp)
			}

		for(p in 1:length(p.list1)){
			assign(paste(p.list2[p], "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p.list1[p], "MLE"])
			}

		# Note: for some reason as.numeric is necessary otherwise it gets put in a weird format
		temp.response[start:(start+length(x.temp)-1),paste(s, "temp", sep=".")] <- x.temp
		
		# seq(min(model.data[model.data$Spp==s,temp.col.ind[j]]), max(model.data[model.data$Spp==s,temp.col.ind[j]]), length=250)

		temp.response[start:(start+length(x.temp)-1),paste(s, "MLE", sep=".")] <- as.numeric(temp.effect(TEMP=temp.response[start:(start+length(x.temp)-1),paste(s, "temp", sep=".")], ta1=ta1.MLE, tb1=tb1.MLE))
		}
	temp.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(temp.temp)
	temp.response[,paste(s, "boot.sd", sep=".")] <- apply(temp.temp, 1, FUN=sd)
	}

summary(temp.response)
head(temp.response)
names(temp.response)

temp.stack <- stack(temp.response[,substr(names(temp.response),6,9)=="temp"])
names(temp.stack) <- c("x.temp", "Species")
temp.stack$Species <- as.factor(substr(temp.stack$Species, 1, 4))
temp.stack$season.code <- temp.response$Season
temp.stack$Year <- as.factor(ifelse(temp.stack$season.code==1 | temp.stack$season.code==2,"-1","1"))
levels(temp.stack$Year) <- c("Prior", "Current")
temp.stack$Season <- recode(temp.stack$season.code, "'1'='3'; '2'='4'; '3'='1'; '4'='2'; '5'='3'; '6'='4'")
levels(temp.stack$Season) <- c("Winter", "Spring", "Summer", "Fall")
summary(temp.stack) 

temp.stack1 <- stack(temp.response[,substr(names(temp.response),6,8)=="MLE"])
summary(temp.stack1)

temp.stack2 <- stack(temp.response[,substr(names(temp.response),6,12)=="boot.sd"])
summary(temp.stack2)

temp.stack$MLE <- temp.stack1[,1]
temp.stack$boot.sd <- temp.stack2[,1]
summary(temp.stack)


# adding seasonal max/min to the data set (to do vertical lines for what's observed)
for(i in 1:length(seasons)){
	temp.stack[temp.stack$season.code==i,"temp.min"] <- min(model.data[,temp.col.ind[i]])
	temp.stack[temp.stack$season.code==i,"temp.max"] <- max(model.data[,temp.col.ind[i]])
}
summary(temp.stack)

# Some summary stats
1-min(temp.stack[temp.stack$Species=="BELE" & temp.stack$Season=="Summer" & temp.stack$Year=="Prior" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/max(temp.stack[temp.stack$Species=="BELE" & temp.stack$Season=="Summer" & temp.stack$Year=="Prior" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

max(temp.stack[temp.stack$Species=="QURU" & temp.stack$Season=="Summer" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/min(temp.stack[temp.stack$Species=="QURU" & temp.stack$Season=="Summer" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

max(temp.stack[temp.stack$Species=="NYSY" & temp.stack$Season=="Spring" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/min(temp.stack[temp.stack$Species=="NYSY" & temp.stack$Season=="Spring" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

1-min(temp.stack[temp.stack$Species=="ACRU" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/max(temp.stack[temp.stack$Species=="ACRU" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

max(temp.stack[temp.stack$Species=="QUPR" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/min(temp.stack[temp.stack$Species=="QUPR" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model Seasonal - Temperature.pdf", width=10, height=7.5)
ggplot(data=temp.stack) + large.axes + facet_grid(Year~Season) +
	geom_ribbon(aes(x=x.temp, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp, y=MLE, color=Species), size=1.25) +
	geom_vline(aes(xintercept=temp.min), linetype="dashed", color="black") + 
	geom_vline(aes(xintercept=temp.max), linetype="dashed", color="black") + 
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Mean Season temp (K)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	theme(legend.position=c(0.12,0.8), legend.text=element_text(size=14), legend.title=element_text(size=rel(1.5))) + labs(fill="Species") + theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()




##################################################################################
# Precipitation; BootStrapped
##################################################################################
seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
seasons

x.precip <- seq(min(model.data[,precip.col.ind]), max(model.data[,precip.col.ind]), length=250)
x.flow <- mean(model.data$flow)

summary(param.est)
unique(param.est$Parameter)
##########################
# Some Initial Data Frames
##########################
# Vector needs to combine which season and x values
x.names <- vector(length=length(seasons)*length(x.precip))
for(i in 1:length(seasons)){
	start <- i*length(x.precip)-length(x.precip)+1
	x.names[start:(start+length(x.precip)-1)] <- paste(i, x.precip, sep=".")	
}

# Data frame where each run will be placed
precip.temp <- data.frame(array(dim=c(length(x.names),1)))
row.names(precip.temp) <- x.names

# Data frame where the summary (mean & SD) of the runs will be placed
# precip.response <- data.frame(array(dim=c(length(x.precip),1)))
# names(precip.response) <- "x.precip"
# precip.response[,1] <- x.precip
precip.response <- data.frame(array(dim=c(length(x.names),1)))
row.names(precip.response) <- x.names
names(precip.response) <- "x.precip"
precip.response[,1] <- x.precip
precip.response$Season <- as.factor(substr(row.names(precip.response),1,1))
summary(precip.response)


for(s in unique(param.distrib$Species)){
	for(j in 1:length(seasons)){
		start <- j*length(x.precip)-length(x.precip)+1

		p.list1 <- c(paste("pa1", j, sep=""), paste("pb1", j, sep=""), paste("pc1"))
		p.list2 <- c("pa1", "pb1", "pc1")
		for(i in 1:1000){
			for(p in 1:length(p.list1)){
				assign(paste(p.list2[p], "samp", sep="."), as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[p], 3:202], size=1, replace=T)))
				}

	precip.temp[start:(start+length(x.precip)-1),i] <- precip.effect(x.precip, x.flow, pa1=pa1.samp, pb1=pb1.samp, pc1=pc1.samp)
			}

		for(p in 1:length(p.list1)){
			assign(paste(p.list2[p], "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p.list1[p], "MLE"])
			}

		precip.response[start:(start+length(x.precip)-1),paste(s, "precip", sep=".")] <- x.precip


		# Note: for some reason as.numeric is necessary otherwise it gets put in a weird format
		precip.response[start:(start+length(x.precip)-1),paste(s, "MLE", sep=".")] <- as.numeric(precip.effect(PRECIP=precip.response[start:(start+length(x.precip)-1),paste(s, "precip", sep=".")], FLOW=x.flow, pa1=pa1.MLE, pb1=pb1.MLE, pc1=pc1.MLE))
		}
	precip.response[,paste(s, "boot.mean", sep=".")] <- rowMeans(precip.temp)
	precip.response[,paste(s, "boot.sd", sep=".")] <- apply(precip.temp, 1, FUN=sd)
	}

summary(precip.response)
head(precip.response)
names(precip.response)

precip.stack <- stack(precip.response[,substr(names(precip.response),6,11)=="precip"])
names(precip.stack) <- c("x.precip", "Species")
precip.stack$Species <- as.factor(substr(precip.stack$Species, 1, 4))
precip.stack$x.precip <- precip.response$x.precip
precip.stack$season.code <- precip.response$Season
precip.stack$Year <- as.factor(ifelse(precip.stack$season.code==1 | precip.stack$season.code==2,"-1","1"))
levels(precip.stack$Year) <- c("Prior", "Current")
precip.stack$Season <- recode(precip.stack$season.code, "'1'='3'; '2'='4'; '3'='1'; '4'='2'; '5'='3'; '6'='4'")
levels(precip.stack$Season) <- c("Winter", "Spring", "Summer", "Fall")
summary(precip.stack) 

summary(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=min(model.data[,precip.col.ind]) & precip.stack$x.precip<=max(model.data[,precip.col.ind]),]) 

summary(precip.stack[precip.stack$Species=="QUPR" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=min(model.data[,precip.col.ind]) & precip.stack$x.precip<=max(model.data[,precip.col.ind]),]) 

precip.stack1 <- stack(precip.response[,substr(names(precip.response),6,8)=="MLE"])

precip.stack2 <- stack(precip.response[,substr(names(precip.response),6,12)=="boot.sd"])
summary(precip.stack2)

precip.stack$MLE <- precip.stack1[,1]
precip.stack$boot.sd <- precip.stack2[,1]
summary(precip.stack)

for(i in 1:length(seasons)){
	precip.stack[precip.stack$season.code==i,"precip.min"] <- min(model.data[,precip.col.ind[i]])
	precip.stack[precip.stack$season.code==i,"precip.max"] <- max(model.data[,precip.col.ind[i]])
}
summary(precip.stack)

# Quantifying changes in growth
1-min(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

1-min(precip.stack[precip.stack$Species=="BELE" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="BELE" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

max(precip.stack[precip.stack$Species=="QURU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/min(precip.stack[precip.stack$Species=="QURU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

1-min(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

1-min(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

1-min(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

1-min(precip.stack[precip.stack$Species=="QUPR" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="QUPR" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])


species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Species Comparisons Full Model Seasonal - Precipitation.pdf", width=10, height=7.5)
ggplot(data=precip.stack) + large.axes + facet_grid(Year~Season) +
	geom_ribbon(aes(x=x.precip, ymin=MLE-1.96*boot.sd, ymax=MLE+1.96*boot.sd, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE, color=Species), size=1.25) +
	geom_vline(aes(xintercept=precip.min), linetype="dashed", color="black") + 
	geom_vline(aes(xintercept=precip.max), linetype="dashed", color="black") + 
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Total Season Precip (mm)") + scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1.2), breaks=seq(0, 1, 0.25)) +
	theme(legend.position=c(0.12,0.8), legend.text=element_text(size=14), legend.title=element_text(size=rel(1.5))) + labs(fill="Species") + theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()
