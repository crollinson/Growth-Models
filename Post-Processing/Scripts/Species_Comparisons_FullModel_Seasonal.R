####################################################################
# Comparing Niches from full model for different species
# Climate is from year of BAI measurement (mean Tmin, total Precip)
####################################################################
# rm(list=ls())
# load("Species_Comparisons_FullModel_Seasonal.Rdata")
setwd("..")
source("Scripts/GrowthEffect_Functions.R")
source("Scripts/GrowthModel_Functions.R")

library(likelihood)
library(ggplot2)
library(raster)
library(grid)
library(car)

#######################
# # Subsetting data
# all.data <- read.csv("../RawInputs/CARCA_CoreData_Climate_Month_Wide.csv")
# all.data$BA.tree.cm2 <- all.data$BA.tree/100
# summary(all.data)

# species <- c("QURU", "QUPR", "NYSY", "BELE", "ACRU")

# model.data <- all.data[all.data$Spp %in% species & all.data$Year>=1990 & all.data$Year<=2011,]
# model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]+273.15
# model.data <- model.data[complete.cases(model.data),]
# summary(model.data)
# write.csv(model.data, "Post-Processing/Inputs/TargetSpecies_AllSites_1990-2011.csv", row.names=F)
#######################

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-0.5),  axis.title.y=element_text(face="bold", size=20, vjust=0.5), plot.margin=unit(c(0.1,0.1,0.5,0.1), "lines"))

####################################################################
dat.acru <- read.csv("../Seasonal/Inputs/ACRU_AllSites_1990-2011.csv")
dat.bele <- read.csv("../Seasonal/Inputs/BELE_AllSites_1990-2011.csv")
dat.nysy <- read.csv("../Seasonal/Inputs/NYSY_AllSites_1990-2011.csv")
dat.qupr <- read.csv("../Seasonal/Inputs/QUPR_AllSites_1990-2011.csv")
dat.quru <- read.csv("../Seasonal/Inputs/QURU_AllSites_1990-2011.csv")

model.data <- rbind(dat.acru, dat.bele, dat.nysy, dat.qupr, dat.quru)

# model.data <- read.csv("Inputs/TargetSpecies_AllSites_Seasonal_1990-2011.csv")


# model.data <- read.csv("Inputs/TargetSpecies_AllSites_Seasonal_1990-2011.csv")
# model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]+273.15
# model.data <- model.data[complete.cases(model.data) & model.data$Year>=1990 & model.data$Year<=2011,]
# model.data$BA.tree.cm2 <- model.data$BA.tree/100
summary(model.data)
length(unique(model.data$TreeID))

# Making vectors with the column names of the temp & precip months of interest
temp.col <- paste("Tavg", seasons, sep=".")
precip.col <- paste("Precip", seasons, sep=".")

# Column numbers of temp & precip months of interest
temp.col.ind <- which(names(model.data) %in% c(temp.col))
precip.col.ind <- which(names(model.data) %in% c(precip.col))


### ###########
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


# Making vectors with the column names of the temp & precip months of interest
temp.col <- paste("Tavg", seasons, sep=".")
precip.col <- paste("Precip", seasons, sep=".")

# Column numbers of temp & precip months of interest
temp.col.ind <- which(names(model.data) %in% c(temp.col))
precip.col.ind <- which(names(model.data) %in% c(precip.col))
# summary(model.data[ ,temp.col.ind])
# summary(model.data[ ,precip.col.ind])
# length(temp.col.ind)

# min(model.data[ ,temp.col.ind])-273.15; max(model.data[ ,temp.col.ind])-273.15
# min(model.data[ ,precip.col.ind]); max(model.data[ ,precip.col.ind])

# ####################################################################



# ###################################################################
# # Making a csv with parameter estimates
# ###################################################################
# load("../Seasonal/Outputs/ACRU_Full_Season_Results.Rdata")
# acru.results <- results
# load("../Seasonal/Outputs/BELE_Full_Season_Results.Rdata")
# bele.results <- results
# load("../Seasonal/Outputs/NYSY_Full_Season_Results.Rdata")
# nysy.results <- results
# load("../Seasonal/Outputs/QUPR_Full_Season_Results.Rdata")
# qupr.results <- results
# load("../Seasonal/Outputs/QURU_Full_Season_Results.Rdata")
# quru.results <- results

# # #-----------------------------------
# # Looking at autocorrelation quickly
# #-----------------------------------
# # Calculating residuals
# acru.results$source_data$resid <- acru.results$source_data$BAI - acru.results$source_data$predicted
# bele.results$source_data$resid <- bele.results$source_data$BAI - bele.results$source_data$predicted
# nysy.results$source_data$resid <- nysy.results$source_data$BAI - nysy.results$source_data$predicted
# qupr.results$source_data$resid <- qupr.results$source_data$BAI - qupr.results$source_data$predicted
# quru.results$source_data$resid <- quru.results$source_data$BAI - quru.results$source_data$predicted

# # Graphing the Autocorrelation
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
# write.csv(param.est, "Inputs/SpeciesModelParameters_Season.csv", row.names=F)

####################################################################
param.est <- read.csv("Inputs/SpeciesModelParameters_Season.csv")
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
		p.range <- seq(from=param.est[param.est$Species==s & param.est$Parameter==j, "Lower.Lim"], to=param.est[param.est$Species==s & param.est$Parameter==j, "Upper.Lim"], length.out=500)

		temp <- sample(p.range, n)
		param.distrib[param.distrib$Species==s & param.distrib$Parameter==j, 3:(3+n-1)] <- temp
	}
}

param.distrib[1:30,1:10]
summary(param.distrib[,1:20])

param.means <- rowMeans(param.distrib[,3:202])





##################################################################################
# Autogenic: Size = Basal Area of tree (cm2); BootStrapped
##################################################################################
n=250
x.auto <- seq(0, max(model.data$BA.tree.cm2, na.rm=T), length=n)
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
	x.auto.spp <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)
	aut.response <- aut.predict(s, x.auto.spp, aut.response, param.est, param.distrib, n=n)
	}

summary(aut.response)
# ---------------------------------------------------------
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

pdf("Figures/Species Comparisons Full Model Seasonal - Autogenic.pdf")
ggplot(data=aut.stack) + large.axes +
	geom_ribbon(aes(x=x.auto, ymin=Min, ymax=Max, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.auto, y=MLE, color=Species, linetype=Species), size=1) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	xlab(expression(bold(paste(Basal~Area~~(cm^2))))) +
	scale_y_continuous(name="Percent Max Growth Rate", limits=c(0,1), breaks=seq(0, 1, 0.25)) +
	theme(legend.position=c(0.85,0.30), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()

##################################################################################
# Competition; BootStrapped
##################################################################################
n=250
x.relba <- seq(0, 1, length=n)
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
	x.relba <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=n)
	plot.ba <- mean(model.data[model.data$Spp==s, "BA.m2ha"])
	comp.response <- comp.predict(s, x.relba, plot.ba, comp.response, param.est, param.distrib, n=n)
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

pdf("Figures/Species Comparisons Full Model Seasonal - Competition.pdf")
ggplot(data=comp.stack) + large.axes +
	geom_ribbon(aes(x=x.relba, ymin=Min, ymax=Max, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.relba, y=MLE, color=Species, linetype=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Relative Basal Area") + 
	scale_y_continuous(name="Percent Max Growth Rate", limits=c(0, 1.0)) +
	theme(legend.position=c(0.85,0.25), legend.text=element_text(size=14), legend.title=element_text(size=16)) + labs(fill="Species")
dev.off()



##################################################################################
# Temperature; BootStrapped
##################################################################################
n=250
seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
seasons

# x.temp <- seq(260, 305, length=n)
# summary(model.data)

x.temp <- data.frame(array(dim=c(n,length(seasons))))
names(x.temp) <- seasons
for(i in seasons){
	x.temp[,i] <- seq(min(model.data[,paste("Tavg", i, sep=".")])-5, max(model.data[,paste("Tavg", i, sep=".")])+5, length=n)
}
#x.temp2 <- x.temp2[,3:length(x.temp2)]
summary(x.temp)

summary(param.est)
unique(param.est$Parameter)
##########################
# Some Initial Data Frames
##########################
# Vector needs to combine which season and x values
x.names <- vector(length=length(seasons)*nrow(x.temp))
for(i in 1:length(seasons)){
	start <- i*length(x.temp[,i])-length(x.temp[,i])+1
	x.names[start:(start+length(x.temp[,i])-1)] <- paste(i, x.temp[,i], sep=".")	
}

# Data frame where each run will be placed
temp.temp <- data.frame(array(dim=c(nrow(x.temp),1)))
# row.names(temp.temp) <- x.temp

# # Data frame where the summary (mean & SD) of the runs will be placed
temp.response <- data.frame(array(dim=c(length(x.temp),1)))
# names(temp.response) <- "x.temp"
# temp.response[,1] <- x.temp
# temp.response <- data.frame(array(dim=c(length(x.names),1)))
# row.names(temp.response) <- x.names
# names(temp.response) <- "x.temp"
# temp.response[,1] <- x.temp
# temp.response$Season <- as.factor(substr(row.names(temp.response),1,1))
# summary(temp.response)

temp.response <- data.frame()
for(s in unique(param.distrib$Species)){
	temp.response <- temp.seasonal(s, x.temp, temp.response, param.est, param.distrib, n)
		}

summary(temp.response)
head(temp.response)
names(temp.response)

temp.stack <- stack(temp.response[,substr(names(temp.response),6,9)=="temp"])
names(temp.stack) <- c("x.temp", "Species")
temp.stack$Species <- as.factor(substr(temp.stack$Species, 1, 4))
temp.stack$season.code <- as.factor(substr(x.names,1,1))
temp.stack$Year <- as.factor(ifelse(temp.stack$season.code==1 | temp.stack$season.code==2,"-1","1"))
levels(temp.stack$Year) <- c("Previous", "Current")
temp.stack$Season <- recode(temp.stack$season.code, "'1'='3'; '2'='4'; '3'='1'; '4'='2'; '5'='3'; '6'='4'")
levels(temp.stack$Season) <- c("Winter", "Spring", "Summer", "Fall")
summary(temp.stack) 

temp.stack1 <- stack(temp.response[,substr(names(temp.response),6,8)=="MLE"])
summary(temp.stack1)

temp.stack2 <- stack(temp.response[,substr(names(temp.response),6,11)=="CI.low"])
summary(temp.stack2)

temp.stack3 <- stack(temp.response[,substr(names(temp.response),6,12)=="CI.high"])
summary(temp.stack3)

temp.stack4 <- stack(temp.response[,substr(names(temp.response),6,12)=="Min"])
summary(temp.stack4)

temp.stack5 <- stack(temp.response[,substr(names(temp.response),6,8)=="Max"])
summary(temp.stack5)

temp.stack$MLE <- temp.stack1[,1]
temp.stack$CI.lo <- temp.stack2[,1]
temp.stack$CI.hi <- temp.stack3[,1]
temp.stack$Min <- temp.stack4[,1]
temp.stack$Max <- temp.stack5[,1]
summary(temp.stack)



# adding seasonal max/min to the data set (to do vertical lines for what's observed)
for(i in 1:length(seasons)){
	temp.stack[temp.stack$season.code==i,"temp.min"] <- min(model.data[,temp.col.ind[i]])
	temp.stack[temp.stack$season.code==i,"temp.max"] <- max(model.data[,temp.col.ind[i]])
}
summary(temp.stack)
temp.stack$Ribbon.min <- ifelse(temp.stack$x.temp < temp.stack$temp.min, temp.stack$x.temp, temp.stack$temp.min)
temp.stack$Ribbon.max <- ifelse(temp.stack$x.temp > temp.stack$temp.max, temp.stack$x.temp, temp.stack$temp.max)

# # Some summary stats
# 1-min(temp.stack[temp.stack$Species=="BELE" & temp.stack$Season=="Summer" & temp.stack$Year=="Prior" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/max(temp.stack[temp.stack$Species=="BELE" & temp.stack$Season=="Summer" & temp.stack$Year=="Prior" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

# max(temp.stack[temp.stack$Species=="QURU" & temp.stack$Season=="Summer" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/min(temp.stack[temp.stack$Species=="QURU" & temp.stack$Season=="Summer" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

# max(temp.stack[temp.stack$Species=="NYSY" & temp.stack$Season=="Spring" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/min(temp.stack[temp.stack$Species=="NYSY" & temp.stack$Season=="Spring" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

# 1-min(temp.stack[temp.stack$Species=="ACRU" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/max(temp.stack[temp.stack$Species=="ACRU" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

# max(temp.stack[temp.stack$Species=="QUPR" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])/min(temp.stack[temp.stack$Species=="QUPR" & temp.stack$Season=="Winter" & temp.stack$Year=="Current" & temp.stack$x.temp>=temp.stack$temp.min & temp.stack$x.temp<=temp.stack$temp.max, "MLE"])

species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Figures/Species Comparisons Full Model Seasonal - Temperature.pdf", width=10, height=7.5)
ggplot(data=temp.stack) + large.axes + facet_grid(Year~Season, scale="free") +
	geom_ribbon(aes(x=x.temp-273.15, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp-273.15, y=MLE*100, color=Species, linetype=Species), size=1.25) +
	geom_vline(aes(xintercept=temp.min-273.15), linetype="dashed", color="black") + 
	geom_vline(aes(xintercept=temp.max-273.15), linetype="dashed", color="black") + 
	geom_ribbon(aes(x=Ribbon.min-273.15, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	geom_ribbon(aes(x=Ribbon.max-273.15, ymin=0, ymax=100),fill="gray50", alpha=0.5) +

	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name=expression(bold(paste("Mean Season Temperature ("^"o","C)")))) + 
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,105), breaks=seq(0, 100, 25), expand=c(0,0)) +
	theme(legend.position=c(0.12,0.8), legend.text=element_text(size=14), legend.title=element_text(size=rel(1.5))) + labs(fill="Species") + theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()




##################################################################################
# Precipitation; BootStrapped
##################################################################################
n=250
seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
seasons

# x.precip <- seq(260, 305, length=n)
# summary(model.data)

x.precip <- data.frame(array(dim=c(n,length(seasons))))
names(x.precip) <- seasons
for(i in seasons){
	x.precip[,i] <- seq(min(model.data[,paste("Precip", i, sep=".")])-50, max(model.data[,paste("Precip", i, sep=".")])+50, length=n)
}
#x.precip2 <- x.precip2[,3:length(x.precip2)]
summary(x.precip)

summary(param.est)
unique(param.est$Parameter)
##########################
# Some Initial Data Frames
##########################
# Vector needs to combine which season and x values
x.names <- vector(length=length(seasons)*nrow(x.precip))
for(i in 1:length(seasons)){
	start <- i*length(x.precip[,i])-length(x.precip[,i])+1
	x.names[start:(start+length(x.precip[,i])-1)] <- paste(i, x.precip[,i], sep=".")	
}

# Data frame where each run will be placed
precip.temp <- data.frame(array(dim=c(nrow(x.precip),1)))
# row.names(precip.precip) <- x.precip

# Data frame where the summary (mean & SD) of the runs will be placed
# precip.response <- data.frame(array(dim=c(length(x.precip),1)))
# names(precip.response) <- "x.precip"
# precip.response[,1] <- x.precip
# precip.response <- data.frame(array(dim=c(length(x.names),1)))
# row.names(precip.response) <- x.names
# names(precip.response) <- "x.precip"
# precip.response[,1] <- x.precip
# precip.response$Season <- as.factor(substr(row.names(precip.response),1,1))
# summary(precip.response)

precip.response <- data.frame()
for(s in unique(param.distrib$Species)){
	FLOW <- mean(model.data[model.data$Spp==s, "flow"], na.rm=T)
	precip.response <- precip.seasonal(s, x.precip, FLOW, precip.response, param.est, param.distrib, n)
	}


summary(precip.response)
head(precip.response)
names(precip.response)

precip.stack <- stack(precip.response[,substr(names(precip.response),6,11)=="precip"])
names(precip.stack) <- c("x.precip", "Species")
precip.stack$Species <- as.factor(substr(precip.stack$Species, 1, 4))
# precip.stack$x.precip <- precip.response$x.precip
precip.stack$season.code <- as.factor(substr(x.names,1,1))
precip.stack$Year <- as.factor(ifelse(precip.stack$season.code==1 | precip.stack$season.code==2,"-1","1"))
levels(precip.stack$Year) <- c("Previous", "Current")
precip.stack$Season <- recode(precip.stack$season.code, "'1'='3'; '2'='4'; '3'='1'; '4'='2'; '5'='3'; '6'='4'")
levels(precip.stack$Season) <- c("Winter", "Spring", "Summer", "Fall")
summary(precip.stack) 


precip.stack1 <- stack(precip.response[,substr(names(precip.response),6,8)=="MLE"])
summary(precip.stack1)

precip.stack2 <- stack(precip.response[,substr(names(precip.response),6,11)=="CI.low"])
summary(precip.stack2)

precip.stack3 <- stack(precip.response[,substr(names(precip.response),6,12)=="CI.high"])
summary(precip.stack3)

precip.stack4 <- stack(precip.response[,substr(names(precip.response),6,12)=="Min"])
summary(precip.stack4)

precip.stack5 <- stack(precip.response[,substr(names(precip.response),6,8)=="Max"])
summary(precip.stack5)

precip.stack$MLE <- precip.stack1[,1]
precip.stack$CI.lo <- precip.stack2[,1]
precip.stack$CI.hi <- precip.stack3[,1]
precip.stack$Min <- precip.stack4[,1]
precip.stack$Max <- precip.stack5[,1]
summary(precip.stack)

for(i in 1:length(seasons)){
	precip.stack[precip.stack$season.code==i,"precip.min"] <- min(model.data[,precip.col.ind[i]])
	precip.stack[precip.stack$season.code==i,"precip.max"] <- max(model.data[,precip.col.ind[i]])
}
precip.stack$Ribbon.min <- ifelse(precip.stack$x.precip < precip.stack$precip.min, precip.stack$x.precip, precip.stack$precip.min)
precip.stack$Ribbon.max <- ifelse(precip.stack$x.precip > precip.stack$precip.max, precip.stack$x.precip, precip.stack$precip.max)
summary(precip.stack)

# # # Quantifying changes in growth
# 1-min(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

# 1-min(precip.stack[precip.stack$Species=="BELE" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="BELE" & precip.stack$Season=="Summer" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

# max(precip.stack[precip.stack$Species=="QURU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/min(precip.stack[precip.stack$Species=="QURU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

# 1-min(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="NYSY" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

# 1-min(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Summer" & precip.stack$Year=="Prior" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

# 1-min(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="ACRU" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])

# 1-min(precip.stack[precip.stack$Species=="QUPR" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])/max(precip.stack[precip.stack$Species=="QUPR" & precip.stack$Season=="Winter" & precip.stack$Year=="Current" & precip.stack$x.precip>=precip.stack$precip.min & precip.stack$x.precip<=precip.stack$precip.max, "MLE"])


species.colors <- c("purple", "blue", "green3", "orange", "red")

pdf("Figures/Species Comparisons Full Model Seasonal - Precipitation.pdf", width=10, height=7.5)
ggplot(data=precip.stack) + large.axes + facet_grid(Year~Season, scales="free") +
	geom_ribbon(aes(x=x.precip, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE*100, color=Species, linetype=Species), size=1.) +
	geom_vline(aes(xintercept=precip.min), linetype="dashed", color="black") + 
	geom_vline(aes(xintercept=precip.max), linetype="dashed", color="black") + 
	geom_ribbon(aes(x=Ribbon.min, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	geom_ribbon(aes(x=Ribbon.max, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_x_continuous(name="Total Season Precipitation (mm)", breaks=c(0, 50, 100, 150, 200)) + 
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,105), breaks=seq(0, 100, 25), expand=c(0,0)) +
	theme(legend.position=c(0.12,0.8), legend.text=element_text(size=14), legend.title=element_text(size=rel(1.5))) + labs(fill="Species") + theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()

save(precip.stack, temp.stack, aut.stack, comp.stack, file="Inputs/Scalars_Seasonal_Bootstrapped.Rdata")