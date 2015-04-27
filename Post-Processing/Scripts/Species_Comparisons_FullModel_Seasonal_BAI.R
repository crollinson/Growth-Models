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

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.5), plot.margin=unit(c(2,2,2,2), "lines"))

####################################################################
model.data <- read.csv("Inputs/TargetSpecies_AllSites_1990-2011.csv")
# model.data[,substr(names(model.data),1,4)=="Tavg"] <- model.data[,substr(names(model.data),1,4)=="Tavg"]+273.15
# model.data <- model.data[complete.cases(model.data) & model.data$Year>=1990 & model.data$Year<=2011,]
# model.data$BA.tree.cm2 <- model.data$BA.tree/100
summary(model.data)
length(unique(model.data$TreeID))


#############
# Vector with names of Months of the year
months <- c("X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09", "X10", "X11", "X12")

# Selecting which previous and current year months to include in model
months.prev <- paste(months[6:12], "prev", sep=".") # Previous June through December
months.curr <- paste(months[1:10], sep=".") # Current Junuary through October
months.use <- c(months.prev, months.curr)

seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
seasons

model.data$Tavg.pX06.pX08 <- rowMeans(model.data[,c("Tavg.X06.prev", "Tavg.X07.prev", "Tavg.X08.prev")])
model.data$Tavg.pX09.pX11 <- rowMeans(model.data[,c("Tavg.X09.prev", "Tavg.X10.prev", "Tavg.X11.prev")])
model.data$Tavg.pX12.X02 <- rowMeans(model.data[,c("Tavg.X12.prev", "Tavg.X01", "Tavg.X02")])
model.data$Tavg.X03.X05 <- rowMeans(model.data[,c("Tavg.X03", "Tavg.X04", "Tavg.X05")])
model.data$Tavg.X06.X08 <- rowMeans(model.data[,c("Tavg.X06", "Tavg.X07", "Tavg.X08")])
model.data$Tavg.X09.X11 <- rowMeans(model.data[,c("Tavg.X09", "Tavg.X10", "Tavg.X11")])

model.data$Precip.pX06.pX08 <- rowMeans(model.data[,c("Precip.X06.prev", "Precip.X07.prev", "Precip.X08.prev")])
model.data$Precip.pX09.pX11 <- rowMeans(model.data[,c("Precip.X09.prev", "Precip.X10.prev", "Precip.X11.prev")])
model.data$Precip.pX12.X02 <- rowMeans(model.data[,c("Precip.X12.prev", "Precip.X01", "Precip.X02")])
model.data$Precip.X03.X05 <- rowMeans(model.data[,c("Precip.X03", "Precip.X04", "Precip.X05")])
model.data$Precip.X06.X08 <- rowMeans(model.data[,c("Precip.X06", "Precip.X07", "Precip.X08")])
model.data$Precip.X09.X11 <- rowMeans(model.data[,c("Precip.X09", "Precip.X10", "Precip.X11")])


# Making vectors with the column names of the temp & precip months of interest
temp.col <- paste("Tavg", seasons, sep=".")
precip.col <- paste("Precip", seasons, sep=".")

# Column numbers of temp & precip months of interest
temp.col.ind <- which(names(model.data) %in% c(temp.col))
precip.col.ind <- which(names(model.data) %in% c(precip.col))
summary(model.data[ ,temp.col.ind])
summary(model.data[ ,precip.col.ind])
length(temp.col.ind)

min(model.data[ ,temp.col.ind])-273.15; max(model.data[ ,temp.col.ind])-273.15
min(model.data[ ,precip.col.ind]); max(model.data[ ,precip.col.ind])

####################################################################



# ####################################################################
# # Making a csv with parameter estimates
# ####################################################################
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
# Vary Summer Temp & Precip
##################################################################################
# x = mean annual temp, based on range of summer temperature (e.g. mean winter, spring, previous summer, previous & current fall, over range of current summer)
# mean precip
# max precip
# min Precip


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

