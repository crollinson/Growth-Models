####################################################
# ACRU MODEL Autogenic x Competition x (Temp x Precip) :
# Autogenic: Size/Age logistic scalar with Max potential growth for absolute values
# Competition: Relative Size, Plot BA Scalars
# Climate: 3-month means, prev Jun - current Nov
#	-- Temp: Gaussian
#	-- Precip: with flow Gaussian
#
####################################################
rm(list=ls())

data_directory <- "~/CARCA/Growth-Models/Seasonal/"
#memory.size(4024)
setwd(data_directory)
library(likelihood)

# run.label <- "ACRU_Full_Season"
# load(file=paste( run.label, "_Results.Rdata", sep=""))

run.label <- "ACRU_Full_Season"

#############
# # Reading in Test Data & some formating
# all.data <-read.csv("../RawInputs/CARCA_CoreData_Climate_Month_Wide.csv")
# all.data[,substr(names(all.data),1,4)=="Tavg"] <- all.data[,substr(names(all.data),1,4)=="Tavg"]+273.15
# all.data$BA.tree.cm2 <- all.data$BA.tree/100
# all.data$Site.Trans <- as.factor(substr(all.data$PlotID,1,4))
# summary(all.data)
# dim(all.data)

# #############
# # Subsetting just ACRU data
# acru.all <- all.data[all.data$Spp=="ACRU", ]
# summary(acru.all)
# dim(acru.all)

# # Subsetting only complete cases & a small range of years
# acru.run <- acru.all[complete.cases(acru.all) & acru.all$Year>=1990 & acru.all$Year<=2011,]
# summary(acru.run)
# dim(acru.run)

# #############
# # Vector with names of Months of the year
# months <- c("X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09", "X10", "X11", "X12")

# # Selecting which previous and current year months to include in model
# months.prev <- paste(months[6:12], "prev", sep=".") # Previous June through December
# months.curr <- paste(months[1:10], sep=".") # Current Junuary through October
# months.use <- c(months.prev, months.curr)

seasons <- c("pX06.pX08", "pX09.pX11", "pX12.X02", "X03.X05", "X06.X08", "X09.X11")
seasons

# acru.run$Tavg.pX06.pX08 <- rowMeans(acru.run[,c("Tavg.X06.prev", "Tavg.X07.prev", "Tavg.X08.prev")])
# acru.run$Tavg.pX09.pX11 <- rowMeans(acru.run[,c("Tavg.X09.prev", "Tavg.X10.prev", "Tavg.X11.prev")])
# acru.run$Tavg.pX12.X02 <- rowMeans(acru.run[,c("Tavg.X12.prev", "Tavg.X01", "Tavg.X02")])
# acru.run$Tavg.X03.X05 <- rowMeans(acru.run[,c("Tavg.X03", "Tavg.X04", "Tavg.X05")])
# acru.run$Tavg.X06.X08 <- rowMeans(acru.run[,c("Tavg.X06", "Tavg.X07", "Tavg.X08")])
# acru.run$Tavg.X09.X11 <- rowMeans(acru.run[,c("Tavg.X09", "Tavg.X10", "Tavg.X11")])

# acru.run$Precip.pX06.pX08 <- rowMeans(acru.run[,c("Precip.X06.prev", "Precip.X07.prev", "Precip.X08.prev")])
# acru.run$Precip.pX09.pX11 <- rowMeans(acru.run[,c("Precip.X09.prev", "Precip.X10.prev", "Precip.X11.prev")])
# acru.run$Precip.pX12.X02 <- rowMeans(acru.run[,c("Precip.X12.prev", "Precip.X01", "Precip.X02")])
# acru.run$Precip.X03.X05 <- rowMeans(acru.run[,c("Precip.X03", "Precip.X04", "Precip.X05")])
# acru.run$Precip.X06.X08 <- rowMeans(acru.run[,c("Precip.X06", "Precip.X07", "Precip.X08")])
# acru.run$Precip.X09.X11 <- rowMeans(acru.run[,c("Precip.X09", "Precip.X10", "Precip.X11")])


# Making vectors with the column names of the temp & precip months of interest
temp.col <- paste("Tavg", seasons, sep=".")
precip.col <- paste("Precip", seasons, sep=".")

# Column numbers of temp & precip months of interest
temp.col.ind <- which(names(acru.run) %in% c(temp.col))
precip.col.ind <- which(names(acru.run) %in% c(precip.col))
summary(acru.run[,temp.col.ind])
summary(acru.run[,precip.col.ind])
length(temp.col.ind)

# acru.run$Tavg.yr <- rowMeans(acru.run[,temp.col.ind])
# acru.run$Precip.yr <- rowSums(acru.run[,precip.col.ind])

# write.csv(acru.run, "Inputs/ACRU_AllSites_1990-2011.csv", row.names=F)

#############
# Reading in Data
acru.run <- read.csv("Inputs/ACRU_AllSites_1990-2011.csv")
summary(acru.run)

# Loading previous runs
#load(file="Test results for ACRU 4 - All Sites 1990-2011.Rdata")


####################################################################################################################
overall.model <- function(
							   SIZE,aa,ab, gmax,             # Autogenic (a) -- ABSOLUTE UNITS (from gmax)
                            BA.PLOT,RS,ca,cb,cc,cd,ce,cf,cg, # Competition (c) -- scalar
                            # # BA.PLOT,RS,ca,cb,cc,cd,cf,cg, # Competition (c) -- scalar, removed intercept
                            TEMP,ta1,tb1,					 # Temperature (t) -- scalar
                            PRECIP,FLOW,pa1,pb1,pc1         		 # Preciptiation (p) -- scalar
                            # TRANS, ha		             	 # Habitat (h) -- scalar
                            )
	{
	################
	# Autogenic Growth: 0-1 * gmax (max possible growth, absolute units)
	################
	autogenic.effect <- gmax*(1 - aa*exp(-ab*SIZE))   
	# autogenic.effect <- gmax*(1-aa*exp(-ab*SIZE))   


	################
	# Competition Effect: 0-1 
	################
	size.effect <- ca*exp(-cb*(RS^cc))
	comp.effect <- (ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg)))
	# comp.effect <- (1/(1+(BA.PLOT/cf)^cg))
	comp.response <- exp(-size.effect*(comp.effect^cd))

 	################
	# Climate Effect: Now Scalar
	################
	
	#Temp on scale 0-1
    temp.effect <- t(exp(-0.5*((t(TEMP)-ta1)/tb1)^2))

    # Precip on scale 0-1
    # precip.effect <- t(1/(1+(t(PRECIP)/pa1)^pb1))
	precip.effect <- t(exp(-0.5*((t(PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2)) # Gaussian Data Frame

	# Multiplying temp & precip together
    climate.effect <- temp.effect * precip.effect
	 
	 ### LEM: do row sums
	 ##### CRR note: I think this should be on a scale from 0-1; 
     # climate.effect <- colSums(climate.effect) 
	 climate.effect2 <- apply(climate.effect, 1, FUN=prod)
	

	################
	# Habitat Effect: 0-1
	# Note: removed from model for the moment, may add Topographic position or upslope area later
	################
	# habitat.effect <- ha[TRANS]

	# comp.response <- 1
	habitat.effect <- 1
	# climate.effect2 <- precip.effect

	autogenic.effect*comp.response*habitat.effect*climate.effect2
	

	}


####################################################################################################################
####################################################################################################################
### setting up annealing

# par <- results$best_par

# need a list that gives initial values for all of the "parameters"
par<-list(
		  aa=1, ab=0.00554, gmax=10000,  	# Autogenic
          ca=2000, cb=14.6, cc=4.98, cd=0.0122, ce=0, cf=1000, cg=-165.27,  # Competition
          ta1=c(278.38,	264.81,	284.25,	314.92,	250.36,	250), 
          tb1=c(266.04,	29.30,	489.61,	451.51,	449.48,	21.43),
          pa1=c(27.23,	440.96,	492.98,	215.03,	19.55,	0.057), 
          pb1=c(224.39,	1956.4,	638.42,	302.86,	725.79,	115.37), 
          pc1=0.409, 
          # # ha=rep(0.5,3),  # Habitat
          sd=126.61)

# also need a list that identifies the independent variables
var <- list(SIZE = "BA.tree.cm2",  # Autogenic
            BA.PLOT="BA.m2ha.plot.live", RS="RelBA",  # Competition
            TEMP = acru.run[,temp.col.ind], # hard-coded above
            PRECIP = acru.run[,precip.col.ind], FLOW="flow"
            # TRANS="Site.Trans" # Habitat
            )

## Set bounds and initial search ranges within which to search for parameters
##   NOTE:  the s and sd parameters have to be greater than zero (algebraically)
par_lo <-list(
			  aa=0, ab=0, gmax=0,  # Autogenic
              ca=0, cb=0, cc=0, cd=0, ce=0, cf=-100, cg=-1000, # Competition
              # # ca=0, cb=0, cc=0, cd=0, cf=-100, cg=-200, # Competition
              ta1=rep(250, length=length(temp.col.ind)), tb1=rep(0, length=length(temp.col.ind)),
              pa1=rep(0, length=length(precip.col.ind)), pb1=rep(0, length=length(precip.col.ind)), pc1=0,
			# ha=rep(0,3), # Habitat
              sd=0.1)


par_hi <-list(
			  aa=1, ab=1000, gmax=10000, # Autogenic
              ca=2000, cb=1000, cc=5, cd=5, ce=1000, cf=1000, cg=1000,	# Competition
              # # ca=1000, cb=1000, cc=50, cd=5, cf=500, cg=1000,	# Competition
              ta1=rep(325, length=length(temp.col.ind)), tb1=rep(500, length=length(temp.col.ind)),
              pa1=rep(500, length=length(precip.col.ind)), pb1=rep(2000, length=length(precip.col.ind)), pc1=1,
              # ha=rep(1,3), # Habitat
              sd=1000)

## Specify the dependent variable, and name it using whatever
##    argument name is used in the pdf ("x" in the normal.pdf function above)
var$x<-"BAI"

## predicted value in your PDF should be given the reserved name "predicted"
##   Anneal will use your scientific model and your data and calculate the
##   predicted value for each observation and store it internally in an object
##   called "predicted"
var$mean<-"predicted"

## Have it calculate log likelihood
var$log<-TRUE

##  now call the annealing algorithm, choosing which model to use
#  "data" should be whatever the name of your dataframe is...
results <- anneal(overall.model, par, var, acru.run, par_lo, par_hi, dnorm, "BAI", hessian = T, slimit=1.92, max_iter=100000)

save(results,file=file.path("Outputs", paste( run.label, "_Results.Rdata", sep="")))

write_results(results,file.path("Outputs", paste( run.label, "_Results.txt", sep="")))

## display some of the results in the console
results$best_pars;
results$max_likeli;
results$aic_corr ;
results$slope;
results$R2

####################################################################################################################
####################################################################################################################
load(file=file.path("Outputs", paste( run.label, "_Results.Rdata", sep="")))


# Looking at Residuals and Parameters
#windows()
plot(results$source_data$predicted,results$source_data$BAI,xlab="Predicted",ylab="Observed")
abline(0,1,lwd=2,col="red")

results$source_data$residual <- results$source_data$BAI - results$source_data$predicted
#windows()
hist(results$source_data$residual)


##################################
autogenic.effect <- function(SIZE,aa,ab) { (1 - aa*exp(-ab*SIZE))  }
##################################
plot(BAI ~ BA.tree.cm2, data=results$source_data)
plot(BAI ~ Age, data=results$source_data)
plot(BA.tree.cm2 ~ Age, data=results$source_data)

summary(results$source_data$BA.tree.cm2)
summary(results$source_data$Age)

# Effect of Tree Basal Area
#windows()
par(mar=c(5,5,4,2), mfrow=c(1,1))
x <- seq(0,max(ceiling(results$source_data$BA.tree.cm2)),1)
y <- autogenic.effect(x,results$best_pars$aa,results$best_pars$ab)

pdf(file.path("Figures", paste(run.label, " - Autogenic Scalar.pdf", sep="")))
plot(x,y,ylim=c(0,1),xlab="Tree Basal Area (cm2)",ylab="Effect of Size on Growth",
     cex.axis=1.25,cex.lab=1.5,type="l",lwd=2,main=run.label)
dev.off()


#################################
size.effect <- function(SIZE,ca,cb,cc) {ca*exp(-cb*(SIZE^cc))}
comp.effect <- function(BA.PLOT,ce,cf,cg) {ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg))}
comp.response <- function(SIZE,BA.PLOT,ca,cb,cc,cd,ce,cf,cg)
  { size.effect <- ca*exp(-cb*(SIZE^cc))
    comp.effect <- (ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg)))
    comp.response <- exp(-size.effect*(comp.effect^cd)) }
##################################
# Size Effect
plot(BAI ~ RelBA, data=results$source_data)

par(mar=c(5,5,4,2))
x <- seq(0,1,0.01)
y <- size.effect(x,results$best_pars$ca, results$best_pars$cb,results$best_pars$cc)
summary(y)

pdf(file.path("Figures", paste(run.label, " - Competition Size Effect.pdf", sep="")))
plot(x,y,xlab="Relative Size",ylab="Size Effect",type="l",lwd=2,
      cex.lab=1.5,cex.axis=1.25,main=run.label)
#savePlot(file=paste("Competition Plot 1 ",run.label,".png",sep=""),type="png")
dev.off()

#####################
# Plot BA effect
plot(BAI ~ BA.m2ha.plot, data=results$source_data)

par(mar=c(5,5,4,2))
x <- seq(0,60,0.1)
y <- comp.effect(x, results$best_pars$ce, results$best_pars$cf,results$best_pars$cg)
summary(y)

pdf(file.path("Figures", paste(run.label, " - Competition Competitive Effect.pdf", sep="")))
plot(x,y,ylim=c(0,1),xlab="Plot BA",ylab="Competitive Effect",type="l",lwd=2,
       cex.lab=1.5,cex.axis=1.25,main=run.label)
dev.off()
#savePlot(file=paste("Competition Plot 2 ",run.label,".png",sep=""),type="png")

#####################
# Competition Response: Size vs. Response with different plot BA levels
plot(BA.m2ha.plot~ RelBA, data=results$source_data)


pdf(file.path("Figures", paste(run.label, " - Competition Response Scalar.pdf", sep="")))
par(mar=c(5,5,4,2))
colors <- c("red","orange","blue","cyan","green")
x <- seq(0,1,0.01)
y <- comp.response(SIZE=x, BA.PLOT=50, results$best_pars$ca, results$best_pars$cb, results$best_pars$cc, results$best_pars$cd, results$best_pars$ce, results$best_pars$cf, results$best_pars$cg)
plot(x,y,ylim=c(0,1),xlab="Relative Size",ylab="Competition Scalar",type="l",col=colors[1],lwd=2,
     cex.lab=1.5,cex.axis=1.25,main=run.label)
y <- comp.response(SIZE=x, BA.PLOT=40, results$best_pars$ca, results$best_pars$cb, results$best_pars$cc, results$best_pars$cd, results$best_pars$ce, results$best_pars$cf, results$best_pars$cg)
lines(x,y,col=colors[2],lwd=2)
y <- comp.response(SIZE=x, BA.PLOT=30, results$best_pars$ca, results$best_pars$cb, results$best_pars$cc, results$best_pars$cd, results$best_pars$ce, results$best_pars$cf, results$best_pars$cg)
lines(x,y,col=colors[3],lwd=2)
y <- comp.response(SIZE=x, BA.PLOT=20, results$best_pars$ca, results$best_pars$cb, results$best_pars$cc, results$best_pars$cd, results$best_pars$ce, results$best_pars$cf, results$best_pars$cg)
lines(x,y,col=colors[4],lwd=2)
y <- comp.response(SIZE=x, BA.PLOT=10, results$best_pars$ca, results$best_pars$cb, results$best_pars$cc, results$best_pars$cd, results$best_pars$ce, results$best_pars$cf, results$best_pars$cg)
lines(x,y,col=colors[5],lwd=2)
lines(x,y,col=colors[6],lwd=2)
legend("bottomright",legend=c(50,40,30,20,10),col=colors,bty="n",lwd=2,title="Plot\nBasal\nArea",cex=1.25)
dev.off()
#savePlot(file=paste("Competition Plot 3 ",run.label,".png",sep=""),type="png")


##################################
temp.effect <- function(TEMP,ta1,tb1)
       { t(exp(-0.5*((t(TEMP)-ta1)/tb1)^2))  }

precip.effect <- function(PRECIP,FLOW,pa1,pb1,pc1)
       { exp(-0.5*((t(PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2) }

# precip.effect <- function(PRECIP,pa1,pb1)
       # { (1/(1+((PRECIP)/pa1)^pb1))  }

climate.effect <- function(TEMP,PRECIP,FLOW,ta1,tb1,pa1,pb1,pc1)
       { (exp(-0.5*(((TEMP)-ta1)/tb1)^2)) * (exp(-0.5*((t(PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2)) }
##################################
# Temperature
plot(acru.run$BAI ~ rowMeans(acru.run[,temp.col]), xlab="Mean Temp Mar-Oct", ylab="BAI (mm)")

x.temp <- array(dim=c(100,length(temp.col.ind)))
x.temp[1:100,] <- seq(250, 300, length.out=100)
dim(x.temp)

y.temp <- temp.effect(x.temp, results$best_pars$ta1, results$best_pars$tb1)
dim(y.temp)

pdf(file.path("Figures", paste(run.label, " - Climate Temperature Effect.pdf", sep="")), width=12, height=9)
par(mfrow=c(2,3))
for(i in 1:length(temp.col))
	{	plot(x.temp[,i],y.temp[,i],ylim=c(0,1), xlab="Tavg (K)",ylab="Temp Effect",type="l",lwd=2, main=temp.col[i])
		abline(v=min(acru.run[,temp.col[i]]), col="red")
		abline(v=max(acru.run[,temp.col[i]]), col="red")
	}
dev.off()

# # summary(rowMeans(acru.run[,temp.col]))
# x.temp <- seq(250, 300, length.out=100)
# y.temp <- temp.effect(x.temp, results$best_pars$ta1, results$best_pars$tb1)

# pdf( paste(run.label, " - Climate Temperature Effect.pdf", sep=""))
# par(mfrow=c(1,1))
# plot(x.temp, y.temp, type="l", lwd=2,ylim=c(0,1), xlab="Tavg (K)", ylab="Temp Effect")
		# abline(v=min(rowMeans(acru.run[,temp.col])), col="red")
		# abline(v=max(rowMeans(acru.run[,temp.col])), col="red")
# dev.off()

##########
# Precipitation
plot(acru.run$BAI ~ rowSums(acru.run[,precip.col]), xlab="Total Precip Mar-Oct", ylab="BAI (mm)")

x.precip <- array(dim=c(100,length(temp.col.ind)))
x.precip[1:100,] <- seq(0, 500, length.out=100)
flow.precip <- rep(mean(acru.run$flow, na.rm=T),100)
y.precip <- t(precip.effect(x.precip, flow.precip, results$best_pars$pa1, results$best_pars$pb1, results$best_pars$pc1))
dim(y.precip)
summary(y.precip)

pdf(file.path("Figures", paste(run.label, " - Climate Precipitation Effect.pdf", sep="")), width=12, height=9)
par(mfrow=c(2,3))
for(i in 1:length(temp.col))
	{	plot(x.precip[,i],y.precip[,i], ylim=c(0,1), xlab="Precip (mm)", ylab="BAI", type="l", lwd=2, main=precip.col[i])
		abline(v=min(acru.run[,precip.col[i]]), col="red")
		abline(v=max(acru.run[,precip.col[i]]), col="red")
	}
dev.off()

# # x.precip <- seq(0, 2500, length.out=100)
# y.precip <- precip.effect(x.precip, results$best_pars$pa1, results$best_pars$pb1)

# pdf( paste(run.label, " - Climate Precipitation Effect.pdf", sep=""))
# plot(x.precip, y.precip, type="l", lwd=2, xlab="Precip (mm)", ylab="precip Effect")
		# abline(v=min(rowSums(acru.run[,precip.col])), col="red")
		# abline(v=max(rowSums(acru.run[,precip.col])), col="red")
# summary(rowSums(acru.run[,precip.col]))
# summary(y.precip)
# dev.off()

##########
##########

# # Data frame with mean temperature for each month in the column
# x.temp <- array(dim=c(100,length(temp.col.ind)))
# x.temp[1:100,] <- seq(250, 300, length.out=100)
# dim(x.temp)
# summary(x.temp)


# # Data frame with mean precip for each month in the column
# x.precip.mean <- data.frame(array(dim=c(100,length(precip.col.ind))))
# names(x.precip.mean) <- precip.col
# for(i in unique(precip.col))
	# { x.precip.mean[,i] <- mean(acru.run[,i]) }
# summary(x.precip.mean)

# x.precip.mean2 <- data.frame(array(dim=c(100,length(precip.col.ind))))
# names(x.precip.mean2) <- precip.col
# for(i in unique(precip.col))
	# { x.precip.mean2[,i] <- mean(acru.run[,i])-50 }
# summary(x.precip.mean2)

# x.precip.mean3 <- data.frame(array(dim=c(100,length(precip.col.ind))))
# names(x.precip.mean3) <- precip.col
# for(i in unique(precip.col))
	# { x.precip.mean3[,i] <- mean(acru.run[,i])+50 }
# summary(x.precip.mean3)

# climate.effect1 <- climate.effect(TEMP=x.temp, PRECIP=500, results$best_par$ta1, results$best_par$tb1, results$best_par$pa1, results$best_par$pb1)
# summary(climate.effect1)

# climate.effect2 <- climate.effect(TEMP=x.temp, PRECIP=1000, results$best_par$ta1, results$best_par$tb1, results$best_par$pa1, results$best_par$pb1)
# summary(climate.effect2)

# climate.effect3 <- climate.effect(TEMP=x.temp, PRECIP=1500, results$best_par$ta1, results$best_par$tb1, results$best_par$pa1, results$best_par$pb1)
# summary(climate.effect3)

# climate.effect4 <- climate.effect(TEMP=x.temp, PRECIP=2000, results$best_par$ta1, results$best_par$tb1, results$best_par$pa1, results$best_par$pb1)
# summary(climate.effect3)

# pdf( paste(run.label, " - Climate Response Temperature.pdf", sep=""))
# par(mar=c(5,5,4,1))
	# plot(x.temp,climate.effect1,ylim=c(0,1), xlab="Tavg (K)",ylab="BAI",type="l",lwd=3, col="red", cex.axis=1.25, cex.lab=1.5, cex.main=2, font.lab=2, main="Climate Effect")

	# lines(x.temp, climate.effect2, type="l", lwd=3, col="orange")
	# lines(x.temp, climate.effect3, type="l", lwd=3, col="green3")
	# lines(x.temp, climate.effect4, type="l", lwd=3, col="blue")

	# abline(v=min(acru.run[,temp.col]), col="black", lty="dashed")
	# abline(v=max(acru.run[,temp.col]), col="black", lty="dashed")			
	# legend("bottomleft", legend=c("500", "1000", "1500", "2000"), col=c("red", "orange", "green3", "blue"), lwd=3, bty="n", cex=1.25)
# dev.off()

