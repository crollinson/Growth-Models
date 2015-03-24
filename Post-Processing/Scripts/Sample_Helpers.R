	#---------------------------------------------
	ci <- apply(comp.temp, 1, FUN=quantile, c(0.025, 0.975),na.rm=T)
	range  <- apply(comp.temp, 1, FUN=range, na.rm=T)

	comp.response[,paste(s, "CI.low", sep=".")] <- ci[1,]
	comp.response[,paste(s, "CI.high", sep=".")] <- ci[2,]
	comp.response[,paste(s, "Min", sep=".")] <- range[1,]
	comp.response[,paste(s, "Max", sep=".")] <- range[2,]
	#---------------------------------------------

####################################################################
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

####################################################################

	aa.mle <- param.est[param.est$Species==s & param.est$Parameter=="aa", "MLE"]
	ab.mle <- param.est[param.est$Species==s & param.est$Parameter=="ab", "MLE"]

	aut.response[,paste(s, "x.auto", sep=".")] <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)
	x.temp <- seq(min(model.data[model.data$Spp==s, "BA.tree.cm2"]), max(model.data[model.data$Spp==s, "BA.tree.cm2"]), length=250)
	#---------------------------------------------
	# Varying the parameters independently because doing it at the same time causes impossible values
	#---------------------------------------------
	for(i in 1:100){
		aa.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="aa", 3:ncol(param.distrib)], size=1, replace=T))
		aut.temp[,i] <- autogenic.effect(x.temp, aa=aa.samp, ab=ab.mle)
		}
	for(i in 101:200){
		ab.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ab", 3:ncol(param.distrib)], size=1, replace=T))
		aut.temp[,i] <- autogenic.effect(x.temp, aa=aa.mle, ab=ab.samp)
		}
	#---------------------------------------------


####################################################################

	comp.response[,paste(s, "relba", sep=".")] <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=250)
	x.temp <- seq(min(model.data[model.data$Spp==s, "RelBA"]), max(model.data[model.data$Spp==s, "RelBA"]), length=250)

	for(p in unique(param.est[substr(param.est$Parameter,1,1)=="c","Parameter"])){	
		assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}

	gmax.MLE <- param.est[param.est$Species==s & param.est$Parameter=="gmax", "MLE"]

	#---------------------------------------------
	# Varying the parameters independently because doing it at the same time causes impossible values
	#---------------------------------------------
	for(i in 1:100){ #ca
		ca.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ca", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.samp, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 101:200){ #cb
		cb.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cb", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.MLE, cb=cb.samp, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 201:300){ #cc
		cc.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cc", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.MLE, cb=cb.MLE, cc=cc.samp,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 301:400){ #cd
		cd.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cd", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.samp, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 401:500){ #ce
		ce.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ce", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.samp, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 501:600){ #cf
		cf.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cf", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.samp, cg=cg.MLE)
		}
	for(i in 601:700){ #cg
		cg.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cg", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=x.temp, 
										BA.PLOT=plot.BA, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.samp)
		}
	#---------------------------------------------

####################################################################

	pa1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pa1", "MLE"]
	pb1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pb1", "MLE"]
	pc1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pc1", "MLE"]

	precip.response[,paste(s, "x.auto", sep=".")] <- seq(min(model.data$Precip.PRISM.sum), max(model.data$Precip.PRISM.sum), length=250)
	x.temp <- seq(min(model.data$Precip.PRISM.sum), max(model.data$Precip.PRISM.sum), length=250)


	for(i in 1:100){
		pa1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pa1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.samp, pb1=pb1.MLE, pc1.MLE)
		}
	for(i in 101:200){
		pb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pb1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.MLE, pb1=pb1.samp, pc1.MLE)
		}
	for(i in 201:300){
		pc1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pc1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.MLE, pb1=pb1.MLE, pc1.samp)
		}

	precip.response[,paste(s, "MLE", sep=".")] <- precip.effect(x.precip, x.flow, pa1=pa1.MLE, pb1=pb1.MLE, pc1=pc1.MLE)

	ci <- apply(precip.temp, 1, FUN=quantile, c(0.025, 0.975))
	range  <- apply(precip.temp, 1, FUN=range)

	precip.response[,paste(s, "CI.low", sep=".")] <- ci[1,]
	precip.response[,paste(s, "CI.high", sep=".")] <- ci[2,]
	precip.response[,paste(s, "Min", sep=".")] <- range[1,]
	precip.response[,paste(s, "Max", sep=".")] <- range[2,]
	}

####################################################################

	ta1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="ta1", "MLE"]
	tb1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="tb1", "MLE"]

	temp.response[,paste(s, "x.auto", sep=".")] <- seq(min(model.data$Tavg), max(model.data$Tavg), length= 250)+273.15
	x.temp <- seq(min(model.data$Tavg), max(model.data$Tavg), length=250)+273.15

	for(i in 1:100){
		ta1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ta1", 3:202], size=1, replace=T))
		temp.temp[,i] <- temp.effect(x.temp, ta1=ta1.samp, tb1=tb1.MLE)
		}
	for(i in 101:200){
		tb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="tb1", 3:202], size=1, replace=T))
		temp.temp[,i] <- temp.effect(x.temp, ta1=ta1.MLE, tb1=tb1.samp)
		}


####################################################################
