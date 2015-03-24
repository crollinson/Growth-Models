# ----------------------------------------------------
# Autogenic (Size) Predict
# ----------------------------------------------------
aut.predict <- function(species, SIZE, out, param.est, param.distrib){
	aut.temp <- data.frame(array(dim=c(length(SIZE),1)))
	row.names(aut.temp) <- SIZE

	aa.mle <- param.est[param.est$Species==species & param.est$Parameter=="aa", "MLE"]
	ab.mle <- param.est[param.est$Species==species & param.est$Parameter=="ab", "MLE"]

	out[,paste(species, "x.auto", sep=".")] <- SIZE	#---------------------------------------------
	# Varying the parameters independently because doing it at the same time causes impossible values
	#---------------------------------------------
	for(i in 1:100){
		aa.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="aa", 3:ncol(param.distrib)], size=1, replace=T))
		aut.temp[,i] <- autogenic.effect(SIZE, aa=aa.samp, ab=ab.mle)
		}
	for(i in 101:200){
		ab.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ab", 3:ncol(param.distrib)], size=1, replace=T))
		aut.temp[,i] <- autogenic.effect(SIZE, aa=aa.mle, ab=ab.samp)
		}
	#---------------------------------------------

	out[,paste(species, "MLE", sep=".")] <- autogenic.effect(
			SIZE=SIZE, 
			aa=aa.mle, 
			ab=ab.mle)
		
	ci <- apply(aut.temp, 1, FUN=quantile, c(0.025, 0.975), na.rm=T)
	range  <- apply(aut.temp, 1, FUN=range, na.rm=T)

	out[,paste(species, "CI.low", sep=".")] <- ci[1,]
	out[,paste(species, "CI.high", sep=".")] <- ci[2,]
	out[,paste(species, "Min", sep=".")] <- range[1,]
	out[,paste(species, "Max", sep=".")] <- range[2,]
	
	return(out)
	}
# ----------------------------------------------------

# ----------------------------------------------------
# Competition Predict
# ----------------------------------------------------
comp.predict <- function(species, RS, BA.PLOT, out, param.est, param.distrib){
	comp.temp <- data.frame(array(dim=c(length(x.relba),1)))
	row.names(comp.temp) <- x.relba

	out[,paste(s, "relba", sep=".")] <- RS

	for(p in unique(param.est[substr(param.est$Parameter,1,1)=="c","Parameter"])){	
		assign(paste(p, "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p, "MLE"])
		}

	gmax.MLE <- param.est[param.est$Species==s & param.est$Parameter=="gmax", "MLE"]

	#---------------------------------------------
	# Varying the parameters independently because doing it at the same time causes impossible values
	#---------------------------------------------
	for(i in 1:100){ #ca
		ca.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ca", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.samp, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 101:200){ #cb
		cb.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cb", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.MLE, cb=cb.samp, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 201:300){ #cc
		cc.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cc", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.MLE, cb=cb.MLE, cc=cc.samp,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 301:400){ #cd
		cd.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cd", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.samp, ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 401:500){ #ce
		ce.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ce", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.samp, cf=cf.MLE, cg=cg.MLE)
		}
	for(i in 501:600){ #cf
		cf.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cf", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.samp, cg=cg.MLE)
		}
	for(i in 601:700){ #cg
		cg.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="cg", 3:ncol(param.distrib)], size=1, replace=T))
		comp.temp[,i] <- comp.effect(RS=RS, 
										BA.PLOT=BA.PLOT, ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
										cd=cd.MLE, ce=ce.MLE, cf=cf.MLE, cg=cg.samp)
		}
	#---------------------------------------------

		
	out[,paste(s, "MLE", sep=".")] <-  comp.effect(
												RS=out[,paste(s, "relba", sep=".")], 
												BA.PLOT=BA.PLOT, 
												ca=ca.MLE, cb=cb.MLE, cc=cc.MLE,
												cd=cd.MLE,
												ce=ce.MLE, cf=cf.MLE, cg=cg.MLE)

	ci <- apply(comp.temp, 1, FUN=quantile, c(0.025, 0.975), na.rm=T)
	range  <- apply(comp.temp, 1, FUN=range, na.rm=T)

	out[,paste(s, "CI.low", sep=".")] <- ci[1,]
	out[,paste(s, "CI.high", sep=".")] <- ci[2,]
	out[,paste(s, "Min", sep=".")] <- range[1,]
	out[,paste(s, "Max", sep=".")] <- range[2,]
	
	return(out)
}
# ----------------------------------------------------

# ----------------------------------------------------
# Temperature Predict -- Annual
# ----------------------------------------------------
temp.ann <- function(species, TEMP, out, param.est, param.distrib){
	temp.temp <- data.frame(array(dim=c(length(TEMP),1)))
	row.names(temp.temp) <- TEMP

	ta1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="ta1", "MLE"]
	tb1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="tb1", "MLE"]

	out[,paste(s, "x.temp", sep=".")] <- TEMP

	for(i in 1:100){
		ta1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="ta1", 3:202], size=1, replace=T))
		temp.temp[,i] <- temp.effect(TEMP=TEMP, ta1=ta1.samp, tb1=tb1.MLE)
		}
	for(i in 101:200){
		tb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="tb1", 3:202], size=1, replace=T))
		temp.temp[,i] <- temp.effect(TEMP=TEMP, ta1=ta1.MLE, tb1=tb1.samp)
		}

	out[,paste(s, "MLE", sep=".")] <- temp.effect(TEMP=TEMP, ta1=ta1.MLE, tb1=tb1.MLE)

	ci <- apply(temp.temp, 1, FUN=quantile, c(0.025, 0.975), na.rm=T)
	range  <- apply(temp.temp, 1, FUN=range, na.rm=T)

	out[,paste(s, "CI.low", sep=".")] <- ci[1,]
	out[,paste(s, "CI.high", sep=".")] <- ci[2,]
	out[,paste(s, "Min", sep=".")] <- range[1,]
	out[,paste(s, "Max", sep=".")] <- range[2,]
	return(out)
}
# ----------------------------------------------------

# ----------------------------------------------------
# Temperature Predict -- Seasonal
# ----------------------------------------------------
temp.seasonal <- function(species, TEMP, out, param.est, param.distrib){
	for(j in 1:length(seasons)){
		start <- j*length(x.temp)-length(x.temp)+1

		p.list1 <- c(paste("ta1", j, sep=""), paste("tb1", j, sep=""))
		p.list2 <- c("ta1", "tb1")

		# Assigning ML Estimate
		for(p in 1:length(p.list1)){
			assign(paste(p.list2[p], "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p.list1[p], "MLE"])
			}

	#---------------------------------------------
	# Varying the parameters independently because doing it at the same time causes impossible values
	#---------------------------------------------
		for(i in 1:100){
			ta1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[1], 3:202], size=1, replace=T))
			temp.temp[,i] <- temp.effect(x.temp, ta1=ta1.samp, tb1=tb1.MLE)
			}
		for(i in 101:200){
			tb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[2], 3:202], size=1, replace=T))
			temp.temp[,i] <- temp.effect(x.temp, ta1=ta1.MLE, tb1=tb1.samp)
			}
	#---------------------------------------------

		# Note: for some reason as.numeric is necessary otherwise it gets put in a weird format
		out[start:(start+length(x.temp)-1),paste(s, "temp", sep=".")] <- x.temp
		
		# seq(min(model.data[model.data$Spp==s,temp.col.ind[j]]), max(model.data[model.data$Spp==s,temp.col.ind[j]]), length=250)

		out[start:(start+length(x.temp)-1),paste(s, "MLE", sep=".")] <- as.numeric(temp.effect(TEMP=x.temp, ta1=ta1.MLE, tb1=tb1.MLE))

		ci <- apply(temp.temp, 1, FUN=quantile, c(0.025, 0.975), na.rm=T)
		range  <- apply(temp.temp, 1, FUN=range, na.rm=T)

		out[start:(start+length(x.temp)-1),paste(s, "CI.low", sep=".")] <- ci[1,]
		out[start:(start+length(x.temp)-1),paste(s, "CI.high", sep=".")] <- ci[2,]
		out[start:(start+length(x.temp)-1),paste(s, "Min", sep=".")] <- range[1,]
		out[start:(start+length(x.temp)-1),paste(s, "Max", sep=".")] <- range[2,]
		}

	return(out)
}
# ----------------------------------------------------


# ----------------------------------------------------
# Precipitation Predict -- Annual
# ----------------------------------------------------
precip.ann <- function(species, PRECIP, FLOW, out, param.est, param.distrib){
	precip.temp <- data.frame(array(dim=c(length(x.auto),1)))
	row.names(precip.temp) <- PRECIP	

	pa1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pa1", "MLE"]
	pb1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pb1", "MLE"]
	pc1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pc1", "MLE"]

	out[,paste(s, "x.auto", sep=".")] <- PRECIP


	for(i in 1:100){
		pa1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pa1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(PRECIP, FLOW, pa1=pa1.samp, pb1=pb1.MLE, pc1.MLE)
		}
	for(i in 101:200){
		pb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pb1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(PRECIP, FLOW, pa1=pa1.MLE, pb1=pb1.samp, pc1.MLE)
		}
	for(i in 201:300){
		pc1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter=="pc1", 3:202], size=1, replace=T))
		precip.temp[,i] <- precip.effect(PRECIP, FLOW, pa1=pa1.MLE, pb1=pb1.MLE, pc1.samp)
		}

	out[,paste(s, "MLE", sep=".")] <- precip.effect(PRECIP, FLOW, pa1=pa1.MLE, pb1=pb1.MLE, pc1=pc1.MLE)

	ci <- apply(precip.temp, 1, FUN=quantile, c(0.025, 0.975), na.rm=T)
	range  <- apply(precip.temp, 1, FUN=range, na.rm=T)

	out[,paste(s, "CI.low", sep=".")] <- ci[1,]
	out[,paste(s, "CI.high", sep=".")] <- ci[2,]
	out[,paste(s, "Min", sep=".")] <- range[1,]
	out[,paste(s, "Max", sep=".")] <- range[2,]
	
	return(out)
}
# ----------------------------------------------------

# ----------------------------------------------------
# Precipitation Predict -- Seasonal
# ----------------------------------------------------
precip.seasonal <- function(species, PRECIP, FLOW, out, param.est, param.distrib){
	precip.temp <- data.frame(array(dim=c(length(x.auto),1)))
	row.names(precip.temp) <- PRECIP	

	for(j in 1:length(seasons)){
		start <- j*length(x.precip)-length(x.precip)+1

		p.list1 <- c(paste("pa1", j, sep=""), paste("pb1", j, sep=""), paste("pc1", j, sep=""))
		p.list2 <- c("pa1", "pb1", "pc1")
		pc1.MLE <- param.est[param.est$Species==s & param.est$Parameter=="pc1", "MLE"]
		# Assigning ML Estimate
		for(p in 1:length(p.list1[1:2])){
			assign(paste(p.list2[p], "MLE", sep="."), param.est[param.est$Species==s & param.est$Parameter==p.list1[p], "MLE"])
			}

	#---------------------------------------------
	# Varying the parameters independently because doing it at the same time causes impossible values
	#---------------------------------------------
		for(i in 1:100){
			pa1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[1], 3:202], size=1, replace=T))
			precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.samp, pb1=pb1.MLE, pc1=pc1.MLE)
			}
		for(i in 101:200){
			pb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[2], 3:202], size=1, replace=T))
			precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.MLE, pb1=pb1.samp, pc1=pc1.MLE)
			}
		for(i in 201:300){
			pc1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==s & param.distrib$Parameter==p.list1[3], 3:202], size=1, replace=T))
			precip.temp[,i] <- precip.effect(x.precip, x.flow, pa1=pa1.MLE, pb1=pb1.MLE, pc1=pc1.samp)
			}
	#---------------------------------------------

		# Note: for some reason as.numeric is necessary otherwise it gets put in a weird format
		out[start:(start+length(x.precip)-1),paste(s, "precip", sep=".")] <- x.precip
		
		# seq(min(model.data[model.data$Spp==s,precip.col.ind[j]]), max(model.data[model.data$Spp==s,precip.col.ind[j]]), length=250)

		out[start:(start+length(x.precip)-1),paste(s, "MLE", sep=".")] <- as.numeric(precip.effect(PRECIP=x.precip, FLOW=x.flow, pa1=pa1.MLE, pb1=pb1.MLE, pc1=pc1.MLE))

		ci <- apply(precip.temp, 1, FUN=quantile, c(0.025, 0.975), na.rm=T)
		range  <- apply(precip.temp, 1, FUN=range, na.rm=T)

		out[start:(start+length(x.temp)-1),paste(s, "CI.low", sep=".")] <- ci[1,]
		out[start:(start+length(x.temp)-1),paste(s, "CI.high", sep=".")] <- ci[2,]
		out[start:(start+length(x.temp)-1),paste(s, "Min", sep=".")] <- range[1,]
		out[start:(start+length(x.temp)-1),paste(s, "Max", sep=".")] <- range[2,]
		}

	
	return(out)
}
# ----------------------------------------------------


# ----------------------------------------------------
# Full Growth Model, BAI
# ----------------------------------------------------
full.model.annual.bai <- function(species, var, SIZE, RS, BA.PLOT, TEMP, PRECIP, FLOW, out, param.est, param.distrib){
	full.temp <- data.frame(array(dim=c(length(var),1)))
	row.names(full.temp) <- var

	for(p in unique(param.est$Parameter)){	
		assign(paste(p, "mle", sep="."), param.est[param.est$Species==species & param.est$Parameter==p, "MLE"])
		}
	

	# aa
	for(i in 1:100){ #aa
		aa.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="aa", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE=SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP=PRECIP,
									FLOW=FLOW,
									aa=aa.samp, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ab
	for(i in 101:200){ #ab
		ab.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ab", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP=PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.samp,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ca
	for(i in 201:300){ #ca
		ca.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ca", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.samp, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}		
	# cb
	for(i in 301:400){ #cb
		cb.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cb", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.samp, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cc
	for(i in 401:500){ #cc
		cc.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cc", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.samp, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cd
	for(i in 501:600){ #cd
		cd.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cd", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.samp, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ce
	for(i in 601:700){ #ce
		ce.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ce", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.samp, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cf
	for(i in 701:800){ #cf
		cf.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cd", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.samp, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cg
	for(i in 801:900){ #cg
		cg.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cg", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.samp, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.samp,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ta
	for(i in 901:1000){ #ta
		ta1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ta1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.samp, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# tb
	for(i in 1001:1100){ #tb
		tb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="tb1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.samp,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# pa
	for(i in 1101:1200){ #pa
		pa1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="pa1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.samp, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# pb
	for(i in 1201:1300){ #pb
		pb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="pb1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.samp, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# pc
	for(i in 1301:1400){ #pc
		pc1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="pc1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.samp,
									gmax=gmax.mle)
		}
	# gmax
	for(i in 1401:1500){ #gmax
		gmax.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="gmax", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.samp)
		}

		#############


	out[,paste(species, "MLE", sep=".")] <- full.model.annual(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP=PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
	
	#---------------------------------------------
	ci <- apply(full.temp, 1, FUN=quantile, c(0.025, 0.975),na.rm=T)
	range  <- apply(full.temp, 1, FUN=range, na.rm=T)

	out[,paste(species, "CI.low", sep=".")] <- ci[1,]
	out[,paste(species, "CI.high", sep=".")] <- ci[2,]
	out[,paste(species, "Min", sep=".")] <- range[1,]
	out[,paste(species, "Max", sep=".")] <- range[2,]
	
	return(out)
	#---------------------------------------------
	}
# ----------------------------------------------------
